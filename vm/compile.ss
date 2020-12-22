#! /usr/bin/env scheme-r5rs

;; Compile code into something that's readable by the bytecode
;; interpreter. This includes any quotations (literals) and things
;; defined as globals; the result looks like:
;;
;; (program
;;   (quotations (value...))
;;   (code       (opcode ...))
;; )

(define *quotations* '())

(define (compile-expr prog)
  (set! *quotations* '())
  (let ((code (meaning prog r.init #t)))
    `(program
      (quotations ,*quotations*)
      (code ,code))))

(define (compile-file file)
  (let* ((input (open-input-file file))
         (prog  (read input)))
    (close-input-port input)
    (compile-expr prog)))

(define (main args)
  (case (length args)
    ((1)
     (let ((expr (read)))
       (pretty-print (compile-expr expr))
       (newline)))
    ((2)
     (pretty-print (compile-file (cadr args)))
     (newline))))

;; --------- these are copypasta taken verbatim from
;;
;;     https://github.com/squaremo/lisp-in-small-pieces
;;
;; These all call procedures to be defined by the compiler -- i.e.,
;; they do a frontend pass of interpretation, with the backend to be
;; supplied.

(define (meaning e r tail?)
  (if (pair? e)
      (case (car e)
        ((quote) (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if) (meaning-alternative (cadr e) (caddr e) (cadddr e)
                                   r tail?))
        ((begin) (meaning-sequence (cdr e) r tail?))
        ((set!) (meaning-assignment (cadr e) (caddr e) r tail?))
        (else (meaning-application (car e) (cdr e) r tail?)))
      (if (symbol? e)
          (meaning-deref e r tail?)
          (meaning-quotation e r tail?))))

;; Literal values and quotations

(define (meaning-quotation v r tail?)
  (CONSTANT v))

;; Variable references

(define (meaning-deref n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((level (cadr kind))
                 (index (cddr kind)))
             (if (= level 0)
                 (SHALLOW-ARGUMENT-REF index)
                 (DEEP-ARGUMENT-REF level index))))
          ((global)
           (let ((index (cdr kind)))
             (CHECKED-GLOBAL-REF index)))
          ((predefined)
           (let ((index (cdr kind)))
             (PREDEFINED index))))
        (compiler-error "No such variable" n))))

;; Conditional

(define (meaning-alternative ec et ef r tail?)
  (let ((mc (meaning ec r #f))
        (mt (meaning et r tail?))
        (mf (meaning ef r tail?)))
    (ALTERNATIVE mc mt mf)))

;; Assignment

(define (meaning-assignment n e r tail?)
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((level (cadr kind))
                 (index (cddr kind)))
             (if (= level 0)
                 (SHALLOW-ASSIGNMENT! index m)
                 (DEEP-ASSIGNMENT! level index m))))
          ((global)
           (let ((index (cdr kind)))
             (GLOBAL-SET! index m)))
          ((predefined)
           (compiler-error
            "Attempted to assign to immutable variable" n)))
        (compiler-error "Unknown variable" n))))

;; Begin

(define (meaning-sequence e+ r tail?)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
          (meaning*-single-sequence (car e+) r tail?))
      (compiler-error "Illegal form (begin)")))

(define (meaning*-single-sequence e r tail?)
  (meaning e r tail?))

(define (meaning*-multiple-sequence e e* r tail?)
  (let ((m (meaning e r #f))
        (m+ (meaning-sequence e* r tail?)))
    (SEQUENCE m m+)))

;; OK now the slightly harder bits. Starting with

;; Abstraction

(define (meaning-abstraction nn* e+ r tail?)
  (let parse ((n* nn*)
              (regular '()))
    (cond ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
          ;; We ran through them all and no dot!
          ;; Use nn* to avoid having to reverse `regular`
          ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
          (else (meaning-dotted-abstraction
                 (reverse regular) n* e+ r tail?)))))

(define (meaning-fix-abstraction n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)))
    (FIX-CLOSURE m+ arity)))

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)))
    (NARY-CLOSURE m+ arity)))

;; The most fun of all, application

(define (meaning-application e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     ;; As before I move the checking into
                     ;; meaning-primitive-application; it just gets to
                     ;; messy here.
                     (meaning-primitive-application e e* r tail?)))))
        ((and (pair? e)
              (eq? 'lambda (car e)))
         (meaning-closed-application e e* r tail?))
        (else (meaning-regular-application e e* r tail?))))

(define (meaning-regular-application e e* r tail?)
  (let ((m (meaning e r #f))
        (m* (meaning* e* r (length e*) #f)))
    (if tail?
        (TR-REGULAR-CALL m m*)
        (REGULAR-CALL m m*))))

(define (meaning* e* r size tail?)
  (if (pair? e*)
      (meaning-some-args (car e*) (cdr e*) r size tail?)
      (meaning-no-arg r size tail?)))

(define (meaning-no-arg r size tail?)
  (ALLOCATE-FRAME size))

(define (meaning-some-args e e* r size tail?)
  (let ((m (meaning e r tail?))
        (m* (meaning* e* r size tail?))
        (index (- size (+ (length e*) 1))))
    (STORE-ARGUMENT m m* index)))

;; left-left-lambda
;; ((lambda (n*...) body) ee*...)
(define (meaning-closed-application e ee* r tail?)
  (let parse ((n* (cadr e))
              (e* ee*)
              (regular '()))
    (cond
      ((pair? n*)
       (if (pair? e*)
           (parse (cdr n*) (cdr e*) (cons (car n*) regular))
           (compiler-error "Too few arguments: need" (cadr e)
                           "got" ee*)))
      ((null? n*)
       (if (null? e*)
           (meaning-fix-closed-application
            (cadr e) (cddr e) ee* r tail?)
           (compiler-error "Too many arguments: need" (cadr e)
                           "got" ee*)))
      (else
        (meaning-dotted-closed-application
         (reverse regular) n* (cddr e) ee* r tail?)))))

;; ((lambda (a b) (+ a b)) 1 2)
(define (meaning-fix-closed-application n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) #f))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail?
        (TR-FIX-LET m* m+)
        (FIX-LET m* m+))))

;; ((lambda as (apply + as)) 1 2 3)
(define (meaning-dotted-closed-application n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail?
        (TR-FIX-LET m* m+)
        (FIX-LET m* m+))))

(define (meaning-dotted* e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-args (car e*) (cdr e*)
                                r size arity tail?)
      (meaning-no-dotted-arg r size arity tail?)))

(define (meaning-some-dotted-args e e* r size arity tail?)
  (let ((m (meaning e r tail?))
        (m* (meaning-dotted* e* r size arity tail?))
        (index (- size (+ (length e*) 1))))
    (if (< index arity)
        (STORE-ARGUMENT m m* index)
        (CONS-ARGUMENT m m* arity))))

(define (meaning-no-dotted-arg r size arity tail?)
  (ALLOCATE-DOTTED-FRAME arity))

(define (meaning-primitive-application e e* r tail?)
  (let ((desc (get-description e)))
    (and desc ;; I don't know why it wouldn't be there, but anyway
         ;; desc = (function address . arity)
         (or (eq? 'function (car desc))
             (compiler-error "Function expected"))
         (let ((address (cadr desc))
               (size (caddr desc)))
           (and
            ;; I did say I would check arity here
            (or (= size (length e*))
                (compiler-error "Wrong arity for " e
                                "expected" size))
            ;; This time I'll do it the book way; this sets up some of the VM
            ;; instructions later on.
            (case size
              ((0) (CALL0 address))
              ((1) (let ((m (meaning (car e*) r #f)))
                     (CALL1 address m)))
              ((2) (let ((m1 (meaning (car e*) r #f))
                         (m2 (meaning (cadr e*) r #f)))
                     (CALL2 address m1 m2)))
              ((3) (let ((m1 (meaning (car e*) r #f))
                         (m2 (meaning (cadr e*) r #f))
                         (m3 (meaning (caddr e*) r #f)))
                     (CALL3 address m1 m2 m3)))
              (else
                (meaning-regular-application e e* r tail?))))))))

;; ------- end frontend

;; ------- Backend

;; This is abstracted in the frontend; the simplest thing to do is
;; just exit.
(define (compiler-error . err)
  (error err))

;; Among other things, I need a lexical environment. This one is from
;; env.ss (in the same place as above), and keeps a list of lists of
;; names.

(define (r-extend* r n*)
  (cons n* r))

;; See if the given name is a local variable in the given environment
(define (local-variable? r i n)
  (and (pair? r)
       (let scan ((names (car r))
                  (j 0))
         (cond ((pair? names)
                (if (eq? n (car names))
                    `(local ,i . ,j)
                    (scan (cdr names) (+ j 1))))
               ((null? names)
                (local-variable? (cdr r) (+ i 1) n))
               ;; Don't think I understand this clause -- why would
               ;; these be improper? A convenience perhaps
               ((eq? n names) `(local ,i . ,j))))))

;; Names of mutable globals
(define g.current '())
;; Names of predefined globals
(define g.init '())

;; Initial env
(define r.init '())

(define (g.current-extend! n)
  (let ((level (length g.current)))
    (set! g.current (cons (cons n `(global . ,level)) g.current))
    level))

(define (g.init-extend! n)
  (let ((level (length g.init)))
    (set! g.init (cons (cons n `(predefined . ,level)) g.init))
    level))

(define (global-variable? g n)
  (let ((var (assq n g)))
    (and (pair? var) (cdr var))))

;; This is used when a variable is encountered.
(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)))

;; Setting up the predefined symbol table is different from the LISP
;; in Small Pieces code, because I'm trying to line things up with
;; primitives and values that are defined in Rust code rather than
;; inline. So: values and invocation of primitives go in Rust code,
;; and here (mostly) just gets names to translate into opcodes.

;; This is going to record the instruction to include for each
;; predefined variable. These are accessed by index, so the order must
;; match up here and in the Rust initialisation code.
(define *predefined* (make-vector 100))

;; Records a predefined value with a dedicated opcode.
(define (define-initial name opcode)
  (let ((level (g.init-extend! name)))
    (vector-set! *predefined* level (list opcode))))

;; Records a predefined values which will use the generic predefined
;; opcode.
(define (define-initial-generic* . names)
  (let loop ((name* names))
    (if (pair? name*)
        (let ((level (g.init-extend! (car name*))))
          (vector-set! *predefined* level `(predefined ,level))
          (loop (cdr name*))))))

;; Primitives can be dereferenced in two ways: they can be in head
;; position, or arg position. If in head position, the application is
;; compiled via a CALL* procedure (below), which looks up a
;; description by name. If in arg position (or there's no
;; description), they are dereferenced via `(PREDEFINED i)`, which can
;; compile to a dedicated opcode, or to an instruction with an index.

;; This is to capture more information about primitives, so they can
;; be compiled to call* instructions.
(define desc.init '())

(define (description-extend! name description)
  (set! desc.init (cons (cons name description) desc.init))
  name)

(define (get-description name)
  (let ((d (assq name desc.init)))
    (and (pair? d) (cdr d))))

;; This is almost circular, due to how the pretreatment works; it
;; looks up the description, then calls the `CALLn` with the given
;; arity; those will then use the CALL* opcode given.
(define (define-primitive name opcode arity)
  (description-extend! name `(function ,opcode ,arity)))

;; These are values that come predefined and have their own opcodes.
(define-initial 'true 'PREDEFINED_HASHT)
(define-initial 'false 'PREDEFINED_HASHF)
(define-initial 'nil 'PREDEFINED_NIL)
(define-initial 'cons 'PREDEFINED_CONS)
(define-initial 'car 'PREDEFINED_CAR)
(define-initial 'cdr 'PREDEFINED_CDR)
(define-initial 'pair? 'PREDEFINED_PAIR_P)
(define-initial 'symbol? 'PREDEFINED_SYMBOL_P)
(define-initial 'eq? 'PREDEFINED_EQ_P)

(define-primitive 'cons 'CALL2_cons 2)
(define-primitive 'car 'CALL1_car 1)
(define-primitive 'cdr 'CALL1_cdr 1)
(define-primitive 'pair? 'CALL1_pair_p 1)
(define-primitive 'symbol? 'CALL1_symbol_p 1)
(define-primitive 'eq? 'CALL2_eq_p 2)

;; These don't come with a dedicated opcode for the value, but they do
;; need an entry so that they'll be compiled to a predefind lookup.
(define-initial-generic*
  'read
  'newline
  'display
  '+
  '-
  '=
  '<
  '>
  '<=
  '>=
  '*
  '/)

;; They _do_ all have an inlined implementation
(define-primitive 'read 'CALL0_read 0)
(define-primitive 'newline 'CALL0_newline 0)
(define-primitive 'display 'CALL1_display 1)
(define-primitive '+ 'CALL2_PLUS 2)
(define-primitive '- 'CALL2_MINUS 2)
(define-primitive '= 'CALL2_EQUAL 2)
(define-primitive '< 'CALL2_LT 2)
(define-primitive '> 'CALL2_GT 2)
(define-primitive '<= 'CALL2_LE 2)
(define-primitive '>= 'CALL2_GE 2)
(define-primitive '* 'CALL2_TIMES 2)
(define-primitive '/ 'CALL2_DIVIDE 2)

;; These are primitives that don't have an inlined implementation, so
;; go via the generic predefined lookup and regular application.
(define-initial-generic*
  'apply
  'list)

;; --- Combinators

;; These are implementations for the procedures (generally in CAPS)
;; that create a result during pretreatment. In the original use,
;; these either produced thunks that returned a value, or operated on
;; registers and stack, or bytes that were interpreted (and could be
;; disassembled). Here I'm going to produce something fairly close to
;; the disassembly, which can be translated easily (in Rust) to my own
;; opcodes.

;; First, there's a few combinators, that create program
;; fragments. Each instruction is of the form `(opcode arg*)`, and the
;; fragments are lists of instructions.

(define (SHALLOW-ASSIGNMENT! j m)
  (append m (SET-SHALLOW-ARGUMENT! k)))

(define (DEEP-ASSIGNMENT! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)))

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)))

;; All the predefined things are stored as the instruction to fetch
;; it.
(define (PREDEFINED i)
  (if (< i (vector-length *predefined*))
      (list (vector-ref *predefined* i))
      (compiler-error "reference to undefined predefined")))

(define (CONSTANT v)
  (cond
    ((eq? v #t)    '((predefined-hasht)))
    ((eq? v #f)    '((predefined-hashf)))
    ((eq? v '())   '((predefined-nil)))
    ;; some allegedly commonly-used integers get their own opcodes
    ((equal? v -1) '((int_neg1)))
    ((equal? v 0)  '((int_0)))
    ((equal? v 1)  '((int_1)))
    ((equal? v 2)  '((int_2)))
    ((equal? v 3)  '((int_3)))
    ;; other integers up to 255 get an opcode and an operand of the
    ;; number itself
    ((and (integer? v) (<= 0 v) (< v 256)) ;; book has 255 here?
     `((int ,v)))
    (else (EXPLICIT-CONSTANT v))))

;; NB no attempt to dedup constants
(define (EXPLICIT-CONSTANT v)
  (set! *quotations* (append *quotations* (list v)))
  `((constant ,(- (length *quotations*) 1))))

;; JUMP-FALSE and GOTO are simplified (and appear later) because the
;; offset is not spread over two bytes.

(define (ALTERNATIVE m1 m2 m3)
  ;; GOTO can be different lengths now, so we have to do it first
  (let ((mm2 (append m2 (GOTO (length m3)))))
    (append m1 ;; result of test clause to *val*
            (JUMP-FALSE (length mm2)) ;; jump over success k (and goto)
            mm2 ;; success k, including GOTO
            m3)))

(define (NARY-CLOSURE m+ arity)
  (let ((the-function
         (append (ARITY>=? (+ arity 1)) ;; bail if not enough arguments
                 (PACK-FRAME! arity) ;; collect varargs
                 (EXTEND-ENV) ;; extend *env* with arguments
                 m+ ;; execute the forms
                 (RETURN))))
    (append (CREATE-CLOSURE 1) ;; make a closure that starts at pc+1
            (GOTO (length the-function)) ;; skip the definition
            the-function)))

(define (SEQUENCE m m+)
  (append m m+))

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+))
(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)))

;; These encode the built-in primitives. The argument is simply the
;; opcode for the primitive, put in the description by
;; `(define-primitive ...)`, and fished out by
;; `meaning-primitive-application`.

(define (INVOKE opcode)
  `((,opcode)))

(define (CALL0 opcode)
  (INVOKE0 opcode))

(define (CALL1 address m1)
  (append m1 (INVOKE address)))

(define (CALL2 address m1 m2)
  (append m1 ;; m1 -> *val*
          (PUSH-VALUE) ;; m1 -> stack
          m2 ;; m2 -> *val*
          (POP-ARG1) ;; m1 -> *arg1*
          (INVOKE address)))

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE)
          m2 (PUSH-VALUE)
          m3
          (POP-2ARG)
          (INVOKE address)))

(define (FIX-CLOSURE m+ arity)
  (let ((the-function
         (append (ARITY=? arity)
                 (EXTEND-ENV)
                 m+
                 (RETURN))))
    (let ((goto (GOTO (length the-function))))
      (append (CREATE-CLOSURE (length goto))
              goto
              the-function))))

(define (REGULAR-CALL m m*)
  (append m (PUSH-VALUE)
          m*
          (POP-FUNCTION)
          (PRESERVE-ENV) (FUNCTION-INVOKE) (RESTORE-ENV)))

(define (TR-REGULAR-CALL m m*)
  (append m (PUSH-VALUE)
          m* (POP-FUNCTION)
          (FUNCTION-GOTO)))

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE) m* (POP-FRAME! rank)))

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE) m* (POP-CONS-FRAME! arity)))

;; These just reduce to instructions.

(define (SHALLOW_ARGUMENT_REF i) `((shallow-argument-ref ,i)))
(define (SET-SHALLOW-ARGUMENT! j) `((set-shallow-argument! ,j)))
(define (DEEP-ARGUMENT-REF level index) `((deep-argument-ref ,level ,index)))
(define (SET-DEEP-ARGUMENT! level index) `((set-deep-argument! ,level ,index)))
(define (SET-GLOBAL! i) `((set-global! ,i)))
(define (JUMP-FALSE offset) `((jump ,offset)))
(define (GOTO offset) `((goto ,offset)))
(define (EXTEND-ENV) '((extend-env)))
(define (UNLINK-ENV) '((unlink-env)))
(define (PUSH-VALUE) '((push-value)))
(define (POP-ARG1) '((pop-arg1)))
(define (POP-2ARG) '((pop-2arg)))
(define (PRESERVE-ENV) '((preserve-env)))
(define (RESTORE-ENV) '((restore-env)))
(define (POP-FUNCTION) '((pop-function)))
(define (CREATE-CLOSURE offset) `((create-closure ,offset)))
(define (RETURN) '((return)))
(define (PACK-FRAME! arity) `((pack-frame! ,arity)))
(define (FUNCTION-INVOKE) '((function-invoke)))
(define (FUNCTION-GOTO) '((function-goto)))
(define (POP-FRAME! rank) `((pop-frame! ,rank)))
(define (POP-CONS-FRAME! arity) `((pop-cons-frame! ,arity)))
(define (ALLOCATE-FRAME size) `((allocate-frame ,(+ size 1))))
(define (ALLOCATE-DOTTED-FRAME arity) `((allocate-dotted-frame ,(+ arity 1))))
(define (ARITY>=? arity+1) `((arity>=? ,arity+1)))
(define (ARITY=? n) `((arity=? ,(+ n 1))))
(define (FINISH) '((finish)))
