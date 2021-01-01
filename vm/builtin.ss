;; Procedures that are used in compile.ss and are usually available in
;; the bootstrapping system, but aren't primitives in the VM.

;; These get used, up to three 'd's.
(define (cadr e) (car (cdr e)))
(define (cddr e) (cdr (cdr e)))
(define (caddr e) (car (cddr e)))
(define (cdddr e) (cdr (cddr e)))
(define (cadddr e) (car (cdddr e)))

(define (list . args) args)

(define (length lst)
  (let loop ((len 0)
             (rest lst))
    (if (null? rest)
        len
        (loop (+ len 1) (cdr rest)))))

;; This is the `eq?` version of {assq, assv, assoc}
(define (assq v lst)
  (if (null? lst)
      #f
      (let ((f (car lst)))
        (if (eq? v (car f))
            f
            (assq v (cdr lst))))))

;; Returns the list at the point v is found, or #f if not found.
(define (member v lst)
  (if (pair? lst)
      (if (equal? v (car lst))
          lst
          (member v (cdr lst)))
      #f))

;; This map only needs to deal with one list.
(define (map fn lst)
  (let loop ((rest lst)
             (acc '()))
    (if (null? rest)
        (reverse acc)
        (loop (cdr rest) (cons (fn (car rest)) acc)))))

;; Used for compiling quasiquote, possibly elsewhere.
(define (append1 list1 list2)
  (cond ((null? list2) list1)
        ((null? list1) list2)
        (else
         (let loop ((rev (reverse list1))
                    (acc list2))
           (if (pair? rev)
               (loop (cdr rev) (cons (car rev) acc))
               acc)))))

(define (append . lists)
  (let loop ((rest lists))
    (if (null? rest)
        '()
        (append1 (car rest) (loop (cdr rest))))))

;; Needed for append1 above, maybe elsewhere.
(define (reverse lst)
  (let loop ((rest lst)
             (acc '()))
    (if (pair? rest)
        (loop (cdr rest) (cons (car rest) acc))
        acc)))
