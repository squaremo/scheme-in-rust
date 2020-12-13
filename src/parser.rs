use nom::*;

// macro named! names a parsing rule, here as `string`, with the
// return type <rest of bytes, String content>.
named!(string<&[u8], String>,
       do_parse!( // <-- like do {} syntax
           word: ws!(alphanumeric) >> // <-- eat whitespace, put alphanumeric (builtin of nom) in `word`
               (String::from_utf8(word.to_vec()).unwrap()) // <-- result -- unwrap Result, optimistically
       )
);

#[test]
fn string_parser() {
    let comp_string = String::from("hi");

    match string(b"hi") {
        IResult::Done(_,s) => assert_eq!(comp_string, s),
        _ => panic!("Failed to parse string")
    }

    match string(b"   hi  ") {
        IResult::Done(_, s) => assert_eq!(comp_string, s),
        _ => panic!("Failed to parse string with whitespace")
    }

    match string(b"hi      ") {
        IResult::Done(_, s) => assert_eq!(comp_string, s),
        _ => panic!("Failed to parse string")
    }

    match string(b"        hi") {
        IResult::Done(_, s) => assert_eq!(comp_string, s),
        _ => panic!("Failed to parse string")
    }
}
