// Implements a reader for this Scheme.

use nom::{
    IResult,
};
use nom::{
    bytes::complete::{tag,is_not},
    combinator::{map, all_consuming},
    multi::many0,
    character::complete::{satisfy,char,multispace0,digit1,one_of},
    branch::alt,
    sequence::{tuple,delimited,preceded},
};

#[derive(Debug,PartialEq,Clone)]
pub enum Expr {
    Int(i64),
    Symbol(String),
    String(String),
    List(Box<Expr>, Vec<Expr>), // head, tail
    Nil, // empty list
    True,
    False,
}

// See https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_4.html for
// Scheme (R4RS) grammar. I'm not trying to follow it faithfully here,
// just borrowing some of the definitions, e.g., for symbols.

// https://github.com/Geal/nom/blob/master/doc/nom_recipes.md#integers
// https://github.com/Geal/nom/blob/master/examples/s_expression.rs
fn parse_int(i: &str) -> IResult<&str, Expr> {
    map(digit1, |digit_str: &str| {
        Expr::Int(digit_str.parse::<i64>().unwrap()) // <-- assume it'll parse
    })(i)
}

const SYMBOL_EXTENDED_CHARS: &str = "+-.*/<=>!?:$%_&~^";

fn symbol_extended(i: &str) -> IResult<&str, char> {
    one_of(SYMBOL_EXTENDED_CHARS)(i)
}

fn parse_bool(i: &str) -> IResult<&str, Expr> {
    alt((map(tag("#t"), |_| { Expr::True }),
         map(tag("#f"), |_| { Expr::False })))(i)
}

fn parse_symbol(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            alt((symbol_extended, satisfy(|c| { c.is_alphabetic() }))),
            many0(alt((symbol_extended, satisfy(|c| { c.is_alphanumeric() }))))
        )), |(start, rest)| {
            use std::iter::FromIterator;
            let mut s = String::from_iter(rest);
            s.insert(0, start);
            Expr::Symbol(s)
        })(i)
}

fn parse_string(i: &str) -> IResult<&str, Expr> {
    map(delimited(
        char('"'),
        // dead simple for the minute -- no escapes
        is_not("\""),
        char('"'),
    ), |s| {
        Expr::String(String::from(s))
    })(i)
}

fn parse_atom(i: &str) -> IResult<&str, Expr> {
    alt((parse_int, parse_symbol, parse_bool, parse_string))(i)
}

fn parse_list(i: &str) -> IResult<&str, Expr> {
    alt((
        map(delimited(
            char('('),
            multispace0,
            char(')')
        ), |_| { Expr::Nil }),

        map(delimited(
            char('('),
            tuple((
                preceded(multispace0, parse_expr),
                many0(preceded(multispace0, parse_expr)),
            )),
            preceded(multispace0, char(')'))
        ), |(head, tail)| { Expr::List(Box::new(head), tail) })))(i)
}

fn parse_expr(i: &str) -> IResult<&str, Expr> {
    alt(( // reader syntax for quote
        map(preceded(char('\''), alt((parse_list, parse_atom))),
            |e| Expr::List(Box::new(Expr::Symbol(String::from("quote"))), vec![e])
        ),
        map(preceded(char('`'), alt((parse_list, parse_atom))),
            |e| Expr::List(Box::new(Expr::Symbol(String::from("quasiquote"))), vec![e])
        ),
        map(preceded(char(','), alt((parse_list, parse_atom))),
            |e| Expr::List(Box::new(Expr::Symbol(String::from("unquote"))), vec![e])
        ),
        map(preceded(tag(",@"), alt((parse_list, parse_atom))),
            |e| Expr::List(Box::new(Expr::Symbol(String::from("unquote-splicing"))), vec![e])
        ),
        // normal ol' expression
        alt((parse_list, parse_atom)))
    )(i)
}

// parse a _whole line_ (must consume all the byteses).
pub fn parse_line(i: &str) -> IResult<&str, Expr> {
    all_consuming(delimited(
        multispace0,
        parse_expr,
        multispace0
    ))(i)
}

#[test]
fn atom_parser() {
    // integers
    assert_eq!(Ok(("", Expr::Int(12))), parse_int("12"));
    // symbols
    assert_eq!(Ok(("", Expr::Symbol(String::from("sym")))), parse_symbol("sym"));
    assert_eq!(Ok(("",Expr::Symbol(String::from("=sym/098765+")))), parse_symbol("=sym/098765+"));
    // bools
    assert_eq!(Ok(("", Expr::String(String::from("foobar")))), parse_string("\"foobar\""));
}

#[test]
fn list_parser() {
    let sym = Expr::Symbol(String::from("sym"));
    let one = Expr::Int(1);
    let two = Expr::Int(2);
    let three = Expr::Int(3);
    let three1 = three.clone();

    assert_eq!(Ok(("", Expr::List(Box::new(one), vec![two, three]))), parse_list("(1 2 3)"));
    assert_eq!(Ok(("", Expr::List(Box::new(sym), vec![three1]))), parse_list("(sym 3)"));
    assert_eq!(Ok(("", Expr::Nil)), parse_list("()"));
}

#[test]
fn expr_parser() {
    let sym = Expr::Symbol(String::from("sym"));
    let one = Expr::Int(1);
    let two = Expr::Int(2);
    let three = Expr::Int(3);
    assert_eq!(Ok(("", Expr::List(Box::new(sym), vec![one, Expr::List(Box::new(two), vec![three])]))),
               parse_expr("(sym 1 (2 3))"))
}

#[test]
fn quote_syntax() {
    let quote0 = Expr::Symbol(String::from("quote"));
    let sym = Expr::Symbol(String::from("sym"));
    let bol = Expr::List(Box::new(quote0.clone()), vec![Expr::Symbol(String::from("bol"))]);
    let two = Expr::Int(2);
    let three = Expr::Int(3);
    let nums = Expr::List(Box::new(quote0.clone()), vec![Expr::List(Box::new(two), vec![three])]);
    assert_eq!(Ok(("", Expr::List(Box::new(sym), vec![bol, nums]))),
               parse_expr("(sym 'bol '(2 3))"));
    assert_eq!(Ok(("", Expr::List(Box::new(quote0.clone()), vec![Expr::Nil]))), parse_expr("'()"));
}

#[test]
fn quasi_reader_syntax() {
    let orig = "(foo `(bar ,baz ,@boo))";
    let expanded = "(foo (quasiquote (bar (unquote baz) (unquote-splicing boo))))";
    assert_eq!(parse_expr(orig), parse_expr(expanded));
}
