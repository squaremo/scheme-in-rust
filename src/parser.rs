use nom::{
    IResult,
};
use nom::character::complete::{digit1,one_of};
use nom::{
    combinator::map,
    multi::many0,
    character::complete::satisfy,
    branch::alt,
    sequence::tuple,
};

// I depart from the how-to here (the author abandoned it after this
// point anyway). It defined `Ops`, `Primitives` and `SepcialForms`;
// but I'm going to get just the abstract syntax, and distinguish
// between those things when evaluating.


// See https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_4.html for
// Scheme (R4RS) grammar. I'm not trying to follow it faithfully here,
// just borrowing some of the definitions, e.g., for symbols.

// https://github.com/Geal/nom/blob/master/doc/nom_recipes.md#integers
// https://github.com/Geal/nom/blob/master/examples/s_expression.rs
fn parse_int(i: &str) -> IResult<&str, Atom> {
    map(digit1, |digit_str: &str| {
        Atom::Int(digit_str.parse::<i64>().unwrap()) // <-- assume it'll parse
    })(i)
}

const SYMBOL_EXTENDED_CHARS: &str = "+-.*/<=>!?:$%_&~^";

fn symbol_extended(i: &str) -> IResult<&str, char> {
    one_of(SYMBOL_EXTENDED_CHARS)(i)
}

fn parse_symbol(i: &str) -> IResult<&str, Atom> {
    map(
        tuple((
            alt((symbol_extended, satisfy(|c| { c.is_alphabetic() }))),
            many0(alt((symbol_extended, satisfy(|c| { c.is_alphanumeric() }))))
        )), |(start, rest)| {
            use std::iter::FromIterator;
            let mut s = String::from_iter(rest);
            s.insert(0, start);
            Atom::Symbol(s)
        })(i)
}

#[derive(Debug, PartialEq, Clone)] // <-- these expected by the parser combinators
enum Atom {
    Symbol(String),
    Int(i64)
    // more kinds of atom go here: string, float, bool
}

#[derive(Debug, PartialEq, Clone)]
enum Expr {
    List(Vec<Expr>),
    Atom(Atom)
    // vector might go here too
}

#[test]
fn atom_parser() {
    // integers
    assert_eq!(Ok(("", Atom::Int(12))), parse_int("12"));
    // symbols
    assert_eq!(Ok(("", Atom::Symbol(String::from("sym")))), parse_symbol("sym"));
    assert_eq!(Ok(("", Atom::Symbol(String::from("=sym/098765+")))), parse_symbol("=sym/098765+"));
}
