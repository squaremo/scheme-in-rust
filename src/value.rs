#[derive(Debug, PartialEq, Clone)] // <-- these expected by the parser combinators
pub enum Value {
    Symbol(String),
    Int(i64),
    List(Vec<Value>),
    // more kinds of atom go here: string, float, bool
}
