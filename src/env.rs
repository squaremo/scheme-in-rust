use std::collections::HashMap;

// Environments are a chain of hashmaps. Why a chain? So that each new
// lexical environment can shadow variables without disturbing older
// bindings.

use crate::value::ValueRef;

pub struct Env {
    parent: Option<Box<Env>>,
    here: HashMap<String, ValueRef>,
}

impl Env {

    pub fn top_env() -> Env {
        Env{
            parent: None,
            here: HashMap::new(),
        }
    }

    pub fn new_env(parent: Env) -> Env {
        Env{
            parent: Some(Box::new(parent)),
            here: HashMap::new(),
        }
    }

    pub fn set_env(&mut self, sym: &str, val: ValueRef) {
        self.here.insert(String::from(sym), val);
    }

    pub fn lookup(&self, sym: &str) -> Option<ValueRef> {
        let mut env = self;
        loop {
            if let Some(val) = env.here.get(sym) {
                break Some(ValueRef::clone(&val))
            } else {
            if let Some(parent) = &env.parent {
                env = &parent
            } else {
                break None
            }
            }
        }
    }
}
