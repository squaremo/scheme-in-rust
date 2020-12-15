use std::collections::HashMap;
use std::rc::Rc;

// Environments are a chain of hashmaps. Why a chain? So that each new
// lexical environment can shadow variables without disturbing older
// bindings.

use crate::value::ValueRef;

// Env values end up in closures, and at some point I may want to
// mutate them; so, I'm going to use reference-counted pointers to
// them when passing them around.
pub type EnvRef = Rc<Env>;

#[derive(Debug,PartialEq,Clone)]
pub struct Env {
    parent: Option<EnvRef>,
    here: HashMap<String, ValueRef>,
}

impl Env {

    pub fn top_env() -> Env {
        Env{
            parent: None,
            here: HashMap::new(),
        }
    }

    pub fn new(parent: &EnvRef) -> Env {
        Env{
            parent: Some(parent.clone()),
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
