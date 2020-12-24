use std::rc::Rc;
use std::cell::RefCell;

use crate::values::ValueRef;
use crate::instructions;

// Activation frames. These represent an environment for a
// closure. Because closures need their environment to be preserved,
// frames are kept on the heap; thus, the reference-counted FrameRef.

pub type FrameRef = Rc<Frame>;

#[derive(Clone,Debug,PartialEq)]
pub struct Frame {
    pub args: Vec<RefCell<Option<ValueRef>>>,
    pub next: Option<FrameRef>,
}

impl Frame {
    pub fn new(size: instructions::index) -> Frame {
        Frame{
            args: vec![RefCell::new(None); size as usize],
            next: None,
        }
    }

    pub fn extend(next: FrameRef, size: instructions::index) -> Frame {
        Frame{
            args: vec![RefCell::new(None); size as usize],
            next: Some(next),
        }
    }
}
