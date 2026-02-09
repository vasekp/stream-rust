use crate::base::*;

pub trait Tracer {
    fn log(&mut self, ev: Event<'_>);
}

pub enum Event<'a> {
    Enter(&'a Node),
    Leave(&'a Result<Item, StreamError>)
}

impl Tracer for () {
    fn log(&mut self, _ev: Event<'_>) { }
}
