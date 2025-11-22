use crate::base::*;
use std::collections::HashMap;
use once_cell::sync::Lazy;

type Constructor = fn(Node, &'_ Env) -> Result<Item, StreamError>;

#[derive(Default)]
pub(crate) struct Symbols(HashMap<&'static str, Constructor>);

static SYMBOLS: Lazy<Symbols> = Lazy::new(|| {
    let mut symbols = Default::default();
    crate::ops::init(&mut symbols);
    symbols
});

impl Symbols {
    pub(crate) fn insert(&mut self, name: &'static str, ctor: Constructor) {
        self.0.insert(name, ctor);
    }

    pub(crate) fn find_ctor(name: &str) -> Option<Constructor> {
        SYMBOLS.0.get(name).copied()
    }
}
