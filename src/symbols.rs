use crate::base::*;
use std::collections::HashMap;
use once_cell::sync::Lazy;

type Constructor = fn(Node, &'_ Env) -> Result<Item, StreamError>;

pub(crate) type Symbols = HashMap<&'static str, Constructor>;

static SYMBOLS: Lazy<Symbols> = Lazy::new(|| {
    let mut symbols = Default::default();
    crate::ops::init(&mut symbols);
    symbols
});

pub(crate) fn find_symbol(name: &str) -> Option<Constructor> {
    SYMBOLS.get(name).copied()
}
