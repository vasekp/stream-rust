use crate::base::*;
use std::collections::HashMap;
use once_cell::sync::Lazy;
use std::rc::Rc;

type Constructor = for<'a> fn(Node, &'a Rc<Env>) -> Result<Item, StreamError>;

pub(crate) type Keywords = HashMap<&'static str, Constructor>;

static KEYWORDS: Lazy<Keywords> = Lazy::new(|| {
    let mut keywords = Default::default();
    crate::ops::init(&mut keywords);
    keywords
});

pub(crate) fn find_keyword(name: &str) -> Result<Constructor, BaseError> {
    KEYWORDS.get(name).copied()
        .ok_or_else(|| format!("symbol '{name}' not found").into())
}
