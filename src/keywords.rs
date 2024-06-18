use crate::base::*;
use std::collections::HashMap;
use once_cell::sync::Lazy;

type Constructor = for<'a> fn(Node, &'a Env) -> Result<Item, StreamError>;

pub(crate) type Keywords = HashMap<&'static str, Constructor>;

static KEYWORDS: Lazy<Keywords> = Lazy::new(|| {
    let mut keywords = Default::default();
    crate::lang::init(&mut keywords);
    crate::ops::init(&mut keywords);
    keywords
});

pub(crate) fn find_keyword(name: &str) -> Result<Constructor, StreamError> {
    KEYWORDS.get(name).copied()
        .ok_or_else(|| StreamError::from(format!("symbol '{name}' not found")))
}
