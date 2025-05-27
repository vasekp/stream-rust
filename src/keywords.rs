use crate::base::*;
use std::collections::HashMap;
use once_cell::sync::Lazy;

type Constructor = for<'a> fn(Node, &'a Env) -> Result<Item, StreamError>;

pub(crate) type Keywords = HashMap<&'static str, Constructor>;

static KEYWORDS: Lazy<Keywords> = Lazy::new(|| {
    let mut keywords = Default::default();
    crate::ops::init(&mut keywords);
    keywords
});

pub(crate) fn find_keyword(name: &str) -> Option<Constructor> {
    KEYWORDS.get(name).copied()
}
