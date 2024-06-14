use crate::base::*;
use std::collections::HashMap;

type Constructor = fn(Node) -> Result<Item, StreamError>;

pub(crate) type Keywords = HashMap<&'static str, Constructor>;

static mut KEYWORDS: Option<Keywords> = None;

pub(crate) fn find_keyword(name: &str) -> Result<Constructor, StreamError> {
    let keywords = unsafe {
        if KEYWORDS.is_none() {
            let mut keywords = Default::default();
            crate::lang::init(&mut keywords);
            crate::ops::init(&mut keywords);
            KEYWORDS = Some(keywords);
        }
        KEYWORDS.as_ref().unwrap()
    };
    keywords.get(name).copied()
        .ok_or_else(|| StreamError::from(format!("symbol '{name}' not found")))
}
