use crate::base::*;

use std::collections::HashMap;
use std::collections::hash_map::Entry;

/// A set of distinct [`Item`]s kept in order of first insertion.
///
/// Numbers, booleans and characters are looked up by hash. Streams and strings are lazy and can't
/// be hashed, so they are compared with [`Item::try_eq`], but only against the streams and
/// strings already present, never against the hashed items (an item of a hashable type is never
/// equal to a stream or a string).
#[derive(Default)]
pub(crate) struct Distinct {
    items: Vec<Item>,
    hashed: HashMap<Key, usize>,
    others: Vec<usize>,
}

#[derive(PartialEq, Eq, Hash)]
enum Key {
    Number(Number),
    Bool(bool),
    Char(Char),
}

impl Distinct {
    /// Inserts `item` unless an equal one is already present. Returns the position of the item
    /// among the distinct ones and whether it was new.
    pub fn insert(&mut self, item: &Item) -> SResult<(usize, bool)> {
        let key = match item {
            Item::Number(num) => Some(Key::Number(num.clone())),
            Item::Bool(val) => Some(Key::Bool(*val)),
            Item::Char(ch) => Some(Key::Char(*ch)),
            _ => None,
        };
        let found = match key {
            Some(key) => match self.hashed.entry(key) {
                Entry::Occupied(entry) => Some(*entry.get()),
                Entry::Vacant(entry) => {
                    entry.insert(self.items.len());
                    None
                },
            },
            None => {
                let mut found = None;
                for &index in &self.others {
                    if item.try_eq(&self.items[index])? {
                        found = Some(index);
                        break;
                    }
                }
                if found.is_none() {
                    self.others.push(self.items.len());
                }
                found
            },
        };
        match found {
            Some(index) => Ok((index, false)),
            None => {
                self.items.push(item.clone());
                Ok((self.items.len() - 1, true))
            },
        }
    }

    pub fn into_items(self) -> Vec<Item> {
        self.items
    }
}
