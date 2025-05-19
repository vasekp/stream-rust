use super::*;

use std::fmt::{Display, Debug, Formatter};
use std::cell::Cell;

mod chr;
mod stream;
mod length;
mod siter;
mod list;
mod litstr;

pub use chr::{Char, CharCase};
pub use stream::*;
pub use length::Length;
pub use siter::{SIterator, StringIterator};
pub(crate) use list::List;
pub(crate) use litstr::LiteralString;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub(crate) use tests::*;

/// An `Item` is a concrete value or stream, the result of evaluation of a [`Node`].
pub enum Item {
    Number(Number),
    Bool(bool),
    Char(Char),
    Stream(Box<dyn Stream>)
}

impl Item {
    pub fn new_number(value: impl Into<Number>) -> Item {
        Item::Number(value.into())
    }

    pub fn new_bool(value: bool) -> Item {
        Item::Bool(value)
    }

    pub fn new_char(value: impl Into<Char>) -> Item {
        Item::Char(value.into())
    }

    pub fn new_stream(value: impl Stream + 'static) -> Item {
        Item::Stream(Box::new(value))
    }

    pub fn new_string(value: impl Into<String>) -> Item {
        Item::Stream(Box::new(LiteralString::from(value.into())))
    }

    pub fn as_num(&self) -> Result<&Number, BaseError> {
        match self {
            Item::Number(x) => Ok(x),
            _ => Err(format!("expected number, found {:?}", &self).into())
        }
    }

    pub fn to_bool(&self) -> Result<bool, BaseError> {
        match self {
            Item::Bool(x) => Ok(*x),
            _ => Err(format!("expected boolean value, found {:?}", &self).into())
        }
    }

    pub fn is_stream(&self) -> bool {
        matches!(self, Item::Stream(_))
    }

    pub fn as_stream(&self) -> Result<&(dyn Stream + 'static), BaseError> {
        match self {
            Item::Stream(s) => Ok(&**s),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn to_stream(&self) -> Result<Box<dyn Stream>, BaseError> {
        match self {
            Item::Stream(s) => Ok(s.clone_box()),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn format(&self, max_items: Option<usize>, max_len: Option<usize>) -> (String, usize, Option<StreamError>) {
        struct Stateful<'item> {
            item: &'item Item,
            count: Cell<usize>,
            err: Cell<Option<StreamError>>
        }

        impl Display for Stateful<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.item.format_int(f, &self.count, &self.err)
            }
        }

        let s = Stateful{item: self, count: Default::default(), err: Default::default()};
        let result = match (max_items, max_len) {
            (Some(width), Some(prec)) => format!("{s:w$.p$}", w = width, p = prec),
            (Some(width), None) => format!("{s:w$}", w = width),
            (None, Some(prec)) => format!("{s:.p$}", p = prec),
            (None, None) => format!("{s}")
        };
        (result, s.count.take(), s.err.take())
    }

    pub(crate) fn format_int(&self, f: &mut Formatter<'_>, count: &Cell<usize>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        use Item::*;
        count.set(count.get() + 1);
        match self {
            Number(n) => write!(f, "{n}"),
            Bool(b) => write!(f, "{b}"),
            Char(c) => write!(f, "{c}"),
            Stream(s) => s.writeout(f, count, error)
        }
    }

    pub(crate) fn type_str(&self) -> &'static str {
        use Item::*;
        match self {
            Number(_) => "number",
            Bool(_) => "bool",
            Char(_) => "char",
            Stream(s) if s.is_string().is_true() => "string",
            Stream(_) => "stream"
        }
    }

    pub(crate) fn try_eq(&self, other: &Self) -> Result<bool, BaseError> {
        use Item::*;
        Ok(match (self, other) {
            (Number(x1), Number(x2)) => x1 == x2,
            (Bool(x1), Bool(x2)) => x1 == x2,
            (Char(x1), Char(x2)) => x1 == x2,
            (Stream(x1), Stream(x2)) => {
                let l1 = x1.length();
                let l2 = x2.length();
                if !Length::possibly_eq(&l1, &l2) { return Ok(false); }
                for (x, y) in x1.iter().zip(x2.iter()) {
                    check_stop!();
                    if !x?.try_eq(&y?)? { return Ok(false); }
                }
                true
            },
            _ => false
        })
    }
}

impl Default for Item {
    fn default() -> Item {
        Item::Number(Default::default())
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.format_int(f, &Default::default(), &Default::default())
    }
}

impl Debug for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", self.type_str())?;
        self.format_int(f, &Default::default(), &Default::default())
    }
}

impl Describe for Item {
    fn describe_prec(&self, prec: u32) -> String {
        use Item::*;
        match self {
            Number(n) => n.describe_prec(prec),
            Bool(b) => format!("{b}"),
            Char(c) => format!("{c}"),
            Stream(s) => s.describe_prec(prec)
        }
    }
}

impl PartialEq for Item {
    /// `PartialEq::eq()` must be used with caution because if asked to compare two infinite streams it
    /// will never return. User-facing code should use [`Item::try_eq()`] which is prepared to handle
    /// interruptions.
    fn eq(&self, other: &Self) -> bool {
        use Item::*;
        match (self, other) {
            (Number(x1), Number(x2)) => x1 == x2,
            (Bool(x1), Bool(x2)) => x1 == x2,
            (Char(x1), Char(x2)) => x1 == x2,
            (Stream(x1), Stream(x2)) => {
                let l1 = x1.length();
                let l2 = x2.length();
                if !Length::possibly_eq(&l1, &l2) { return false; }
                x1.iter().zip(x2.iter())
                    .all(|(x, y)| x == y)
            },
            _ => false
        }
    }
}

impl Clone for Item {
    fn clone(&self) -> Item {
        use Item::*;
        match self {
            Number(x) => Number(x.clone()),
            Bool(x) => Bool(*x),
            Char(x) => Char(x.clone()),
            Stream(s) => Stream(s.clone_box())
        }
    }
}

pub(crate) enum ProxyItem<'a> {
    Number(&'a Number),
    //Bool(bool),
    Char(&'a Char),
    //Stream(&'a (dyn Stream + 'static))
}

impl Describe for ProxyItem<'_> {
    fn describe_prec(&self, prec: u32) -> String {
        use ProxyItem::*;
        match self {
            Number(n) => n.describe_prec(prec),
            //Bool(b) => format!("{b}"),
            Char(c) => format!("{c}"),
            //Stream(s) => s.describe_prec()
        }
    }
}
