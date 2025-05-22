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
    Stream(Box<dyn Stream>),
    String(Box<dyn Stream>),
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

    pub fn new_string(value: &str) -> Item {
        Item::String(Box::new(LiteralString::from(value)))
    }

    pub fn new_string2(value: impl Stream + 'static) -> Item {
        Item::String(Box::new(value))
    }

    pub fn empty_stream() -> Item {
        Item::Stream(Box::new(EmptyStream))
    }

    pub fn empty_stream_or_string(is_string: bool) -> Item {
        if is_string {
            Item::String(Box::new(LiteralString::from("")))
        } else {
            Item::empty_stream()
        }
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

    pub fn into_char(self) -> Result<Char, BaseError> {
        match self {
            Item::Char(x) => Ok(x),
            _ => Err(format!("expected character, found {:?}", &self).into())
        }
    }

    pub fn is_stream(&self) -> bool {
        matches!(self, Item::Stream(_))
    }

    pub fn as_stream(&self) -> Result<&(dyn Stream + 'static), BaseError> {
        match self {
            Item::Stream(s) | Item::String(s) => Ok(&**s),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn to_stream(&self) -> Result<Box<dyn Stream>, BaseError> {
        match self {
            Item::Stream(s) | Item::String(s) => Ok(s.clone_box()),
            _ => Err(format!("expected stream, found {:?}", &self).into())
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Item::String(_))
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
            Stream(s) => s.writeout_stream(f, count, error),
            String(s) => s.writeout_string(f, error),
        }
    }

    pub(crate) fn type_str(&self) -> &'static str {
        use Item::*;
        match self {
            Number(_) => "number",
            Bool(_) => "bool",
            Char(_) => "char",
            Stream(_) => "stream",
            String(_) => "string",
        }
    }

    pub(crate) fn try_eq(&self, other: &Self) -> Result<bool, BaseError> {
        use Item::*;
        Ok(match (self, other) {
            (Number(x1), Number(x2)) => x1 == x2,
            (Bool(x1), Bool(x2)) => x1 == x2,
            (Char(x1), Char(x2)) => x1 == x2,
            (Stream(x1), Stream(x2)) | (String(x1), String(x2)) => {
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

    pub(crate) fn lex_cmp(&self, other: &Self, alpha: &Rc<Alphabet>) -> Result<std::cmp::Ordering, BaseError> {
        use Item::*;
        use std::cmp::Ordering;
        Ok(match (self, other) {
            (Number(x), Number(y)) => x.cmp(y),
            (Bool(x), Bool(y)) => x.cmp(y),
            (Char(x), Char(y)) => alpha.cmp(x, y)?,
            (Stream(x), Stream(y)) | (String(x), String(y)) =>{
                let mut xi = x.iter();
                let mut yi = y.iter();
                loop {
                    check_stop!();
                    let lhs = xi.next().transpose()?;
                    let rhs = yi.next().transpose()?;
                    match (lhs, rhs) {
                        (None, None) => break Ordering::Equal,
                        (Some(_), None) => break Ordering::Greater,
                        (None, Some(_)) => break Ordering::Less,
                        (Some(lhs), Some(rhs)) => match lhs.lex_cmp(&rhs, alpha)? {
                            Ordering::Less => break Ordering::Less,
                            Ordering::Greater => break Ordering::Greater,
                            Ordering::Equal => continue
                        }
                    }
                }
            },
            (x, y) => return Err(format!("can't compare {x:?} with {y:?}").into())
        })
    }
}

impl Default for Item {
    fn default() -> Item {
        Item::Number(Default::default())
    }
}

impl Display for Item {
    /// Format this `Item` in human-readable form. For streams and strings, the formatter may specify
    /// a maximum number of items (using `{:n}`) or maximum width in characters (using `"{:.n}"`),
    /// if no constraints are given they default to 5 items (total, including sub-streams), or 20
    /// characters for strings. If an error happens during reading the stream, or a non-character
    /// value is encountered in a string, it is represented as `"<!>"` and no further output is
    /// printed.
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.format_int(f, &Default::default(), &Default::default())
    }
}

impl Debug for Item {
    /// This works exactly the same as the `Display` trait, but prepends the type of the item to
    /// its value.
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
            Stream(s) | String(s) => s.describe_prec(prec)
        }
    }
}

// `PartialEq::eq()` must be used with caution because if asked of two infinite streams it
// will never return. This implementation is appropriate for tests only where predictable streams
// are being compared, and as such is only available for #[cfg(test)]. For user-facing use
// `Item::try_eq()` which can handle interruption.
#[cfg(test)]
impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        use Item::*;
        match (self, other) {
            (Number(x1), Number(x2)) => x1 == x2,
            (Bool(x1), Bool(x2)) => x1 == x2,
            (Char(x1), Char(x2)) => x1 == x2,
            (Stream(x1), Stream(x2)) | (String(x1), String(x2)) => {
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
            Stream(s) => Stream(s.clone_box()),
            String(s) => String(s.clone_box())
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
