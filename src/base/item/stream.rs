use crate::base::*;
pub use crate::utils::TriState;

use std::fmt::{Display, Formatter};
use std::cell::Cell;

use dyn_clone::DynClone;

/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait Stream: DynClone + Describe {
    /// Create an [`SIterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    ///
    /// This method does not return a `Result` and thus can't fail. Implementors may return 
    /// a `std::iter::once(Err(...))` to report errors that may happen during constructing the 
    /// iterator.
    #[must_use]
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node>;

    /// An indication whether this stream should be treated as a string. The implementation should
    /// only return [`TriState::True`] if it can be sure that the iterator will produce a stream of 
    /// [`Char`]s. If so, this affects the behaviour of [`dyn Stream::writeout()`](trait.Stream.html#impl-dyn+Stream).
    ///
    /// The default implementation returns [`TriState::False`].
    // TODO link do <dyn Stream>::writeout unsupported?
    fn is_string(&self) -> TriState {
        TriState::False
    }

    /// Returns the length of this stream, in as much information as available *without* consuming
    /// the entire stream. See [`Length`] for the possible return values. The return value must be 
    /// consistent with the actual behaviour of the stream.
    ///
    /// The default implementation forwards to [`SIterator::len_remain()`].
    fn length(&self) -> Length {
        self.iter().len_remain()
    }

    /// Checks for emptiness. The default implementation first tries to answer statically from
    /// looking at [`length()`](Stream::length). If the information is insufficient, constructs the
    /// iterator and tries answering using [`SIterator::len_remain()`]. As a last resort, the 
    /// iterator is consumed.
    ///
    /// This function can't return an error. If the first call to `iter().next()` produces an
    /// error, i.e. `Some(Err(_))`, it's reported that the stream is nonempty.
    fn is_empty(&self) -> bool {
        match self.length() {
            Length::Exact(len) | Length::AtMost(len) if len.is_zero() => true,
            Length::Exact(_) | Length::Infinite => false,
            _ => {
                let mut iter = self.iter();
                match iter.len_remain() {
                    Length::Exact(len) | Length::AtMost(len) if len.is_zero() => true,
                    Length::Exact(_) | Length::Infinite => false,
                    _ => iter.next().is_none()
                }
            }
        }
    }
}

impl dyn Stream {
    /// Write the contents of the stream (i.e., the items returned by its iterator) in a
    /// human-readable form. This is called by the [`Display`] trait. The formatter may specify a
    /// maximum number of items (using `{:n}`) or maximum width in characters (using `"{:.n}"`),
    /// if no constraints are given they default to 5 items. If an error happens during reading the
    /// stream, it is represented as `"<!>"`.
    ///
    /// If this is `Stream` represents a string, as expressed by its [`Stream::is_string()`]
    /// method, the formatting follows that of a string, including character escapes. If no length
    /// is given, up to 20 characters are printed. Any value returned by the iterator which is not
    /// a [`Char`] is treated as a reading error.
    pub fn writeout(&self, f: &mut Formatter<'_>, count: &Cell<usize>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        if self.is_string().is_true() {
            self.writeout_string(f, error)
        } else {
            self.writeout_stream(f, count, error)
        }
    }

    fn writeout_stream(&self, f: &mut Formatter<'_>, count: &Cell<usize>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        let mut iter = self.iter();
        let (prec, max) = match (f.precision(), f.width()) {
            (Some(prec), width) => (Some(prec), width),
            (None, Some(width)) => (None, Some(width)),
            (None, None) => (None, Some(5))
        };
        let max = max.map(|x| x + 1);
        let mut s = String::new();
        let mut comma = false;
        s.push('[');
        if prec.is_some_and(|prec| prec < 4) {
            s += "...";
            return write!(f, "{}", s);
        }
        'a: {
            while prec.is_none_or(|prec| s.len() < prec) && max.is_none_or(|max| count.get() < max) {
                match iter.next() {
                    None => {
                        s.push(']');
                        break 'a;
                    },
                    Some(Ok(item)) => {
                        let plen = s.len();
                        if comma {
                            s += ", ";
                        }
                        let (string, cnt, err) = item.format(max.map(|max| max - count.get() - 1), prec.map(|prec| prec - plen));
                        count.set(count.get() + cnt);
                        s += &string;
                        if err.is_some() {
                            error.set(err);
                            break 'a;
                        }
                        comma = true;
                    },
                    Some(Err(err)) => {
                        if comma {
                            s += ", ";
                        }
                        s += "<!>";
                        error.set(Some(err));
                        break 'a;
                    }
                };
            }
            if iter.next().is_none() {
                s.push(']');
            } else {
                s += if comma { ", ..." } else { "..." };
                if max.is_some_and(|max| max == count.get()) { s.push(']'); }
            }
        }
        match prec {
            Some(prec) if prec < s.len() => write!(f, "{:.*}...", prec - 3, s),
            _ => write!(f, "{}", s)
        }
    }

    fn writeout_string(&self, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        let mut iter = self.string_iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (Some(std::cmp::max(prec, 4)), None),
            None => (None, Some(20))
        };
        let mut s = String::new();
        let mut i = 0;
        s.push('"');
        'a: {
            while prec.is_none_or(|prec| s.len() < prec) && max.is_none_or(|max| i < max) {
                if let Some(next) = iter.next() {
                    match next {
                        Ok(ch) => s += &format!("{ch:#}"),
                        Err(err) => {
                            s += "<!>";
                            error.set(Some(err));
                            break 'a;
                        }
                    }
                } else {
                    s.push('"');
                    break 'a;
                }
                i += 1;
            }
            s += match iter.next() {
                None => "\"",
                Some(_) => "..."
            };
        }
        match prec {
            Some(prec) if prec < s.len() => write!(f, "{:.*}...", prec - 3, s),
            _ => write!(f, "{}", s)
        }
    }

    pub(crate) fn clone_box(&self) -> Box<dyn Stream> {
        dyn_clone::clone_box(self)
    }

    pub(crate) fn to_item(&self) -> Item {
        Item::Stream(self.clone_box())
    }

    pub(crate) fn to_expr(&self) -> Expr {
        Expr::Imm(self.to_item())
    }

    /// Create an iterator adapted over `self.iter()` extracting [`Char`] values from [`Item`] and
    /// failing for other types. Suitable for iterating over strings ([`Stream::is_string()`]` == `[`TriState::True`]).
    pub fn string_iter(&self) -> StringIterator<'_> {
        StringIterator::new(self)
    }
}

impl Display for Box<dyn Stream> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.writeout(f, &Default::default(), &Default::default())
    }
}


pub(crate) struct BoxedStream(Box<dyn Stream>);

impl Clone for BoxedStream {
    fn clone(&self) -> Self {
        Self(self.0.clone_box())
    }
}

impl std::ops::Deref for BoxedStream {
    type Target = dyn Stream;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl From<Box<dyn Stream>> for BoxedStream {
    fn from(val: Box<dyn Stream>) -> Self {
        BoxedStream(val)
    }
}

impl Describe for BoxedStream {
    fn describe_prec(&self, prec: u32) -> String {
        self.0.describe_prec(prec)
    }
}

#[derive(Clone)]
pub(crate) struct EmptyStream();

impl Stream for EmptyStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(std::iter::empty())
    }
}

impl Describe for EmptyStream {
    fn describe_prec(&self, _: u32) -> String {
        "[]".into()
    }
}


#[derive(Clone)]
pub struct EmptyString();

impl Stream for EmptyString {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(std::iter::empty())
    }

    fn is_string(&self) -> TriState {
        TriState::True
    }
}

impl Describe for EmptyString {
    fn describe_prec(&self, _: u32) -> String {
        "\"\"".into()
    }
}
