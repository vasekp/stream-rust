use crate::base::*;

use std::fmt::Formatter;
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
    /// Consume this `Stream` and turn it into a one-time standalone [`SIterator`].
    ///
    /// To avoid a large amount of code duplication, a `Stream` only needs to implement
    /// [`iter()`](Stream::iter), which takes it by reference. This metod creates
    /// a self-referential struct which holds the owned instance for the duration
    /// of its lifetime.
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self: Box<Self>) -> OwnedStreamIter {
        OwnedStreamIter::from(self)
    }

    pub(crate) fn listout(&self) -> Result<Vec<Item>, StreamError> {
        let mut vec = Vec::new();
        match self.length() {
            Length::Exact(len) | Length::AtMost(len) => {
                if let Some(len) = len.to_usize() {
                    vec.reserve(len);
                }
            },
            _ => ()
        };
        for res in self.iter() {
            check_stop!();
            vec.push(res?);
        }
        Ok(vec)
    }

    pub(crate) fn writeout_stream(&self, f: &mut Formatter<'_>, count: &Cell<usize>, error: &Cell<Option<StreamError>>)
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

    pub(crate) fn writeout_string(&self, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
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

    /// Create an iterator adapted over `self.iter()` extracting [`Char`] values from [`Item`] and
    /// failing for other types.
    pub fn string_iter(&self) -> StringIterator<'_> {
        StringIterator::new(self)
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

impl From<BoxedStream> for Box<dyn Stream> {
    fn from(val: BoxedStream) -> Self {
        val.0
    }
}

impl Describe for BoxedStream {
    fn describe_inner(&self, prec: u32, env: &Rc<Env>) -> String {
        self.0.describe_inner(prec, env)
    }
}

#[derive(Clone, Copy)]
pub(crate) struct EmptyStream;

impl Stream for EmptyStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(std::iter::empty())
    }
}

impl Describe for EmptyStream {
    fn describe_inner(&self, _prec: u32, _env: &Rc<Env>) -> String {
        "[]".into()
    }
}

pub struct OwnedStreamIter {
    iter: Box<dyn SIterator>,
    _stream: std::pin::Pin<Box<dyn Stream>>
}

impl From<Box<dyn Stream>> for OwnedStreamIter {
    fn from(stm: Box<dyn Stream>) -> Self {
        let pin = Box::into_pin(stm);
        let iter = unsafe { std::mem::transmute::<&dyn Stream, &(dyn Stream + 'static)>(&*pin) }.iter();
        OwnedStreamIter { iter, _stream: pin }
    }
}

impl std::ops::Deref for OwnedStreamIter {
    type Target = dyn SIterator;

    fn deref(&self) -> &Self::Target {
        self.iter.deref()
    }
}

impl std::ops::DerefMut for OwnedStreamIter {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.iter.deref_mut()
    }
}
