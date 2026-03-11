use crate::base::*;

use std::fmt::Formatter;
use std::cell::Cell;

/// The common trait for [`Stream`] [`Item`]s. Represents a stream of other [`Item`]s. Internally,
/// types implementing this trait need to hold enough information to produce a reconstructible
/// [`Iterator`].
pub trait Stream<I = Item>: Describe {
    /// Create an [`SIterator`] of this stream. Every instance of the iterator must produce the same
    /// values.
    ///
    /// This method does not return a `Result` and thus can't fail. Implementors may return 
    /// a `std::iter::once(Err(...))` to report errors that may happen during constructing the 
    /// iterator.
    // TODO docstring
    fn iter(&self) -> SResult<Box<dyn SIterator<I> + '_>>;

    /// Returns the length of this stream, in as much information as available *without* consuming
    /// the entire stream. See [`Length`] for the possible return values. The return value must be 
    /// consistent with the actual behaviour of the stream.
    ///
    /// The default implementation forwards to [`SIterator::len_remain()`].
    fn len(&self) -> Length;

    /// Checks for emptiness. The default implementation first tries to answer statically from
    /// looking at [`len()`](Stream::len). If the information is insufficient, constructs the
    /// iterator and tries answering using [`SIterator::len_remain()`]. As a last resort, the
    /// iterator is consumed. This usually does not need to be overridden.
    ///
    /// This function can't return an error. If the first call to `iter().next()` produces an
    /// error, i.e. `Some(Err(_))`, it's reported that the stream is nonempty.
    fn is_empty(&self) -> SResult<bool> {
        Ok(match self.len() {
            Length::Exact(len) | Length::AtMost(len) if len.is_zero() => true,
            Length::Exact(_) | Length::Infinite => false,
            _ => {
                let mut iter = self.iter()?;
                match iter.len_remain() {
                    Length::Exact(len) | Length::AtMost(len) if len.is_zero() => true,
                    Length::Exact(_) | Length::Infinite => false,
                    _ => iter.next()?.is_none()
                }
            }
        })
    }
}

impl<I: ItemType> dyn Stream<I> {
    /// Consume this `Stream` and turn it into a one-time standalone [`SIterator`].
    ///
    /// To avoid a large amount of code duplication, a `Stream` only needs to implement
    /// [`iter()`](Stream::iter), which takes it by reference. This metod creates
    /// a self-referential struct which holds the owned instance for the duration
    /// of its lifetime.
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self: Rc<Self>) -> OwnedStreamIter<I> {
        OwnedStreamIter::from(self)
    }

    pub fn iter(self: &Rc<Self>) -> Box<dyn SIterator<I> + '_> {
        match (**self).iter() {
            Ok(iter) => Box::new(WrappedIter{iter, parent: self}),
            Err(err) => Box::new(std::iter::once(Err(err.wrap(self))))
        }
    }

    pub(crate) fn listout(self: &Rc<Self>) -> SResult<Vec<I>> {
        I::listout(self)
    }

    pub(crate) fn listout_check_nonempty(self: &Rc<Self>) -> SResult<Vec<I>> {
        let vec = I::listout(self)?;
        if !vec.is_empty() { Ok(vec) }
        else { Err(StreamError::with_expr("can't be empty", self)) }
    }

    pub(crate) fn try_count(self: &Rc<Self>) -> SResult<UNumber> {
        match self.len() {
            Length::Exact(len) => Ok(len),
            Length::Infinite => Err(StreamError::with_expr("stream is infinite", self)),
            _ => {
                let mut ret: usize = 0;
                let mut it = self.iter();
                while it.next()?.is_some() {
                    check_stop!();
                    ret += 1;
                }
                Ok(ret.into())
            }
        }
    }

    pub(crate) fn map_iter<'node, I2: 'static, F: Fn(I) -> SResult<I2> + 'node>(self: &'node Rc<Self>, func: F) -> Box<dyn SIterator<I2> + 'node> {
        Box::new(SMap::new(self, func))
    }
}

impl dyn Stream<Item> {
    pub(crate) fn listout_impl(self: &Rc<Self>) -> SResult<Vec<Item>> {
        let mut vec = Vec::new();
        match &self.len() {
            lobj @ (Length::Exact(len) | Length::AtMost(len)) => {
                if let Ok(len) = len.try_into() {
                    vec.reserve(len);
                } else if matches!(lobj, Length::Exact(_)) {
                    return Err(StreamError::with_expr("stream is too long", self));
                }
            },
            Length::Infinite => return Err(StreamError::with_expr("stream is infinite", self)),
            _ => ()
        };
        for item in self.iter().transposed() {
            check_stop!();
            vec.push(item?);
        }
        Ok(vec)
    }

    pub(crate) fn writeout(self: &Rc<Self>, f: &mut Formatter<'_>, count: &Cell<usize>, error: &Cell<Option<StreamError>>)
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
                    Ok(None) => {
                        s.push(']');
                        break 'a;
                    },
                    Ok(Some(item)) => {
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
                    Err(err) => {
                        if comma {
                            s += ", ";
                        }
                        s += "<!>";
                        error.set(Some(err));
                        break 'a;
                    }
                };
            }
            if iter.next().transpose().is_none() {
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
}

impl dyn Stream<Char> {
    pub(crate) fn listout_impl(self: &Rc<Self>) -> SResult<Vec<Char>> {
        let mut vec = Vec::new();
        match &self.len() {
            lobj @ (Length::Exact(len) | Length::AtMost(len)) => {
                if let Ok(len) = len.try_into() {
                    vec.reserve(len);
                } else if matches!(lobj, Length::Exact(_)) {
                    return Err(StreamError::with_expr("string is too long", self));
                }
            },
            Length::Infinite => return Err(StreamError::with_expr("string is infinite", self)),
            _ => ()
        };
        for ch in self.iter().transposed() {
            check_stop!();
            vec.push(ch?);
        }
        Ok(vec)
    }

    pub(crate) fn writeout(self: &Rc<Self>, f: &mut Formatter<'_>, error: &Cell<Option<StreamError>>)
        -> std::fmt::Result
    {
        let mut iter = self.iter();
        let (prec, max) = match f.precision() {
            Some(prec) => (Some(std::cmp::max(prec, 4)), None),
            None => (None, Some(20))
        };
        let mut s = String::new();
        let mut i = 0;
        s.push('"');
        'a: {
            while prec.is_none_or(|prec| s.len() < prec) && max.is_none_or(|max| i < max) {
                match iter.next() {
                    Ok(Some(ch)) => s += &format!("{ch:#}"),
                    Ok(None) => {
                        s.push('"');
                        break 'a;
                    },
                    Err(err) => {
                        s += "<!>";
                        error.set(Some(err));
                        break 'a;
                    }
                }
                i += 1;
            }
            s += match iter.next().transpose() {
                None => "\"",
                Some(_) => "..."
            };
        }
        match prec {
            Some(prec) if prec < s.len() => write!(f, "{:.*}...", prec - 3, s),
            _ => write!(f, "{}", s)
        }
    }
}

impl<I: ItemType> Describe for Rc<dyn Stream<I>> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        (**self).describe_inner(prec, env)
    }
}


#[derive(Clone, Copy)]
pub(crate) struct EmptyStream;

impl EmptyStream {
    pub(crate) fn new_rc() -> Rc<dyn Stream> {
        EMPTY_STREAM.with(Rc::clone)
    }
}

impl Stream<Item> for EmptyStream {
    fn iter(&self) -> SResult<Box<dyn SIterator<Item> + '_>> {
        Ok(Box::new(std::iter::empty()))
    }

    fn len(&self) -> Length { Length::Exact(UNumber::zero()) }
}

impl Describe for EmptyStream {
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        "[]".into()
    }
}

#[derive(Clone, Copy)]
pub(crate) struct EmptyString;

impl EmptyString {
    pub(crate) fn new_rc() -> Rc<dyn Stream<Char>> {
        EMPTY_STRING.with(Rc::clone)
    }
}

impl EmptyString {
    pub fn iter(&self) -> Box<dyn SIterator<Char> + '_> {
        Box::new(std::iter::empty())
    }
}

impl Stream<Char> for EmptyString {
    fn iter(&self) -> SResult<Box<dyn SIterator<Char> + '_>> {
        Ok(self.iter())
    }

    fn len(&self) -> Length { Length::Exact(UNumber::zero()) }
}

impl Describe for EmptyString {
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        "\"\"".into()
    }
}

thread_local! {
    static EMPTY_STREAM: Rc<EmptyStream> = Rc::new(EmptyStream);
    static EMPTY_STRING: Rc<EmptyString> = Rc::new(EmptyString);
}

pub struct OwnedStreamIter<I = Item> {
    iter: Box<dyn SIterator<I>>,
    stream: Rc<dyn Stream<I>>,
}

impl<I: ItemType> From<Rc<dyn Stream<I>>> for OwnedStreamIter<I> {
    fn from(stm: Rc<dyn Stream<I>>) -> Self {
        let iter = match unsafe { &*Rc::as_ptr(&stm) as &'static dyn Stream<I> }.iter() {
            Ok(iter) => iter,
            Err(err) => Box::new(std::iter::once(Err(err.wrap(&stm)))),
        };
        OwnedStreamIter { iter, stream: stm }
    }
}

impl<I: ItemType> SIterator<I> for OwnedStreamIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.iter.next()
            .map_err(|err| err.wrap(&self.stream))
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
            .map_err(|err| err.wrap(&self.stream))
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}

struct WrappedIter<'node, I: ItemType> {
    iter: Box<dyn SIterator<I> + 'node>,
    parent: &'node Rc<dyn Stream<I>>,
}

impl<I: ItemType> SIterator<I> for WrappedIter<'_, I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.iter.next()
            .map_err(|err| err.wrap(self.parent))
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
            .map_err(|err| err.wrap(self.parent))
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}
