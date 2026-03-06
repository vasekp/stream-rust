use crate::base::*;

/// The iterator trait returned by [`Stream::iter()`]. Every call to `next()` returns either:
/// - `Some(Ok(item))`: any [`Item`] ready for direct consumption,
/// - `Some(Err(err))`: an error occurred at some point,
/// - `None`: the stream ended.
///
/// `next()` should not be called any more after *either* of the two latter conditions.
/// The iterators are not required to be fused and errors are not meant to be recoverable or
/// replicable, so the behaviour of doing so is undefined.
pub trait SIterator<I = Item> {
    fn next(&mut self) -> Result<Option<I>, StreamError>;

    /// Returns the number of items remaining in the iterator, if it can be deduced from its
    /// current state. The return value must be consistent with the actual behaviour of the stream.
    ///
    /// The default implementation relies on [`Iterator::size_hint()`] to return one of 
    /// [`Length::Exact`], [`Length::AtMost`], or [`Length::Unknown`].
    fn len_remain(&self) -> Length; // TODO docstring

    /// Inspired by (at the moment, experimental) `Iterator::advance_by()`, advances the iterator
    /// by `n` elements.
    ///
    /// The return value is `Ok(None)` if `n` elements were skipped. If the iterator finishes
    /// early, the result is `Ok(Some(k))`, where `k` is the number of remaining elements. This is
    /// important to know when multiple iterators are chained. Calling `next()` after this
    /// condition, or after an `Err` is returned, is undefined behaviour.
    ///
    /// It is possible to return `Ok(Some(0))` to indicate that all elements have been skipped
    /// without remainder, but that `next()` should not be called again. If `None` is returned
    /// in such case, `next()` may be called but must return `None` for consistency.
    ///
    /// The default implementation calls `next()` an appropriate number of times, and thus is
    /// reasonably usable only for small values of `n`, except when `n` is found to exceed the
    /// value given by [`SIterator::len_remain()`].
    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if let Length::Exact(len) = self.len_remain()
            && n > len {
                return Ok(Some(n - &len));
            }
        while !n.is_zero() {
            check_stop!();
            match self.next()? {
                Some(_) => (),
                None => return Ok(Some(n))
            }
            n -= 1;
        }
        Ok(None)
    }
}

impl<I> dyn SIterator<I> + '_ {
    pub fn transpose(self: Box<Self>) -> impl Iterator<Item = Result<I, StreamError>> {
        RustIterator(self)
    }
}

struct RustIterator<'a, I>(Box<dyn SIterator<I> + 'a>);

impl<I> Iterator for RustIterator<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().transpose()
    }
}

impl<I, T, U, V> SIterator<I> for std::iter::Map<T, U>
where T: Iterator<Item = V>,
      U: FnMut(V) -> Result<I, StreamError>
{
    fn next(&mut self) -> Result<Option<I>, StreamError> {
        Iterator::next(self).transpose()
    }

    fn len_remain(&self) -> Length {
        match self.size_hint() {
            (lo, Some(hi)) if lo == hi => Length::Exact(lo.into()),
            (_, Some(hi)) => Length::AtMost(hi.into()),
            _ => Length::Unknown
        }
    }
}

impl<I> SIterator<I> for std::iter::Once<Result<I, StreamError>> {
    fn next(&mut self) -> Result<Option<I>, StreamError> {
        Iterator::next(self).transpose()
    }

    fn len_remain(&self) -> Length {
        match self.size_hint() {
            (lo, Some(hi)) if lo == hi => Length::Exact(lo.into()),
            _ => unreachable!("std::iter::once size_hint")
        }
    }
}

impl<I> SIterator<I> for std::iter::Empty<Result<I, StreamError>> {
    fn next(&mut self) -> Result<Option<I>, StreamError> {
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        Length::Exact(UNumber::zero())
    }
}

impl<I: Clone> SIterator<I> for std::iter::Repeat<I> {
    fn next(&mut self) -> Result<Option<I>, StreamError> {
        Ok(Iterator::next(self))
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, _n: UNumber) -> Result<Option<UNumber>, StreamError> {
        Ok(None)
    }
}

impl<I: Clone, F> SIterator<I> for std::iter::RepeatWith<F>
where F: FnMut() -> Result<I, StreamError>
{
    fn next(&mut self) -> Result<Option<I>, StreamError> {
        Iterator::next(self).transpose()
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, _n: UNumber) -> Result<Option<UNumber>, StreamError> {
        Ok(None)
    }
}

pub(crate) struct SMap<'node, I1: ItemType, I2, F: Fn(I1) -> Result<I2, StreamError>> {
    _parent: Rc<dyn Stream<I1>>,
    source: Box<dyn SIterator<I1> + 'node>,
    func: F
}

impl<'node, I1: ItemType, I2, F: Fn(I1) -> Result<I2, StreamError>> SMap<'node, I1, I2, F> {
    pub(crate) fn new(stream: &'node Rc<dyn Stream<I1>>, func: F) -> Self {
        SMap{_parent: Rc::clone(stream), source: stream.iter(), func}
    }
}

impl<I1: ItemType, I2, F: Fn(I1) -> Result<I2, StreamError>> SIterator<I2> for SMap<'_, I1, I2, F> {
    fn next(&mut self) -> Result<Option<I2>, StreamError> {
        self.source.next()?
            .map(&self.func)
            .transpose()
    }

    fn len_remain(&self) -> Length {
        self.source.len_remain()
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.source.advance(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_iters() {
        let mut iter = std::iter::empty::<Result<Item, StreamError>>();
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::empty::<Result<Item, StreamError>>();
        assert_eq!(iter.advance(UNumber::zero()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::empty::<Result<Item, StreamError>>();
        assert_eq!(iter.advance(UNumber::one()), Ok(Some(UNumber::one())));

        let mut iter = std::iter::once(Ok(Item::default()));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::one()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::once(Ok(Item::default()));
        assert_eq!(iter.advance(UNumber::one()), Ok(None));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::once(Ok(Item::default()));
        assert_eq!(iter.advance(2u32.into()), Ok(Some(UNumber::one())));

        let mut iter = std::iter::repeat(Item::default());
        assert_eq!(iter.len_remain(), Length::Infinite);
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Infinite);
        assert_eq!(iter.advance(100u32.into()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Infinite);

        let mut iter = std::iter::repeat_with(|| Ok(Item::default()));
        assert_eq!(iter.len_remain(), Length::Infinite);
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Infinite);
        assert_eq!(iter.advance(100u32.into()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Infinite);

        let mut iter = [Item::default(), Item::default()].into_iter().map(Result::Ok);
        assert_eq!(iter.len_remain(), Length::Exact(2u32.into()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::one()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = [Item::default(), Item::default()].into_iter().map(Result::Ok);
        assert_eq!(iter.advance(UNumber::one()), Ok(None));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::one()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(Item::default())));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = [Item::default(), Item::default()].into_iter().map(Result::Ok);
        assert_eq!(iter.advance(2u32.into()), Ok(None));
        assert_eq!(iter.len_remain(), Length::Exact(UNumber::zero()));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = [Item::default(), Item::default()].into_iter().map(Result::Ok);
        assert_eq!(iter.advance(3u32.into()), Ok(Some(UNumber::one())));
    }
}
