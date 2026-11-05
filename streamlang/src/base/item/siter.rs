use crate::base::*;

/// The iterator trait returned by [`Stream::iter()`](Stream#method.iter). Every call to `next()`
/// returns either:
/// - `Ok(Some(item))`: an item of type `I` ([`Item`] or [`Char`]) ready for direct consumption,
/// - `Ok(None)`: the stream ended,
/// - `Err(err)`: an error occurred at some point.
///
/// `next()` should not be called any more after *either* of the two latter conditions.
/// The iterators are not required to be fused and errors are not meant to be recoverable or
/// replicable, so the behaviour of doing so is unspecified.
///
/// Within the crate, iterators are usually written against `PreIterator` and converted using its
/// `wrap()`, which attaches the originating stream to the backtrace of every error.
pub trait SIterator<I = Item> {
    fn next(&mut self) -> SResult<Option<I>>;

    /// Inspired by (at the moment, experimental) `Iterator::advance_by()`, advances the iterator
    /// by `n` elements.
    ///
    /// The return value is `Ok(None)` if `n` elements were skipped. If the iterator finishes
    /// early, the result is `Ok(Some(k))`, where `k` is the number of remaining elements. This is
    /// important to know when multiple iterators are chained. Calling `next()` after this
    /// condition, or after an `Err` is returned, is unspecified behaviour.
    ///
    /// It is possible to return `Ok(Some(0))` to indicate that all elements have been skipped
    /// without remainder, but that `next()` should not be called again. If `Ok(None)` is returned
    /// in such case, `next()` may be called but must return `Ok(None)` for consistency.
    ///
    /// # Contract
    ///
    /// Position and remainder must be exact: `advance(n)` followed by `next()` must yield the same
    /// item as `n + 1` calls to `next()` would, and a returned remainder must be the true number
    /// of missing items.
    ///
    /// Errors need not be: an implementation may skip the per-item computation `next()` performs,
    /// so an error at a skipped position goes unreported. This is deliberate, it is what keeps
    /// indexing into lazily computed streams O(1), and the language documents it in `[]` and
    /// `skip`. An item that the implementation does inspect while skipping, however, must be
    /// treated exactly as `next()` would treat it, errors included, so that the position stays
    /// exact.
    ///
    /// The default implementation calls `next()` an appropriate number of times, checking for an
    /// interrupt request in between, and thus is reasonably usable only for small values of `n`.
    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut n = n.clone();
        while !n.is_zero() {
            check_stop!();
            match self.next()? {
                Some(_) => (),
                None => return Ok(Some(n)),
            }
            n -= 1;
        }
        Ok(None)
    }
}

impl<I> dyn SIterator<I> + '_ {
    pub fn transposed(&mut self) -> impl Iterator<Item = SResult<I>> {
        Transposed(self)
    }
}

struct Transposed<'a, I>(&'a mut dyn SIterator<I>);

impl<I> Iterator for Transposed<'_, I> {
    type Item = SResult<I>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().transpose()
    }
}

/// The trait against which concrete iterators are written. It mirrors [`SIterator`], with the
/// same contract for `next()` and `advance()`, but adds [`origin()`](PreIterator::origin), the
/// stream the iterator was created from. [`wrap()`](PreIterator::wrap) turns the implementor into
/// a boxed [`SIterator`] which pushes that stream onto the backtrace of every error, so that the
/// implementations themselves don't need to care about blame.
pub(crate) trait PreIterator<I: ItemType = Item>: Sized + 'static {
    fn next(&mut self) -> SResult<Option<I>>;

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut n = n.clone();
        while !n.is_zero() {
            check_stop!();
            match self.next()? {
                Some(_) => (),
                None => return Ok(Some(n)),
            }
            n -= 1;
        }
        Ok(None)
    }

    fn origin(&self) -> &Rc<impl Stream<I> + 'static>;

    fn get_blame(&self) -> Rc<dyn Stream<I>> {
        Rc::clone(self.origin()) as Rc<dyn Stream<I>>
    }

    fn wrap(self) -> Box<dyn SIterator<I>> {
        Box::new(WrappedIterator(self, std::marker::PhantomData))
    }
}

pub(crate) struct WrappedIterator<I: ItemType, T: PreIterator<I>>(T, std::marker::PhantomData<I>);

impl<I: ItemType, T: PreIterator<I>> SIterator<I> for WrappedIterator<I, T> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.0.next().map_err(|err| err.wrap(&self.0.get_blame()))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.0.advance(n).map_err(|err| err.wrap(&self.0.get_blame()))
    }
}

impl<I, T, U, V> SIterator<I> for std::iter::Map<T, U>
where
    T: Iterator<Item = V>,
    U: FnMut(V) -> SResult<I>,
{
    fn next(&mut self) -> SResult<Option<I>> {
        Iterator::next(self).transpose()
    }
}

impl<I> SIterator<I> for std::iter::Once<SResult<I>> {
    fn next(&mut self) -> SResult<Option<I>> {
        Iterator::next(self).transpose()
    }
}

impl<I> SIterator<I> for std::iter::Empty<SResult<I>> {
    fn next(&mut self) -> SResult<Option<I>> {
        Ok(None)
    }
}

impl<I: ItemType + Clone> SIterator<I> for std::iter::Repeat<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        Ok(Iterator::next(self))
    }

    fn advance(&mut self, _n: &UNumber) -> SResult<Option<UNumber>> {
        Ok(None)
    }
}

impl<I: Clone> SIterator<I> for std::iter::Repeat<SResult<I>> {
    fn next(&mut self) -> SResult<Option<I>> {
        Iterator::next(self).transpose()
    }

    fn advance(&mut self, _n: &UNumber) -> SResult<Option<UNumber>> {
        Ok(None)
    }
}

impl<I: Clone, F> SIterator<I> for std::iter::RepeatWith<F>
where F: FnMut() -> SResult<I>
{
    fn next(&mut self) -> SResult<Option<I>> {
        Iterator::next(self).transpose()
    }

    fn advance(&mut self, _n: &UNumber) -> SResult<Option<UNumber>> {
        Ok(None)
    }
}

pub(crate) struct Map<I1: ItemType, I2, F: Fn(I1) -> I2> {
    source: Box<dyn SIterator<I1>>,
    func: F,
}

impl<I1: ItemType, I2, F: Fn(I1) -> I2> Map<I1, I2, F> {
    pub(crate) fn new(stream: &Rc<dyn Stream<I1>>, func: F) -> Self {
        Map { source: stream.iter(), func }
    }
}

impl<I1: ItemType, I2, F: Fn(I1) -> I2> SIterator<I2> for Map<I1, I2, F> {
    fn next(&mut self) -> SResult<Option<I2>> {
        Ok(self.source.next()?.map(&self.func))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.source.advance(n)
    }
}

pub(crate) struct SMap<I1: ItemType, I2: ItemType, F: Fn(I1) -> SResult<I2>> {
    source: Box<dyn SIterator<I1>>,
    func: F,
    origin: Rc<dyn Stream<I2>>,
}

impl<I1: ItemType, I2: ItemType, F: Fn(I1) -> SResult<I2>> SMap<I1, I2, F> {
    pub(crate) fn new<T>(stream: &Rc<dyn Stream<I1>>, func: F, origin: &Rc<T>) -> Self
    where T: Stream<I2> + 'static {
        SMap { source: stream.iter(), func, origin: Rc::clone(origin) as Rc<dyn Stream<I2>> }
    }
}

impl<I1: ItemType, I2: ItemType, F: Fn(I1) -> SResult<I2>> SIterator<I2> for SMap<I1, I2, F> {
    fn next(&mut self) -> SResult<Option<I2>> {
        self.source
            .next()
            .map_err(|e| e.wrap(&self.origin))?
            .map(&self.func)
            .transpose()
            .map_err(|e| e.wrap(&self.origin))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.source.advance(n).map_err(|e| e.wrap(&self.origin))
    }
}

macro_rules! iter_try {
    ($expr:expr) => {
        match $expr? {
            Some(ret) => ret,
            None => return Ok(None),
        }
    };
}

pub(crate) use iter_try;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_iters() {
        let mut iter = std::iter::empty::<SResult<Item>>();
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::empty::<SResult<Item>>();
        assert_eq!(iter.advance(&UNumber::zero()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::empty::<SResult<Item>>();
        assert_eq!(iter.advance(&UNumber::one()), Ok(Some(UNumber::one())));

        let some_item = || Item::Number(Default::default());

        let mut iter = std::iter::once(Ok(some_item()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::once(Ok(some_item()));
        assert_eq!(iter.advance(&UNumber::one()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = std::iter::once(Ok(some_item()));
        assert_eq!(iter.advance(&2u32.into()), Ok(Some(UNumber::one())));

        let mut iter = std::iter::repeat(Ok(some_item()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));
        assert_eq!(iter.advance(&100u32.into()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));

        let mut iter = std::iter::repeat_with(|| Ok(some_item()));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));
        assert_eq!(iter.advance(&100u32.into()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));

        let mut iter = [some_item(), some_item()].into_iter().map(Result::Ok);
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = [some_item(), some_item()].into_iter().map(Result::Ok);
        assert_eq!(iter.advance(&UNumber::one()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(Some(some_item())));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = [some_item(), some_item()].into_iter().map(Result::Ok);
        assert_eq!(iter.advance(&2u32.into()), Ok(None));
        assert_eq!(SIterator::next(&mut iter), Ok(None));
        let mut iter = [some_item(), some_item()].into_iter().map(Result::Ok);
        assert_eq!(iter.advance(&3u32.into()), Ok(Some(UNumber::one())));
    }
}
