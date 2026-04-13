use crate::base::*;

pub(crate) struct RandomAccess<I: ItemType = Item> {
    source: Rc<dyn Stream<I>>,
    iter: Box<dyn SIterator<I>>,
    consumed: UNumber,
}

impl<I: ItemType> RandomAccess<I> {
    pub fn new(source: Rc<dyn Stream<I>>) -> Self {
        Self {
            iter: source.iter(),
            consumed: UNumber::zero(),
            source,
        }
    }
}

impl<I: ItemType> SIterator<I> for RandomAccess<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.consumed += 1;
        self.iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.consumed += n;
        self.iter.advance(n)
    }
}

impl<I: ItemType> RandomAccess<I> {
    pub(crate) fn move_to(&mut self, index: UNumber) -> SResult<Option<UNumber>> {
        if index < self.consumed {
            self.iter = self.source.iter();
            self.consumed = UNumber::zero();
        }
        if index == self.consumed { return Ok(None) }
        let ret = self.iter.advance(&(&index - &self.consumed));
        self.consumed = index;
        ret
    }

    pub(crate) fn nth_from_start(&mut self, index: UNumber) -> SResult<Option<I>> {
        if self.move_to(index)?.is_some() {
            return Ok(None);
        }
        self.next()
    }
}
