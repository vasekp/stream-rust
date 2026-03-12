use crate::base::*;

pub(crate) struct RandomAccess<'stm, I: ItemType = Item> {
    source: &'stm Rc<dyn Stream<I>>,
    iter: Box<dyn SIterator<I> + 'stm>,
    consumed: UNumber,
}

impl<'stm, I: ItemType> RandomAccess<'stm, I> {
    pub fn new(source: &'stm Rc<dyn Stream<I>>) -> Self {
        Self {
            source,
            iter: source.iter(),
            consumed: UNumber::zero(),
        }
    }
}

impl<I: ItemType> SIterator<I> for RandomAccess<'_, I> {
    fn next(&mut self) -> SResult<Option<I>> {
        self.consumed += 1;
        self.iter.next()
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.consumed += &n;
        self.iter.advance(n)
    }
}

impl<I: ItemType> RandomAccess<'_, I> {
    pub(crate) fn move_to(&mut self, index: UNumber) -> SResult<Option<UNumber>> {
        if index < self.consumed {
            self.iter = self.source.iter();
            self.consumed = UNumber::zero();
        }
        if index == self.consumed { return Ok(None) }
        let ret = self.iter.advance(&index - &self.consumed);
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
