use crate::base::*;

pub(crate) struct RandomAccess<'stm, I: ItemType = Item> {
    source: &'stm dyn Stream<I>,
    iter: Box<dyn SIterator<I> + 'stm>,
    consumed: UNumber,
}

impl<'stm, I: ItemType> RandomAccess<'stm, I> {
    pub fn new(source: &'stm dyn Stream<I>) -> Self {
        Self {
            source,
            iter: source.iter(),
            consumed: UNumber::zero(),
        }
    }
}

impl<I: ItemType> Iterator for RandomAccess<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.consumed.inc();
        self.iter.next()
    }
}

impl<I: ItemType> SIterator<I> for RandomAccess<'_, I> {
    fn len_remain(&self) -> Length {
        Length::Unknown
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.consumed += &n;
        self.iter.advance(n)
    }
}

impl<I: ItemType> RandomAccess<'_, I> {
    pub(crate) fn move_to(&mut self, index: UNumber) -> Result<Option<UNumber>, StreamError> {
        if index < self.consumed {
            self.iter = self.source.iter();
            self.consumed = UNumber::zero();
        }
        if index == self.consumed { return Ok(None) }
        let ret = self.iter.advance(&index - &self.consumed);
        self.consumed = index;
        ret
    }

    pub(crate) fn nth_from_start(&mut self, index: UNumber) -> Option<Result<I, StreamError>> {
        if iter_try_expr!(self.move_to(index)).is_some() {
            return None;
        }
        self.next()
    }
}
