use crate::base::*;

/// An infinite stream returning consecutive numbers.
///
/// ```
/// use streamlang::base::Item;
/// use streamlang::ops::SeqStream;
/// let stream = SeqStream::construct(vec![]).unwrap();
/// assert_eq!(stream.to_string(), "[1, 2, 3, ...");
/// let stream = SeqStream::construct(vec![Item::new_imm(3)]).unwrap();
/// assert_eq!(stream.to_string(), "[3, 4, 5, ...");
/// let stream = SeqStream::construct(vec![Item::new_imm(1), Item::new_imm(3)]).unwrap();
/// assert_eq!(stream.to_string(), "[1, 4, 7, ...");
/// let stream = SeqStream::construct(vec![Item::new_imm(3), Item::new_imm(0)]).unwrap();
/// assert_eq!(stream.to_string(), "[3, 3, 3, ...");
/// ```
pub struct SeqStream {
    from: TNumber,
    step: TNumber
}

impl SeqStream {
    /// Constructs [`SeqStream`].
    ///
    /// Possible inputs:
    /// - []
    /// - [`start` (number)]
    /// - [`start` (number), `step` (number)].
    ///
    /// Default values for both `start` and `step` are 1.
    pub fn construct(ins: Vec<Item>) -> StreamResult<Item> {
        if ins.len() > 2 {
            return Err(StreamError())
        }
        let mut nums: Vec<TNumber> = vec![];
        for input in ins {
            nums.push(input.into_num()?)
        }
        let mut it = nums.into_iter();
        let from = it.next().unwrap_or(TNumber::from(1));
        let step = it.next().unwrap_or(TNumber::from(1));
        Ok(Item::new_stream(SeqStream{from, step}))
    }
}

impl TStream for SeqStream {
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>> {
        Box::new(SeqIterator::new(&self.from, &self.step))
    }
}

struct SeqIterator {
    value: TNumber,
    step: TNumber
}

impl SeqIterator {
    fn new(from: &TNumber, step: &TNumber) -> SeqIterator {
        SeqIterator{ value: from.clone(), step: step.clone() }
    }
}

impl Iterator for SeqIterator {
    type Item = StreamResult<Item>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = Item::new_imm(self.value.clone());
        self.value += &self.step;
        Some(Ok(res))
    }
}
