use crate::base::*;
use num::{Zero, ToPrimitive, Signed};


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
            return Err(StreamError(format!("expected 0 to 2 arguments, found {}", ins.len())))
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
        Box::new(num::iter::range_step_from(self.from.clone(), self.step.clone())
                 .map(|x| Ok(Item::new_imm(x))))
    }

    fn describe(&self) -> String {
        format!("seq({}, {})", self.from, self.step)
    }

    fn length(&self) -> Length {
        Infinite
    }
}


/// A range of equidistant numbers.
///
/// ```
/// use streamlang::base::Item;
/// use streamlang::ops::RangeStream;
/// let stream = RangeStream::construct(vec![Item::new_imm(3)]).unwrap();
/// assert_eq!(stream.to_string(), "[1, 2, 3]");
/// let stream = RangeStream::construct(vec![Item::new_imm(0), Item::new_imm(2)]).unwrap();
/// assert_eq!(stream.to_string(), "[0, 1, 2]");
/// let stream = RangeStream::construct(vec![Item::new_imm(3), Item::new_imm(1), Item::new_imm(-1)]).unwrap();
/// assert_eq!(stream.to_string(), "[3, 2, 1]");
/// ```
pub struct RangeStream {
    from: TNumber,
    to: TNumber,
    step: TNumber
}

impl RangeStream {
    /// Constructs [`RangeStream`].
    ///
    /// Possible inputs:
    /// - [`end` (number)]
    /// - [`start` (number), `end` (number)]
    /// - [`start` (number), `end` (number), `step` (number)].
    ///
    /// Default values for both `start` and `step` are 1.
    pub fn construct(ins: Vec<Item>) -> StreamResult<Item> {
        let mut nums: Vec<TNumber> = vec![];
        for input in ins {
            nums.push(input.into_num()?)
        }
        let len = nums.len();
        let mut it = nums.into_iter();
        let (from, to, step) = match len {
            1 => (TNumber::from(1), it.next().unwrap(), TNumber::from(1)), // TODO check nonneg
            2 => (it.next().unwrap(), it.next().unwrap(), TNumber::from(1)),
            3 => (it.next().unwrap(), it.next().unwrap(), it.next().unwrap()),
            _ => return Err(StreamError(format!("expected 1 to 3 arguments, found {}", len)))
        };
        Ok(Item::new_stream(RangeStream{from, to, step}))
    }
}

impl TStream for RangeStream {
    fn iter(&self) -> Box<dyn Iterator<Item = StreamResult<Item>>> {
        Box::new(num::iter::range_step_inclusive(self.from.clone(), self.to.clone(), self.step.clone())
                 .map(|x| Ok(Item::new_imm(x))))
    }

    fn describe(&self) -> String {
        format!("range({}, {}, {})", self.from, self.to, self.step)
    }

    fn length(&self) -> Length {
        if (self.to > self.from && self.step.is_negative())
                || (self.to < self.from && self.step.is_positive()) {
            return Exact(TNumber::zero());
        }
        match self.step.to_i32() {
            Some(1) => Exact(&self.to - &self.from + 1),
            Some(-1) => Exact(&self.from - &self.to + 1),
            Some(0) => Infinite,
            _ => Exact((&self.to - &self.from) / &self.step + 1),
        }
    }
}

#[test]
fn range_tests() {
    let stream = RangeStream::construct(vec![Item::new_imm(1), Item::new_imm(10), Item::new_imm(4)]).unwrap();
    assert_eq!(stream.to_string(), "[1, 5, 9]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = RangeStream::construct(vec![Item::new_imm(1), Item::new_imm(10), Item::new_imm(10)]).unwrap();
    assert_eq!(stream.to_string(), "[1]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = RangeStream::construct(vec![Item::new_imm(1), Item::new_imm(10), Item::new_imm(0)]).unwrap();
    assert_eq!(stream.to_string(), "[1, 1, 1, ...");
    assert_eq!(stream.as_stream().unwrap().length(), Infinite);
    let stream = RangeStream::construct(vec![Item::new_imm(1), Item::new_imm(10), Item::new_imm(-1)]).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = RangeStream::construct(vec![Item::new_imm(1), Item::new_imm(-10), Item::new_imm(-1)]).unwrap();
    assert_eq!(stream.to_string(), "[1, 0, -1, ...");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(12));
}

#[test]
fn range_test_neg_lengths() {
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-2)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(5));
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-3)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(4));
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-4)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-5)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(2));
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-9)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(2));
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-10)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = RangeStream::construct(vec![Item::new_imm(10), Item::new_imm(1), Item::new_imm(-11)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
}
