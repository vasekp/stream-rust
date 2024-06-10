use crate::base::*;
use num::{Zero, One, ToPrimitive, Signed};
use crate::session::Session;
use crate::base::Describe;
use std::marker::PhantomData;

/// An infinite stream returning consecutive numbers, `seq`.
///
/// Possible forms:
/// - `seq`
/// - `seq(start)`
/// - `seq(start, step)`.
///
/// Default values for both `start` and `step` are 1.
///
/// # Examples
/// ```
/// use streamlang::session::Session;
/// use streamlang::parser::parse;
/// let sess = Session::new();
/// let stream = sess.eval(parse("seq").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[1, 2, 3, ...");
/// let stream = sess.eval(parse("seq(3)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[3, 4, 5, ...");
/// let stream = sess.eval(parse("seq(1, 3)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[1, 4, 7, ...");
/// let stream = sess.eval(parse("seq(3, 0)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[3, 3, 3, ...");
/// ```
#[derive(Clone)]
pub struct Seq {
    from: Number,
    step: Number
}

struct SeqIter<'a> {
    value: Number,
    step: Number,
    phantom: PhantomData<&'a Number>
}

impl<'a> Stream<'a> for Seq {
    fn iter(&self) -> Box<dyn SIterator<'a> + 'a> {
        Box::new(SeqIter{
            value: self.from.clone(),
            step: self.step.clone(),
            phantom: PhantomData
        })
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Seq {
    fn construct<'a>(session: &'a Session, node: Node<'a>) -> Result<Item<'a>, StreamError<'a>> {
        node.check_args(false, 0..=2)?
            .eval_all(session)?
            .with(|node| {
                let mut nums = node.args.iter()
                    .map(|x| x.to_item()?.into_num());
                let (from, step) = match nums.len() {
                    0 => (Number::one(), Number::one()),
                    1 => (nums.next().unwrap()?, Number::one()),
                    2 => (nums.next().unwrap()?, nums.next().unwrap()?),
                    _ => unreachable!()
                };
                Ok(Item::new_stream(Seq{from, step}))
            })
    }
}

impl Describe for Seq {
    fn describe(&self) -> String {
        format!("seq({}, {})", self.from, self.step)
    }
}

impl<'a> Iterator for SeqIter<'a> {
    type Item = Result<Item<'a>, StreamError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = Item::new_number(self.value.clone());
        self.value += &self.step;
        Some(Ok(ret))
    }
}

impl<'a> SIterator<'a> for SeqIter<'a> {
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        debug_assert!(!n.is_negative());
        self.value += n * &self.step;
        Ok(())
    }
}

#[test]
fn test_seq() {
    use crate::parser::parse;
    let sess = Session::new();
    // in addition to doc tests
    let stream = sess.eval(parse("seq(0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[0, 1, 2, ...");
    let stream = sess.eval(parse("seq(2, 3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[2, 5, 8, ...");
    let stream = sess.eval(parse("seq(2, 0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[2, 2, 2, ...");
    let stream = sess.eval(parse("seq(2, -3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[2, -1, -4, ...");
}

#[test]
fn test_seq_skip() {
    use crate::parser::parse;
    let sess = Session::new();
    let stream = sess.eval(parse("seq(2, 3)[5]").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "14");
    let stream = sess.eval(parse("seq(2, 0)[5]").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "2");
}


/// A range of equidistant numbers, `range`.
///
/// Possible inputs:
/// - `range(end)`
/// - `range(start, end)`
/// - `range(start, end, step)`
///
/// Default values for both `start` and `step` are 1.
///
/// # Examples
/// ```
/// use streamlang::session::Session;
/// use streamlang::parser::parse;
/// let sess = Session::new();
/// let stream = sess.eval(parse("range(3)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[1, 2, 3]");
/// let stream = sess.eval(parse("range(0, 2)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[0, 1, 2]");
/// let stream = sess.eval(parse("range(3, 1, -1)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[3, 2, 1]");
/// ```
#[derive(Clone)]
pub struct Range {
    from: Number,
    to: Number,
    step: Number
}

struct RangeIter<'a> {
    value: Number,
    step: Number,
    stop: Number,
    phantom: PhantomData<&'a Number>
}

impl<'a> Stream<'a> for Range {
    fn iter(&self) -> Box<dyn SIterator<'a> + 'a> {
        Box::new(RangeIter{
            value: self.from.clone(),
            stop: self.to.clone(),
            step: self.step.clone(),
            phantom: PhantomData
        })
    }

    fn length(&self) -> Length {
        use Length::*;
        if (self.to > self.from && self.step.is_negative())
                || (self.to < self.from && self.step.is_positive()) {
            return Exact(Number::zero());
        }
        if self.step.is_zero() {
            Infinite
        } else {
            Exact(self.iter().len_remain().unwrap())
        }
    }
}

impl Range {
    fn construct<'a>(session: &'a Session, node: Node<'a>) -> Result<Item<'a>, StreamError<'a>> {
        node.check_args(false, 1..=3)?
            .eval_all(session)?
            .with(|node| {
                let mut nums = node.args.iter()
                    .map(|x| x.to_item()?.into_num());
                let (from, to, step) = match nums.len() {
                    1 => (Number::one(), nums.next().unwrap()?, Number::one()),
                    2 => (nums.next().unwrap()?, nums.next().unwrap()?, Number::one()),
                    3 => (nums.next().unwrap()?, nums.next().unwrap()?, nums.next().unwrap()?),
                    _ => unreachable!()
                };
                Ok(Item::new_stream(Range{from, to, step}))
            })
    }
}

impl Describe for Range {
    fn describe(&self) -> String {
        format!("range({}, {}, {})", self.from, self.to, self.step)
    }
}

impl<'a> Iterator for RangeIter<'a> {
    type Item = Result<Item<'a>, StreamError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.step.is_zero()
            || (self.step.is_positive() && self.value <= self.stop)
            || (self.step.is_negative() && self.value >= self.stop) {
                let ret = Item::new_number(self.value.clone());
                self.value += &self.step;
                Some(Ok(ret))
        } else {
            None
        }
    }
}

impl<'a> SIterator<'a> for RangeIter<'a> {
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        debug_assert!(!n.is_negative());
        if self.step.is_zero() {
            return Ok(())
        }
        let max = self.len_remain().unwrap();
        if n <= &max {
            self.value += n * &self.step;
            Ok(())
        } else {
            Err(n - &max)
        }
    }

    fn len_remain(&self) -> Option<Number> {
        match self.step.to_i32() {
            Some(1) => Some(&self.stop - &self.value + 1),
            Some(-1) => Some(&self.value - &self.stop + 1),
            Some(0) => None,
            _ => Some((&self.stop - &self.value) / &self.step + 1)
        }
    }
}

#[test]
fn test_range() {
    use crate::parser::parse;
    let sess = Session::new();
    let stream = sess.eval(parse("range(3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, 2, 3]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = sess.eval(parse("range(0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = sess.eval(parse("range(3, 3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[3]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = sess.eval(parse("range(3, 5)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[3, 4, 5]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = sess.eval(parse("range(5, 3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = sess.eval(parse("range(1, 10, 4)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, 5, 9]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = sess.eval(parse("range(1, 10, 10)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = sess.eval(parse("range(1, 10, 0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, 1, 1, ...");
    assert_eq!(stream.as_stream().unwrap().length(), Length::Infinite);
    let stream = sess.eval(parse("range(1, 10, -1)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = sess.eval(parse("range(1, -10, -3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, -2, -5, ...");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(4));
}

#[test]
fn test_range_skip() {
    use crate::parser::parse;
    let sess = Session::new();
    let mut it = sess.eval(parse("range(2, 7, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&2.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_number(6))));
    let mut it = sess.eval(parse("range(2, 7, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&3.into()), Ok(()));
    assert_eq!(it.next(), None);
    let mut it = sess.eval(parse("range(2, 7, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&4.into()), Err(1.into()));

    let mut it = sess.eval(parse("range(2, 8, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&3.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_number(8))));
    let mut it = sess.eval(parse("range(2, 8, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&4.into()), Ok(()));
    assert_eq!(it.next(), None);
    let mut it = sess.eval(parse("range(2, 8, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&5.into()), Err(1.into()));

    let mut it = sess.eval(parse("range(2, 8, 0)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&3.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_number(2))));
}

#[test]
fn range_test_neg_lengths() {
    use crate::parser::parse;
    let sess = Session::new();
    let stream = sess.eval(parse("range(10, 1, -2)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(5));
    let stream = sess.eval(parse("range(10, 1, -3)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(4));
    let stream = sess.eval(parse("range(10, 1, -4)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(3));
    let stream = sess.eval(parse("range(10, 1, -5)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(2));
    let stream = sess.eval(parse("range(10, 1, -9)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(2));
    let stream = sess.eval(parse("range(10, 1, -10)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(1));
    let stream = sess.eval(parse("range(10, 1, -11)").unwrap()).unwrap().into_stream().unwrap();
    assert_eq!(stream.length(), Length::from(1));
}


pub(crate) fn init(session: &mut Session) {
    session.register_symbol("seq", Seq::construct);
    session.register_symbol("range", Range::construct);
}
