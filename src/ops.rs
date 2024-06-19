use crate::base::*;
use num::{Zero, One, ToPrimitive, Signed};
use crate::base::Describe;

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
/// use streamlang::parser::parse;
/// assert_eq!(parse("seq").unwrap().eval().unwrap().to_string(), "[1, 2, 3, ...");
/// assert_eq!(parse("seq(3)").unwrap().eval().unwrap().to_string(), "[3, 4, 5, ...");
/// assert_eq!(parse("seq(1, 3)").unwrap().eval().unwrap().to_string(), "[1, 4, 7, ...");
/// assert_eq!(parse("seq(3, 0)").unwrap().eval().unwrap().to_string(), "[3, 3, 3, ...");
/// ```
#[derive(Clone)]
pub struct Seq {
    from: Number,
    step: Number,
    node: Node
}

struct SeqIter {
    value: Number,
    step: Number
}

impl Stream for Seq {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(SeqIter{
            value: self.from.clone(),
            step: self.step.clone()
        })
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Seq {
    fn construct(node: Node) -> Result<Item, StreamError> {
        let ((from, step), node) = node.check_args(false, 0..=2)?
            .eval_all()?
            .with_keep(|node| {
                let mut nums = node.args.iter()
                    .map(|x| x.to_item()?.into_num());
                let (from, step) = match nums.len() {
                    0 => (Number::one(), Number::one()),
                    1 => (nums.next().unwrap()?, Number::one()),
                    2 => (nums.next().unwrap()?, nums.next().unwrap()?),
                    _ => unreachable!()
                };
                Ok((from, step))
            })?;
        Ok(Item::new_stream(Seq{from, step, node}))
    }
}

impl Describe for Seq {
    fn describe(&self) -> String {
        self.node.describe()
    }
}

impl Iterator for SeqIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = Item::new_number(self.value.clone());
        self.value += &self.step;
        Some(Ok(ret))
    }
}

impl SIterator for SeqIter {
    fn skip_n(&mut self, n: Number) -> Result<(), Number> {
        debug_assert!(!n.is_negative());
        self.value += n * &self.step;
        Ok(())
    }
}

#[test]
fn test_seq() {
    use crate::parser::parse;
    // in addition to doc tests
    assert_eq!(parse("seq(0)").unwrap().eval().unwrap().to_string(), "[0, 1, 2, ...");
    assert_eq!(parse("seq(2, 3)").unwrap().eval().unwrap().to_string(), "[2, 5, 8, ...");
    assert_eq!(parse("seq(2, 0)").unwrap().eval().unwrap().to_string(), "[2, 2, 2, ...");
    assert_eq!(parse("seq(2, -3)").unwrap().eval().unwrap().to_string(), "[2, -1, -4, ...");

    assert_eq!(parse("seq(2, 3)[10^10]").unwrap().eval().unwrap().to_string(), "29999999999");
    assert_eq!(parse("seq(2, 0)[10^10]").unwrap().eval().unwrap().to_string(), "2");
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
/// use streamlang::parser::parse;
/// assert_eq!(parse("range(3)").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
/// assert_eq!(parse("range(0, 2)").unwrap().eval().unwrap().to_string(), "[0, 1, 2]");
/// assert_eq!(parse("range(3, 1, -1)").unwrap().eval().unwrap().to_string(), "[3, 2, 1]");
/// ```
#[derive(Clone)]
pub struct Range {
    from: Number,
    to: Number,
    step: Number,
    node: Node
}

struct RangeIter {
    value: Number,
    step: Number,
    stop: Number
}

impl Stream for Range {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(RangeIter{
            value: self.from.clone(),
            stop: self.to.clone(),
            step: self.step.clone()
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
    fn construct(node: Node) -> Result<Item, StreamError> {
        let ((from, to, step), node) = node.check_args(false, 1..=3)?
            .eval_all()?
            .with_keep(|node| {
                let mut nums = node.args.iter()
                    .map(|x| x.to_item()?.into_num());
                let (from, to, step) = match nums.len() {
                    1 => (Number::one(), nums.next().unwrap()?, Number::one()),
                    2 => (nums.next().unwrap()?, nums.next().unwrap()?, Number::one()),
                    3 => (nums.next().unwrap()?, nums.next().unwrap()?, nums.next().unwrap()?),
                    _ => unreachable!()
                };
                Ok((from, to, step))
            })?;
        Ok(Item::new_stream(Range{from, to, step, node}))
    }
}

impl Describe for Range {
    fn describe(&self) -> String {
        self.node.describe()
    }
}

impl Iterator for RangeIter {
    type Item = Result<Item, StreamError>;

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

impl SIterator for RangeIter {
    fn skip_n(&mut self, n: Number) -> Result<(), Number> {
        debug_assert!(!n.is_negative());
        if self.step.is_zero() {
            return Ok(())
        }
        let max = self.len_remain().unwrap();
        if n <= max {
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
    assert_eq!(parse("range(3)").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("range(0)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("range(3, 3)").unwrap().eval().unwrap().to_string(), "[3]");
    assert_eq!(parse("range(3, 5)").unwrap().eval().unwrap().to_string(), "[3, 4, 5]");
    assert_eq!(parse("range(5, 3)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("range(1, 10, 4)").unwrap().eval().unwrap().to_string(), "[1, 5, 9]");
    assert_eq!(parse("range(1, 10, 10)").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("range(1, 10, 0)").unwrap().eval().unwrap().to_string(), "[1, 1, 1, ...");
    assert_eq!(parse("range(1, 10, -1)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("range(1, -10, -3)").unwrap().eval().unwrap().to_string(), "[1, -2, -5, ...");
}

#[test]
fn test_range_skip() {
    use crate::parser::parse;
    let mut it = parse("range(2, 7, 2)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(2.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_number(6))));
    let mut it = parse("range(2, 7, 2)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(3.into()), Ok(()));
    assert_eq!(it.next(), None);
    let mut it = parse("range(2, 7, 2)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(4.into()), Err(1.into()));

    let mut it = parse("range(2, 8, 2)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(3.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_number(8))));
    let mut it = parse("range(2, 8, 2)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(4.into()), Ok(()));
    assert_eq!(it.next(), None);
    let mut it = parse("range(2, 8, 2)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(5.into()), Err(1.into()));

    let mut it = parse("range(2, 8, 0)").unwrap().eval().unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(3.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_number(2))));
}


fn construct_len(node: Node) -> Result<Item, StreamError> {
    let (length, node) = node.check_args(true, 0..=0)?
        .eval_all()?
        .with_keep(|node| {
            Ok(node.source.as_ref().unwrap()
                .as_item().unwrap()
                .as_stream()?
                .length())
        })?;
    use Length::*;
    match length {
        Exact(len) => Ok(Item::new_number(len)),
        AtMost(_) | UnknownFinite | Unknown => {
            let len = node.source.unwrap()
                .as_item().unwrap()
                .as_stream().unwrap()
                .iter().count();
            Ok(Item::new_number(len))
        },
        _ => Err(StreamError::new("stream is infinite", node))
    }
}

#[test]
fn test_range_length() {
    use crate::parser::parse;

    assert_eq!(parse("range(3).len").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("range(0).len").unwrap().eval().unwrap().to_string(), "0");
    assert_eq!(parse("range(3, 3).len").unwrap().eval().unwrap().to_string(), "1");
    assert_eq!(parse("range(3, 5).len").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("range(5, 3).len").unwrap().eval().unwrap().to_string(), "0");
    assert_eq!(parse("range(1, 10, 4).len").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("range(1, 10, 10).len").unwrap().eval().unwrap().to_string(), "1");
    assert!(parse("range(1, 10, 0).len").unwrap().eval().is_err());
    assert_eq!(parse("range(1, 10, -1).len").unwrap().eval().unwrap().to_string(), "0");
    assert_eq!(parse("range(1, -10, -3).len").unwrap().eval().unwrap().to_string(), "4");

    assert_eq!(parse("range(10, 1, -2).len").unwrap().eval().unwrap().to_string(), "5");
    assert_eq!(parse("range(10, 1, -3).len").unwrap().eval().unwrap().to_string(), "4");
    assert_eq!(parse("range(10, 1, -4).len").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("range(10, 1, -5).len").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("range(10, 1, -9).len").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("range(10, 1, -10).len").unwrap().eval().unwrap().to_string(), "1");
    assert_eq!(parse("range(10, 1, -11).len").unwrap().eval().unwrap().to_string(), "1");

    assert_eq!(parse("range(10^9, 10^10, 2).len").unwrap().eval().unwrap().to_string(), "4500000001");
}

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("seq", Seq::construct);
    keywords.insert("range", Range::construct);
    keywords.insert("len", construct_len);
}
