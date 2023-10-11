use crate::base::*;
use num::{Zero, One, ToPrimitive, Signed};
use crate::session::Session;


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
/// let stream = sess.eval(&parse("seq").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[1, 2, 3, ...");
/// let stream = sess.eval(&parse("seq(3)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[3, 4, 5, ...");
/// let stream = sess.eval(&parse("seq(1, 3)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[1, 4, 7, ...");
/// let stream = sess.eval(&parse("seq(3, 0)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[3, 3, 3, ...");
/// ```
#[derive(Clone)]
pub struct SeqStream {
    from: Number,
    step: Number
}

struct SeqIter {
    value: Number,
    step: Number
}

impl Stream for SeqStream {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(SeqIter{value: self.from.clone(), step: self.step.clone()})
    }

    fn describe(&self) -> String {
        format!("seq({}, {})", self.from, self.step)
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Iterator for SeqIter {
    type Item = Result<Item, BaseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = Item::new_atomic(self.value.clone());
        self.value += &self.step;
        Some(Ok(ret))
    }
}

impl SIterator for SeqIter {
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        debug_assert!(!n.is_negative());
        self.value += n * &self.step;
        Ok(())
    }
}

fn construct_seq(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 0..=2)?;
    let nums = node.args.iter()
        .map(|x| session.eval(x).and_then(|y| Ok(y.as_num()?.clone())))
        .collect::<Result<Vec<_>, _>>()?;
    let mut it = nums.into_iter();
    let from = it.next().unwrap_or(Number::one()).clone();
    let step = it.next().unwrap_or(Number::one()).clone();
    Ok(Item::new_stream(SeqStream{from, step}))
}

#[test]
fn test_seq() {
    use crate::parser::parse;
    let sess = Session::new();
    // in addition to doc tests
    let stream = sess.eval(&parse("seq(0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[0, 1, 2, ...");
    let stream = sess.eval(&parse("seq(2, 3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[2, 5, 8, ...");
    let stream = sess.eval(&parse("seq(2, 0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[2, 2, 2, ...");
    /*let stream = sess.eval(&parse("seq(2, -3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[2, -1, -4, ...");*/
}

#[test]
fn test_seq_skip() {
    use crate::parser::parse;
    let sess = Session::new();
    let stream = sess.eval(&parse("seq(2, 3)[5]").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "14");
    let stream = sess.eval(&parse("seq(2, 0)[5]").unwrap()).unwrap();
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
/// let stream = sess.eval(&parse("range(3)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[1, 2, 3]");
/// let stream = sess.eval(&parse("range(0, 2)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[0, 1, 2]");
/// /*let stream = sess.eval(&parse("range(3, 1, -1)").unwrap()).unwrap();
/// assert_eq!(stream.to_string(), "[3, 2, 1]");*/
/// ```
#[derive(Clone)]
pub struct RangeStream {
    from: Number,
    to: Number,
    step: Number
}

struct RangeIter {
    value: Number,
    step: Number,
    stop: Number
}

impl Stream for RangeStream {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(RangeIter{value: self.from.clone(), stop: self.to.clone(), step: self.step.clone()})
    }

    fn describe(&self) -> String {
        format!("range({}, {}, {})", self.from, self.to, self.step)
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
            Exact(Self::length_helper(&self.from, &self.to, &self.step))
        }
    }
}

impl RangeStream {
    fn length_helper(from: &Number, to: &Number, step: &Number) -> Number {
        match step.to_i32() {
            Some(1) => to - from + 1,
            Some(-1) => from - to + 1,
            Some(0) => unreachable!(),
            _ => (to - from) / step + 1
        }
    }
}

impl Iterator for RangeIter {
    type Item = Result<Item, BaseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.step.is_zero()
            || (self.step.is_positive() && self.value <= self.stop)
            || (self.step.is_negative() && self.value >= self.stop) {
                let ret = Item::new_atomic(self.value.clone());
                self.value += &self.step;
                Some(Ok(ret))
        } else {
            None
        }
    }
}

impl SIterator for RangeIter {
    fn skip_n(&mut self, n: &Number) -> Result<(), Number> {
        debug_assert!(!n.is_negative());
        if self.step.is_zero() {
            return Ok(())
        }
        let max = RangeStream::length_helper(&self.value, &self.stop, &self.step);
        if max >= *n {
            self.value += n * &self.step;
            Ok(())
        } else {
            Err(n - max)
        }
    }
}

fn construct_range(session: &Session, node: &Node) -> Result<Item, BaseError> {
    node.check_args(false, 1..=3)?;
    let len = node.args.len();
    let nums = node.args.iter()
        .map(|x| session.eval(x).and_then(|y| Ok(y.as_num()?.clone())))
        .collect::<Result<Vec<_>, _>>()?;
    let mut it = nums.into_iter();
    let (from, to, step) = match len {
        1 => (Number::one(), it.next().unwrap(), Number::one()),
        2 => (it.next().unwrap(), it.next().unwrap(), Number::one()),
        3 => (it.next().unwrap(), it.next().unwrap(), it.next().unwrap()),
        _ => unreachable!()
    };
    Ok(Item::new_stream(RangeStream{from, to, step}))
}

#[test]
fn test_range() {
    use crate::parser::parse;
    let sess = Session::new();
    let stream = sess.eval(&parse("range(3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, 2, 3]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = sess.eval(&parse("range(0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = sess.eval(&parse("range(3, 3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[3]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = sess.eval(&parse("range(3, 5)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[3, 4, 5]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = sess.eval(&parse("range(5, 3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = sess.eval(&parse("range(1, 10, 4)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, 5, 9]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = sess.eval(&parse("range(1, 10, 10)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = sess.eval(&parse("range(1, 10, 0)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, 1, 1, ...");
    assert_eq!(stream.as_stream().unwrap().length(), Length::Infinite);
    /*let stream = sess.eval(&parse("range(1, 10, -1)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[]");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(0));
    let stream = sess.eval(&parse("range(1, -10, -3)").unwrap()).unwrap();
    assert_eq!(stream.to_string(), "[1, -2, -5, ...");
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(4));*/
}

#[test]
fn test_range_skip() {
    use crate::parser::parse;
    let sess = Session::new();
    let mut it = sess.eval(&parse("range(2, 7, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&2.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_atomic(6))));
    let mut it = sess.eval(&parse("range(2, 7, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&3.into()), Ok(()));
    assert_eq!(it.next(), None);
    let mut it = sess.eval(&parse("range(2, 7, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&4.into()), Err(1.into()));

    let mut it = sess.eval(&parse("range(2, 8, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&3.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_atomic(8))));
    let mut it = sess.eval(&parse("range(2, 8, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&4.into()), Ok(()));
    assert_eq!(it.next(), None);
    let mut it = sess.eval(&parse("range(2, 8, 2)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&5.into()), Err(1.into()));

    let mut it = sess.eval(&parse("range(2, 8, 0)").unwrap()).unwrap().into_stream().unwrap().iter();
    assert_eq!(it.skip_n(&3.into()), Ok(()));
    assert_eq!(it.next(), Some(Ok(Item::new_atomic(2))));
}

/*#[test]
fn range_test_neg_lengths() {
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-2)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(5));
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-3)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(4));
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-4)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(3));
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-5)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(2));
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-9)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(2));
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-10)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
    let stream = construct_range_test(vec![Item::new_atomic(10), Item::new_atomic(1), Item::new_atomic(-11)]).unwrap();
    assert_eq!(stream.as_stream().unwrap().length(), Length::from(1));
}*/


pub(crate) fn init(session: &mut Session) {
    session.register_symbol("seq", construct_seq);
    session.register_symbol("range", construct_range);
}
