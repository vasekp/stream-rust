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

impl Stream for SeqStream {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(num::iter::range_step_from(self.from.clone(), self.step.clone())
                 .map(|x| Ok(Item::new_atomic(x))))
    }

    fn describe(&self) -> String {
        format!("seq({}, {})", self.from, self.step)
    }

    fn length(&self) -> Length {
        Length::Infinite
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

impl Stream for RangeStream {
    fn iter(&self) -> Box<dyn SIterator> {
        Box::new(num::iter::range_step_inclusive(self.from.clone(), self.to.clone(), self.step.clone())
                 .map(|x| Ok(Item::new_atomic(x))))
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
        match self.step.to_i32() {
            Some(1) => Exact(&self.to - &self.from + 1),
            Some(-1) => Exact(&self.from - &self.to + 1),
            Some(0) => Infinite,
            _ => Exact((&self.to - &self.from) / &self.step + 1),
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
