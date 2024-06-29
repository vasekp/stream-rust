#![allow(clippy::redundant_closure_call)]
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
/// let env = Default::default();
/// assert_eq!(parse("seq").unwrap().eval(&env).unwrap().to_string(), "[1, 2, 3, ...");
/// assert_eq!(parse("seq(3)").unwrap().eval(&env).unwrap().to_string(), "[3, 4, 5, ...");
/// assert_eq!(parse("seq(1, 3)").unwrap().eval(&env).unwrap().to_string(), "[1, 4, 7, ...");
/// assert_eq!(parse("seq(3, 0)").unwrap().eval(&env).unwrap().to_string(), "[3, 3, 3, ...");
/// ```
#[derive(Clone)]
pub struct Seq {
    from: Number,
    step: Number
}

struct SeqIter<'node> {
    value: Number,
    step: &'node Number
}

impl Stream for Seq {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SeqIter{
            value: self.from.clone(),
            step: &self.step
        })
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Seq {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
        let (from, step) = try_with!(node, match node.args[..] {
            [] => Ok((Number::one(), Number::one())),
            [Item::Number(ref mut from)]
                => Ok((std::mem::take(from), Number::one())),
            [Item::Number(ref mut from), Item::Number(ref mut step)]
                => Ok((std::mem::take(from), std::mem::take(step))),
            _ => Err("expected one of: seq(), seq(number), seq(number, number)".into())
        });
        Ok(Item::new_stream(Seq{from, step}))
    }
}

impl Describe for Seq {
    fn describe(&self) -> String {
        format!("seq({}, {})", self.from, self.step)
    }
}

impl Iterator for SeqIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = Item::new_number(self.value.clone());
        self.value += self.step;
        Some(Ok(ret))
    }
}

impl SIterator for SeqIter<'_> {
    fn skip_n(&mut self, n: Number) -> Result<Option<Number>, StreamError> {
        debug_assert!(!n.is_negative());
        self.value += n * self.step;
        Ok(None)
    }
}

#[test]
fn test_seq() {
    use crate::parser::parse;
    let env = Default::default();

    // in addition to doc tests
    assert!(parse("1.seq").unwrap().eval(&env).is_err());
    assert_eq!(parse("seq(0)").unwrap().eval(&env).unwrap().to_string(), "[0, 1, 2, ...");
    assert_eq!(parse("seq(2, 3)").unwrap().eval(&env).unwrap().to_string(), "[2, 5, 8, ...");
    assert_eq!(parse("seq(2, 0)").unwrap().eval(&env).unwrap().to_string(), "[2, 2, 2, ...");
    assert_eq!(parse("seq(2, -3)").unwrap().eval(&env).unwrap().to_string(), "[2, -1, -4, ...");

    assert_eq!(parse("seq(2, 3)[10^10]").unwrap().eval(&env).unwrap().to_string(), "29999999999");
    assert_eq!(parse("seq(2, 0)[10^10]").unwrap().eval(&env).unwrap().to_string(), "2");
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
/// let env = Default::default();
/// assert_eq!(parse("range(3)").unwrap().eval(&env).unwrap().to_string(), "[1, 2, 3]");
/// assert_eq!(parse("range(0, 2)").unwrap().eval(&env).unwrap().to_string(), "[0, 1, 2]");
/// assert_eq!(parse("range(3, 1, -1)").unwrap().eval(&env).unwrap().to_string(), "[3, 2, 1]");
/// ```
#[derive(Clone)]
pub struct Range {
    from: Number,
    to: Number,
    step: Number,
    rtype: RangeType,
    env: Env
}

#[derive(Clone, Copy)]
enum RangeType {
    Numeric,
    Character(CharCase)
}

struct RangeIter<'node> {
    parent: &'node Range,
    value: Number
}

impl Stream for Range {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(RangeIter{
            parent: self,
            value: self.from.clone()
        })
    }

    fn length(&self) -> Length {
        use Length::*;
        if (self.to > self.from && self.step.is_negative())
                || (self.to < self.from && self.step.is_positive()) {
            return Exact(Number::zero());
        }
        match Range::len_helper(&self.from, &self.to, &self.step) {
            Some(num) => Exact(num),
            None => Infinite
        }
    }
}

impl Range {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        try_with!(node, node.check_no_source());
        let (from, to, step, rtype) = try_with!(node, match node.args[..] {
            [Item::Number(ref mut to)]
                => Ok((Number::one(), std::mem::take(to), Number::one(), RangeType::Numeric)),
            [Item::Number(ref mut from), Item::Number(ref mut to)]
                => Ok((std::mem::take(from), std::mem::take(to), Number::one(), RangeType::Numeric)),
            [Item::Number(ref mut from), Item::Number(ref mut to), Item::Number(ref mut step)]
                => Ok((std::mem::take(from), std::mem::take(to), std::mem::take(step), RangeType::Numeric)),
            [Item::Char(ref from), Item::Char(ref to)]
                => {
                    let abc = env.alphabet();
                    let (from_ix, case) = abc.ord_case(from)?;
                    let (to_ix, _) = abc.ord_case(to)?;
                    Ok((from_ix.into(), to_ix.into(), Number::one(), RangeType::Character(case)))
                },
            [Item::Char(ref from), Item::Char(ref to), Item::Number(ref mut step)]
                => {
                    let abc = env.alphabet();
                    let (from_ix, case) = abc.ord_case(from)?;
                    let (to_ix, _) = abc.ord_case(to)?;
                    Ok((from_ix.into(), to_ix.into(), std::mem::take(step), RangeType::Character(case)))
                },
            _ => Err("expected one of: range(num), range(num, num), range(num, num, num), range(char, char), range(char, char, num)".into())
        });
        Ok(Item::new_stream(Range{from, to, step, rtype, env: env.clone()}))
    }

    fn len_helper(from: &Number, to: &Number, step: &Number) -> Option<Number> {
        match step.to_i32() {
            Some(1) => Some(to - from + 1),
            Some(-1) => Some(from - to + 1),
            Some(0) => None,
            _ => Some((to - from) / step + 1)
        }
    }
}

impl Describe for Range {
    fn describe(&self) -> String {
        match self.rtype {
            RangeType::Numeric => format!("range({}, {}, {})", self.from, self.to, self.step),
            RangeType::Character(case) => {
                let abc = self.env.alphabet();
                let base = format!("range({}, {}, {})", abc.chr_case(&self.from, case), abc.chr_case(&self.to, case), self.step);
                self.env.wrap_describe(base)
            }
        }
    }
}

impl Iterator for RangeIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.parent.step.is_zero()
            || (self.parent.step.is_positive() && self.value <= self.parent.to)
            || (self.parent.step.is_negative() && self.value >= self.parent.to) {
                let ret = match self.parent.rtype {
                    RangeType::Numeric => Item::new_number(self.value.clone()),
                    RangeType::Character(case) => Item::new_char(self.parent.env.alphabet().chr_case(&self.value, case))
                };
                self.value += &self.parent.step;
                Some(Ok(ret))
        } else {
            None
        }
    }
}

impl SIterator for RangeIter<'_> {
    fn skip_n(&mut self, n: Number) -> Result<Option<Number>, StreamError> {
        debug_assert!(!n.is_negative());
        let Some(max) = Range::len_helper(&self.value, &self.parent.to, &self.parent.step)
            else { return Ok(None); };
        if n <= max {
            self.value += n * &self.parent.step;
            Ok(None)
        } else {
            Ok(Some(n - &max))
        }
    }
}

#[test]
fn test_range() {
    use crate::parser::parse;
    let env = Default::default();
    assert_eq!(parse("range(3)").unwrap().eval(&env).unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("range(0)").unwrap().eval(&env).unwrap().to_string(), "[]");
    assert_eq!(parse("range(3, 3)").unwrap().eval(&env).unwrap().to_string(), "[3]");
    assert_eq!(parse("range(3, 5)").unwrap().eval(&env).unwrap().to_string(), "[3, 4, 5]");
    assert_eq!(parse("range(5, 3)").unwrap().eval(&env).unwrap().to_string(), "[]");
    assert_eq!(parse("range(1, 10, 4)").unwrap().eval(&env).unwrap().to_string(), "[1, 5, 9]");
    assert_eq!(parse("range(1, 10, 10)").unwrap().eval(&env).unwrap().to_string(), "[1]");
    assert_eq!(parse("range(1, 10, 0)").unwrap().eval(&env).unwrap().to_string(), "[1, 1, 1, ...");
    assert_eq!(parse("range(1, 10, -1)").unwrap().eval(&env).unwrap().to_string(), "[]");
    assert_eq!(parse("range(1, -10, -3)").unwrap().eval(&env).unwrap().to_string(), "[1, -2, -5, ...");

    assert_eq!(parse("range('a', 'C')").unwrap().eval(&env).unwrap().to_string(), "['a', 'b', 'c']");
    assert_eq!(parse("range('D', 'f')").unwrap().eval(&env).unwrap().to_string(), "['D', 'E', 'F']");
    assert_eq!(parse("range('a', 'h', 3)").unwrap().eval(&env).unwrap().to_string(), "['a', 'd', 'g']");
    assert_eq!(parse("range('a', 'z', -1)").unwrap().eval(&env).unwrap().to_string(), "[]");
    assert_eq!(parse("range('a', 'z', 0)").unwrap().eval(&env).unwrap().to_string(), "['a', 'a', 'a', ...");
    assert!(parse("range('a')").unwrap().eval(&env).is_err());
    assert!(parse("range('a', 1)").unwrap().eval(&env).is_err());
    assert!(parse("range(1, 'a')").unwrap().eval(&env).is_err());
    assert!(parse("range('a', 'h', 'c')").unwrap().eval(&env).is_err());

    assert_eq!(parse("1..3").unwrap().eval(&env).unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("3..3").unwrap().eval(&env).unwrap().to_string(), "[3]");
    assert_eq!(parse("3..1").unwrap().eval(&env).unwrap().to_string(), "[]");
    assert!(parse("1..2..3").unwrap().eval(&env).is_err());
    assert_eq!(parse("'a'..'z'").unwrap().eval(&env).unwrap().to_string(), "['a', 'b', 'c', ...");
    assert_eq!(parse("'A'..'z'").unwrap().eval(&env).unwrap().to_string(), "['A', 'B', 'C', ...");
    assert!(parse("'a'..'á'").unwrap().eval(&env).is_err());
}

#[test]
fn test_range_skip() {
    use crate::parser::parse;
    let env = Default::default();
    assert_eq!(parse("(range(2, 7, 2)~seq(100))[3]").unwrap().eval(&env).unwrap().to_string(), "6");
    assert_eq!(parse("(range(2, 7, 2)~seq(100))[4]").unwrap().eval(&env).unwrap().to_string(), "100");
    assert_eq!(parse("(range(2, 7, 2)~seq(100))[5]").unwrap().eval(&env).unwrap().to_string(), "101");
    assert_eq!(parse("(range(2, 8, 2)~seq(100))[3]").unwrap().eval(&env).unwrap().to_string(), "6");
    assert_eq!(parse("(range(2, 8, 2)~seq(100))[4]").unwrap().eval(&env).unwrap().to_string(), "8");
    assert_eq!(parse("(range(2, 8, 2)~seq(100))[5]").unwrap().eval(&env).unwrap().to_string(), "100");
    assert_eq!(parse("(range(2, 8, 2)~seq(100))[6]").unwrap().eval(&env).unwrap().to_string(), "101");
    assert_eq!(parse("(range(2, 8, 0)~seq(100))[6]").unwrap().eval(&env).unwrap().to_string(), "2");
}


fn eval_len(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    if !node.args.is_empty() {
        return Err(StreamError::new("no arguments allowed", node.into()));
    }
    let length = try_with!(node, node.source_checked()?.as_stream()).length();
    use Length::*;
    match length {
        Exact(len) => Ok(Item::new_number(len)),
        AtMost(_) | UnknownFinite | Unknown => {
            let len = try_with!(node, node.source_checked()?.as_stream()).iter().count();
            Ok(Item::new_number(len))
        },
        _ => Err(StreamError::new("stream is infinite", node.into()))
    }
}

#[test]
fn test_range_length() {
    use crate::parser::parse;
    let env = Default::default();

    assert_eq!(parse("range(3).len").unwrap().eval(&env).unwrap().to_string(), "3");
    assert_eq!(parse("range(0).len").unwrap().eval(&env).unwrap().to_string(), "0");
    assert_eq!(parse("range(3, 3).len").unwrap().eval(&env).unwrap().to_string(), "1");
    assert_eq!(parse("range(3, 5).len").unwrap().eval(&env).unwrap().to_string(), "3");
    assert_eq!(parse("range(5, 3).len").unwrap().eval(&env).unwrap().to_string(), "0");
    assert_eq!(parse("range(1, 10, 4).len").unwrap().eval(&env).unwrap().to_string(), "3");
    assert_eq!(parse("range(1, 10, 10).len").unwrap().eval(&env).unwrap().to_string(), "1");
    assert!(parse("range(1, 10, 0).len").unwrap().eval(&env).is_err());
    assert_eq!(parse("range(1, 10, -1).len").unwrap().eval(&env).unwrap().to_string(), "0");
    assert_eq!(parse("range(1, -10, -3).len").unwrap().eval(&env).unwrap().to_string(), "4");

    assert_eq!(parse("range(10, 1, -2).len").unwrap().eval(&env).unwrap().to_string(), "5");
    assert_eq!(parse("range(10, 1, -3).len").unwrap().eval(&env).unwrap().to_string(), "4");
    assert_eq!(parse("range(10, 1, -4).len").unwrap().eval(&env).unwrap().to_string(), "3");
    assert_eq!(parse("range(10, 1, -5).len").unwrap().eval(&env).unwrap().to_string(), "2");
    assert_eq!(parse("range(10, 1, -9).len").unwrap().eval(&env).unwrap().to_string(), "2");
    assert_eq!(parse("range(10, 1, -10).len").unwrap().eval(&env).unwrap().to_string(), "1");
    assert_eq!(parse("range(10, 1, -11).len").unwrap().eval(&env).unwrap().to_string(), "1");

    assert_eq!(parse("range(10^9, 10^10, 2).len").unwrap().eval(&env).unwrap().to_string(), "4500000001");
}

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("seq", Seq::eval);
    keywords.insert("range", Range::eval);
    keywords.insert("..", Range::eval);
    keywords.insert("len", eval_len);
}
