#![allow(clippy::redundant_closure_call)]
use crate::base::*;
use crate::alphabet::*;
use crate::utils::{EmptyStream, EmptyString};
use std::rc::Rc;

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
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
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
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
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
    test_skip_n(&parse("seq(2,0)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("seq(2,3)").unwrap().eval(&env).unwrap());
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
    env: Rc<Env>
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
        match Range::len_helper(&self.from, &self.to, &self.step) {
            Some(num) => Length::Exact(num),
            None => Length::Infinite
        }
    }
}

impl Range {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
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
        if (to > from && step.is_negative()) || (to < from && step.is_positive()) {
            Ok(Item::new_stream(EmptyStream()))
        } else {
            Ok(Item::new_stream(Range{from, to, step, rtype, env: Rc::clone(env)}))
        }
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
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        debug_assert!(!n.is_negative());
        let Some(max) = Range::len_helper(&self.value, &self.parent.to, &self.parent.step)
            else { return Ok(None); };
        if n <= &max {
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
    assert_eq!(parse("range(1, 1, 0)").unwrap().eval(&env).unwrap().to_string(), "[1, 1, 1, ...");
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

    assert_eq!(parse("range(10^9, 10^10, 2).len").unwrap().eval(&env).unwrap().to_string(), "4500000001");
    test_len_exact(&parse("range(0)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("range(-1)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("range(3)").unwrap().eval(&env).unwrap(), 3);
    test_len_exact(&parse("range(0,3)").unwrap().eval(&env).unwrap(), 4);
    test_len_exact(&parse("range(3,0)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("range(3,3)").unwrap().eval(&env).unwrap(), 1);
    test_len_exact(&parse("range(0,3,2)").unwrap().eval(&env).unwrap(), 2);
    test_len_exact(&parse("range(0,3,3)").unwrap().eval(&env).unwrap(), 2);
    test_len_exact(&parse("range(0,3,4)").unwrap().eval(&env).unwrap(), 1);
    test_len_exact(&parse("range(0,4,2)").unwrap().eval(&env).unwrap(), 3);
    test_len_exact(&parse("range(0,3,-2)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("range(0,-3,-2)").unwrap().eval(&env).unwrap(), 2);
    test_len_exact(&parse("range(0,-3,-3)").unwrap().eval(&env).unwrap(), 2);
    test_len_exact(&parse("range(0,-3,-4)").unwrap().eval(&env).unwrap(), 1);
    test_len_exact(&parse("'a'..'z'").unwrap().eval(&env).unwrap(), 26);
    test_skip_n(&parse("range(0)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(10^10)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(-10^10)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,3)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(3,0)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,3,2)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,3,3)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,3,4)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,4,2)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,3,0)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,3,-2)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(0,-3,-2)").unwrap().eval(&env).unwrap());
}


fn eval_len(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
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


#[derive(Clone)]
pub struct Repeat {
    item: Item,
    count: Option<Number>
}

impl Repeat {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        let (item, count) = try_with!(node, match (&mut node.source, node.args.len(), node.args.get_mut(0)) {
            (Some(ref mut src), 0, None)
                => Ok((std::mem::take(src), None)),
            (Some(ref mut src), 1, Some(Item::Number(ref mut count))) => {
                if count.is_negative() {
                    Err(format!("expected nonnegative count, found {}", count).into())
                } else {
                    Ok((std::mem::take(src), Some(std::mem::take(count))))
                }
            },
            _ => Err("expected one of: source.repeat(), source.repeat(count)".into())
        });
        if let Item::Stream(ref stm) = &item {
            if stm.is_empty() || count.as_ref().map_or(false, Zero::is_zero) {
                return Ok(
                    if stm.is_string() { Item::new_stream(EmptyString()) }
                    else { Item::new_stream(EmptyStream()) }
                );
            }
            if count.as_ref().map_or(false, One::is_one) {
                return Ok(item)
            }
        } else if let Some(ref count) = count {
            if count.is_zero() {
                return Ok(Item::new_stream(EmptyStream()));
            }
        }
        Ok(Item::new_stream(Repeat{item, count}))
    }
}

impl Stream for Repeat {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        match &self.item {
            Item::Stream(stream) => Box::new(RepeatStreamIter {
                stream: &**stream,
                iter: stream.iter(),
                resets_rem: self.count.as_ref()
                    .map(|count| count - 1)
            }),
            item => match &self.count {
                Some(count) => Box::new(RepeatItemIter{item, count_rem: count.to_owned()}),
                None => Box::new(Forever{item})
            }
        }
    }

    fn length(&self) -> Length {
        use Length::*;
        if self.count == Some(Number::zero()) { return Exact(Number::zero()); }
        match &self.item {
            Item::Stream(stream) => {
                if stream.is_empty() { return Exact(Number::zero()); }
                match (stream.length(), &self.count) {
                    (_, None) | (Infinite, _) => Infinite,
                    (Exact(len), Some(count)) => Exact(len * count),
                    (AtMost(len), Some(count)) => AtMost(len * count),
                    (UnknownFinite, _) => UnknownFinite,
                    (Unknown, _) => Unknown
                }
            },
            _ => match &self.count {
                Some(count) => Exact(count.to_owned()),
                None => Infinite
            }
        }
    }

    fn is_string(&self) -> bool {
        match &self.item {
            Item::Stream(stream) => stream.is_string(),
            _ => false
        }
    }
}

impl Describe for Repeat {
    fn describe(&self) -> String {
        Node::describe_helper(&Head::Symbol("repeat".into()), Some(&self.item),
            self.clone().count.map(Item::new_number).as_slice())
    }
}

struct RepeatItemIter<'node> {
    item: &'node Item,
    count_rem: Number // None covered by stream::Forever
}

impl Iterator for RepeatItemIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.count_rem.is_zero() {
            self.count_rem.dec();
            Some(Ok(self.item.clone()))
        } else {
            None
        }
    }
}

impl SIterator for RepeatItemIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        assert!(!n.is_negative());
        if n > &self.count_rem {
            Ok(Some(n - &self.count_rem))
        } else {
            self.count_rem -= n;
            Ok(None)
        }
    }
}

struct RepeatStreamIter<'node> {
    stream: &'node dyn Stream,
    iter: Box<dyn SIterator + 'node>,
    resets_rem: Option<Number>
}

impl Iterator for RepeatStreamIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.iter.next();
        if next.is_some() {
            return next;
        }
        if let Some(ref mut count) = self.resets_rem {
            if count.is_zero() {
                return None;
            }
            (*count).dec();
        }
        self.iter = self.stream.iter();
        self.iter.next()
    }
}

impl SIterator for RepeatStreamIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        assert!(!n.is_negative());

        let Some(n) = self.iter.skip_n(n)? else { return Ok(None); };

        // If skip_n returned Some, iter is depleted. Restart.
        if let Some(ref mut count) = self.resets_rem {
            if count.is_zero() {
                return Ok(Some(n));
            }
            (*count).dec();
        }
        self.iter = self.stream.iter();

        // This point is special: we know that iter() is now newly initiated, so we can use it to
        // determine the length regardless of whether it's statically known.
        let (full_length, mut n) = match self.iter.skip_n(&n)? {
            None => return Ok(None),
            Some(remain) => (n - &remain, remain)
        };

        if full_length.is_zero() {
            return Ok(Some(n));
        }
        let skip_full = &n / &full_length;
        if let Some(count) = &mut self.resets_rem {
            if *count < skip_full {
                return Ok(Some(n - &*count * full_length));
            } else {
                *count -= &skip_full;
            }
        }
        n -= &skip_full * &full_length;

        // Iter is depleted from the counting, restart once more.
        if let Some(ref mut count) = self.resets_rem {
            if count.is_zero() {
                return Ok(Some(n));
            }
            (*count).dec();
        }
        self.iter = self.stream.iter();
        debug_assert!(n < full_length);
        self.iter.skip_n(&n)
    }
}

#[test]
fn test_repeat() {
    use crate::parser::parse;
    let env = Default::default();
    assert_eq!(parse("1.repeat").unwrap().eval(&env).unwrap().to_string(), "[1, 1, 1, ...");
    assert_eq!(parse("1.repeat(1)").unwrap().eval(&env).unwrap().to_string(), "[1]");
    assert_eq!(parse("1.repeat(3)").unwrap().eval(&env).unwrap().to_string(), "[1, 1, 1]");
    assert_eq!(parse("1.repeat(0)").unwrap().eval(&env).unwrap().describe(), "[]");
    assert!(parse("1.repeat(-1)").unwrap().eval(&env).is_err());
    assert_eq!(parse("(1..2).repeat(2)").unwrap().eval(&env).unwrap().to_string(), "[1, 2, 1, ...");
    assert_eq!(parse("[1, 2].repeat(1)").unwrap().eval(&env).unwrap().describe(), "[1, 2]");
    assert_eq!(parse("\"ab\".repeat").unwrap().eval(&env).unwrap().to_string(), "\"abababababababababab...");
    assert_eq!(parse("\"ab\".repeat(3)").unwrap().eval(&env).unwrap().to_string(), "\"ababab\"");
    assert_eq!(parse("\"ab\".repeat(0)").unwrap().eval(&env).unwrap().to_string(), "\"\"");
    assert_eq!(parse("\"ab\".repeat(1)").unwrap().eval(&env).unwrap().describe(), "\"ab\"");
    assert_eq!(parse("seq.repeat(0)").unwrap().eval(&env).unwrap().to_string(), "[]");
    assert_eq!(parse("[].repeat(0)").unwrap().eval(&env).unwrap().describe(), "[]");
    assert_eq!(parse("[].repeat(1)").unwrap().eval(&env).unwrap().describe(), "[]");
    assert_eq!(parse("[].repeat(10)").unwrap().eval(&env).unwrap().describe(), "[]");
    assert_eq!(parse("\"\".repeat(0)").unwrap().eval(&env).unwrap().describe(), "\"\"");
    assert_eq!(parse("\"\".repeat(1)").unwrap().eval(&env).unwrap().describe(), "\"\"");
    assert_eq!(parse("\"\".repeat(10)").unwrap().eval(&env).unwrap().describe(), "\"\"");

    assert_eq!(parse("\"abc\".repeat[10^10]").unwrap().eval(&env).unwrap().to_string(), "'a'");
    assert_eq!(parse("[].repeat~1").unwrap().eval(&env).unwrap().to_string(), "[1]");
    assert_eq!(parse(r#"("ab".repeat(10^10)~"cd".repeat(10^10))[4*10^10]"#).unwrap().eval(&env).unwrap().to_string(), "'d'");

    assert!(parse("\"ab\".repeat.len").unwrap().eval(&env).is_err());
    assert!(parse("1.repeat.len").unwrap().eval(&env).is_err());
    test_len_exact(&parse("1.repeat(0)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("1.repeat(1)").unwrap().eval(&env).unwrap(), 1);
    test_len_exact(&parse("1.repeat(3)").unwrap().eval(&env).unwrap(), 3);
    test_len_exact(&parse("[].repeat").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("[].repeat(3)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("[1,2].repeat(0)").unwrap().eval(&env).unwrap(), 0);
    test_len_exact(&parse("[1,2].repeat(1)").unwrap().eval(&env).unwrap(), 2);
    test_len_exact(&parse("[1,2].repeat(3)").unwrap().eval(&env).unwrap(), 6);
    test_len_exact(&parse("seq.repeat(0)").unwrap().eval(&env).unwrap(), 0);
    test_skip_n(&parse("1.repeat").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("1.repeat(10^10)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("[].repeat").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("[].repeat(10^10)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("[1,2].repeat").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("[1,2].repeat(10^10)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("range(10^10).repeat(10^10)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("seq.repeat").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("seq.repeat(0)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("seq.repeat(1)").unwrap().eval(&env).unwrap());
    test_skip_n(&parse("seq.repeat(2)").unwrap().eval(&env).unwrap());
}

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("seq", Seq::eval);
    keywords.insert("range", Range::eval);
    keywords.insert("..", Range::eval);
    keywords.insert("len", eval_len);
    keywords.insert("repeat", Repeat::eval);
}
