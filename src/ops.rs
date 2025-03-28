use crate::base::*;
use crate::alphabet::*;
use crate::utils::{EmptyStream, EmptyString, TriState};
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::pin::Pin;

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
        try_with!(node, node.check_no_source()?);
        let (from, step) = try_with!(node, match node.args[..] {
            [] => (Number::one(), Number::one()),
            [Item::Number(ref mut from)]
                => (std::mem::take(from), Number::one()),
            [Item::Number(ref mut from), Item::Number(ref mut step)]
                => (std::mem::take(from), std::mem::take(step)),
            _ => return Err("expected one of: seq(), seq(number), seq(number, number)".into())
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
    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        debug_assert!(!n.is_negative());
        self.value += n * self.step;
        Ok(None)
    }
}

#[test]
fn test_seq() {
    use crate::parser::parse;
    // in addition to doc tests
    assert!(parse("1.seq").unwrap().eval().is_err());
    assert_eq!(parse("seq(0)").unwrap().eval().unwrap().to_string(), "[0, 1, 2, 3, 4, ...]");
    assert_eq!(parse("seq(2, 3)").unwrap().eval().unwrap().to_string(), "[2, 5, 8, 11, 14, ...]");
    assert_eq!(parse("seq(2, 0)").unwrap().eval().unwrap().to_string(), "[2, 2, 2, 2, 2, ...]");
    assert_eq!(parse("seq(2, -3)").unwrap().eval().unwrap().to_string(), "[2, -1, -4, -7, -10, ...]");
    assert_eq!(parse("seq(2, 3)[10^10]").unwrap().eval().unwrap().to_string(), "29999999999");
    assert_eq!(parse("seq(2, 0)[10^10]").unwrap().eval().unwrap().to_string(), "2");
    test_skip_n(&parse("seq(2,0)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq(2,3)").unwrap().eval().unwrap());
}


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

impl Range {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        let (from, to, step, rtype) = try_with!(node, match node.args[..] {
            [Item::Number(ref mut to)]
                => (Number::one(), std::mem::take(to), Number::one(), RangeType::Numeric),
            [Item::Number(ref mut from), Item::Number(ref mut to)]
                => (std::mem::take(from), std::mem::take(to), Number::one(), RangeType::Numeric),
            [Item::Number(ref mut from), Item::Number(ref mut to), Item::Number(ref mut step)]
                => (std::mem::take(from), std::mem::take(to), std::mem::take(step), RangeType::Numeric),
            [Item::Char(ref from), Item::Char(ref to)]
                => {
                    let abc = env.alphabet();
                    let (from_ix, case) = abc.ord_case(from)?;
                    let (to_ix, _) = abc.ord_case(to)?;
                    (from_ix.into(), to_ix.into(), Number::one(), RangeType::Character(case))
                },
            [Item::Char(ref from), Item::Char(ref to), Item::Number(ref mut step)]
                => {
                    let abc = env.alphabet();
                    let (from_ix, case) = abc.ord_case(from)?;
                    let (to_ix, _) = abc.ord_case(to)?;
                    (from_ix.into(), to_ix.into(), std::mem::take(step), RangeType::Character(case))
                },
            _ => return Err("expected one of: range(num), range(num, num), range(num, num, num), range(char, char), range(char, char, num)".into())
        });
        if Range::empty_helper(&from, &to, &step) {
            Ok(Item::new_stream(EmptyStream()))
        } else {
            Ok(Item::new_stream(Range{from, to, step, rtype, env: Rc::clone(env)}))
        }
    }

    fn empty_helper(from: &Number, to: &Number, step: &Number) -> bool {
        (to > from && step.is_negative()) || (to < from && step.is_positive())
    }

    fn len_helper(from: &Number, to: &Number, step: &Number) -> Option<Number> {
        if Self::empty_helper(from, to, step) {
            return Some(Number::zero());
        }
        match step.to_i32() {
            Some(1) => Some(to - from + 1),
            Some(-1) => Some(from - to + 1),
            Some(0) => None,
            _ => Some((to - from) / step + 1)
        }
    }
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

struct RangeIter<'node> {
    parent: &'node Range,
    value: Number
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
        if Range::empty_helper(&self.value, &self.parent.to, &self.parent.step) {
            return Ok(Some(n.to_owned()))
        };
        let Some(max) = Range::len_helper(&self.value, &self.parent.to, &self.parent.step)
            else { return Ok(None); };
        if n <= &max {
            self.value += n * &self.parent.step;
            Ok(None)
        } else {
            Ok(Some(n - &max))
        }
    }

    fn len_remain(&self) -> Length {
        match Range::len_helper(&self.value, &self.parent.to, &self.parent.step) {
            Some(num) => Length::Exact(num),
            None => Length::Infinite
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
    assert_eq!(parse("range(1, 10, 0)").unwrap().eval().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
    assert_eq!(parse("range(1, 1, 0)").unwrap().eval().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
    assert_eq!(parse("range(1, 10, -1)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("range(1, -10, -3)").unwrap().eval().unwrap().to_string(), "[1, -2, -5, -8]");

    assert_eq!(parse("range('a', 'C')").unwrap().eval().unwrap().to_string(), "['a', 'b', 'c']");
    assert_eq!(parse("range('D', 'f')").unwrap().eval().unwrap().to_string(), "['D', 'E', 'F']");
    assert_eq!(parse("range('a', 'h', 3)").unwrap().eval().unwrap().to_string(), "['a', 'd', 'g']");
    assert_eq!(parse("range('a', 'z', -1)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("range('a', 'z', 0)").unwrap().eval().unwrap().to_string(), "['a', 'a', 'a', 'a', 'a', ...]");
    assert!(parse("range('a')").unwrap().eval().is_err());
    assert!(parse("range('a', 1)").unwrap().eval().is_err());
    assert!(parse("range(1, 'a')").unwrap().eval().is_err());
    assert!(parse("range('a', 'h', 'c')").unwrap().eval().is_err());

    assert_eq!(parse("1..3").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("3..3").unwrap().eval().unwrap().to_string(), "[3]");
    assert_eq!(parse("3..1").unwrap().eval().unwrap().to_string(), "[]");
    assert!(parse("1..2..3").unwrap().eval().is_err());
    assert_eq!(parse("'a'..'z'").unwrap().eval().unwrap().to_string(), "['a', 'b', 'c', 'd', 'e', ...]");
    assert_eq!(parse("'A'..'z'").unwrap().eval().unwrap().to_string(), "['A', 'B', 'C', 'D', 'E', ...]");
    assert!(parse("'a'..'á'").unwrap().eval().is_err());

    assert_eq!(parse("range(10^9, 10^10, 2).len").unwrap().eval().unwrap().to_string(), "4500000001");
    test_len_exact(&parse("range(0)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("range(-1)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("range(3)").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("range(0,3)").unwrap().eval().unwrap(), 4);
    test_len_exact(&parse("range(3,0)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("range(3,3)").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("range(0,3,2)").unwrap().eval().unwrap(), 2);
    test_len_exact(&parse("range(0,3,3)").unwrap().eval().unwrap(), 2);
    test_len_exact(&parse("range(0,3,4)").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("range(0,4,2)").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("range(0,3,-2)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("range(0,-3,-2)").unwrap().eval().unwrap(), 2);
    test_len_exact(&parse("range(0,-3,-3)").unwrap().eval().unwrap(), 2);
    test_len_exact(&parse("range(0,-3,-4)").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("'a'..'z'").unwrap().eval().unwrap(), 26);
    test_skip_n(&parse("range(0)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(-10^10)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,3)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(3,0)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,3,2)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,3,3)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,3,4)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,4,2)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,3,0)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,3,-2)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(0,-3,-2)").unwrap().eval().unwrap());
}


fn eval_len(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    if !node.args.is_empty() {
        return Err(StreamError::new("no arguments allowed", node));
    }
    let length = try_with!(node, node.source_checked()?.as_stream()?.length());
    use Length::*;
    match length {
        Exact(len) => Ok(Item::new_number(len)),
        AtMost(_) | UnknownFinite | Unknown => {
            let len = try_with!(node, node.source_checked()?.as_stream()?.iter().count());
            Ok(Item::new_number(len))
        },
        _ => Err(StreamError::new("stream is infinite", node))
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
                => (std::mem::take(src), None),
            (Some(ref mut src), 1, Some(Item::Number(ref mut count))) => {
                if count.is_negative() {
                    return Err(format!("expected nonnegative count, found {}", count).into());
                } else {
                    (std::mem::take(src), Some(std::mem::take(count)))
                }
            },
            _ => return Err("expected one of: source.repeat(), source.repeat(count)".into())
        });
        if let Item::Stream(ref stm) = &item {
            if stm.is_empty() || count.as_ref().is_some_and(Zero::is_zero) {
                return Ok(
                    if stm.is_string().is_true() { Item::new_stream(EmptyString()) }
                    else { Item::new_stream(EmptyStream()) }
                );
            }
            if count.as_ref().is_some_and(One::is_one) {
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
                len: stream.length(),
                resets_rem: self.count.as_ref()
                    .map(|count| count - 1)
            }),
            item => match &self.count {
                Some(count) => Box::new(RepeatItemIter{item, count_rem: count.to_owned()}),
                None => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
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

    fn is_string(&self) -> TriState {
        match &self.item {
            Item::Stream(stream) => stream.is_string(),
            Item::Char(_) => TriState::True,
            _ => TriState::False
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
    count_rem: Number // None covered by std::iter::repeat_with
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

    fn len_remain(&self) -> Length {
        Length::Exact(self.count_rem.to_owned())
    }
}

struct RepeatStreamIter<'node> {
    stream: &'node dyn Stream,
    iter: Box<dyn SIterator + 'node>,
    len: Length,
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
            count.dec();
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
            count.dec();
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
            count.dec();
        }
        self.iter = self.stream.iter();
        debug_assert!(n < full_length);
        self.iter.skip_n(&n)
    }

    fn len_remain(&self) -> Length {
        match &self.resets_rem {
            None => Length::Infinite,
            Some(count) => match &self.len {
                Length::Infinite => Length::Infinite,
                Length::Exact(len) => self.iter.len_remain() + count * len,
                _ => Length::Unknown
            }
        }
    }
}

#[test]
fn test_repeat() {
    use crate::parser::parse;

    assert_eq!(parse("1.repeat").unwrap().eval().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
    assert_eq!(parse("1.repeat(1)").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("1.repeat(3)").unwrap().eval().unwrap().to_string(), "[1, 1, 1]");
    assert_eq!(parse("1.repeat(0)").unwrap().eval().unwrap().describe(), "[]");
    assert!(parse("1.repeat(-1)").unwrap().eval().is_err());
    assert_eq!(parse("(1..2).repeat(2)").unwrap().eval().unwrap().to_string(), "[1, 2, 1, 2]");
    assert_eq!(parse("[1, 2].repeat(1)").unwrap().eval().unwrap().describe(), "[1, 2]");
    assert_eq!(parse("\"ab\".repeat").unwrap().eval().unwrap().to_string(), "\"abababababababababab...");
    assert_eq!(parse("\"ab\".repeat(3)").unwrap().eval().unwrap().to_string(), "\"ababab\"");
    assert_eq!(parse("\"ab\".repeat(0)").unwrap().eval().unwrap().to_string(), "\"\"");
    assert_eq!(parse("\"ab\".repeat(1)").unwrap().eval().unwrap().describe(), "\"ab\"");
    assert_eq!(parse("seq.repeat(0)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("[].repeat(0)").unwrap().eval().unwrap().describe(), "[]");
    assert_eq!(parse("[].repeat(1)").unwrap().eval().unwrap().describe(), "[]");
    assert_eq!(parse("[].repeat(10)").unwrap().eval().unwrap().describe(), "[]");
    assert_eq!(parse("\"\".repeat(0)").unwrap().eval().unwrap().describe(), "\"\"");
    assert_eq!(parse("\"\".repeat(1)").unwrap().eval().unwrap().describe(), "\"\"");
    assert_eq!(parse("\"\".repeat(10)").unwrap().eval().unwrap().describe(), "\"\"");
    assert_eq!(parse("1...").unwrap().eval().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
    assert_eq!(parse("\"ab\"...").unwrap().eval().unwrap().to_string(), "\"abababababababababab...");
    assert_eq!(parse("'a'...").unwrap().eval().unwrap().to_string(), "\"aaaaaaaaaaaaaaaaaaaa...");
    assert_eq!(parse("['a']...").unwrap().eval().unwrap().to_string(), "['a', 'a', 'a', 'a', 'a', ...]");

    assert_eq!(parse("\"abc\".repeat[10^10]").unwrap().eval().unwrap().to_string(), "'a'");
    assert_eq!(parse("[].repeat~1").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse(r#"("ab".repeat(10^10)~"cd".repeat(10^10))[4*10^10]"#).unwrap().eval().unwrap().to_string(), "'d'");

    assert!(parse("\"ab\".repeat.len").unwrap().eval().is_err());
    assert!(parse("1.repeat.len").unwrap().eval().is_err());
    test_len_exact(&parse("1.repeat(0)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("1.repeat(1)").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("1.repeat(3)").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("[].repeat").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("[].repeat(3)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("[1,2].repeat(0)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("[1,2].repeat(1)").unwrap().eval().unwrap(), 2);
    test_len_exact(&parse("[1,2].repeat(3)").unwrap().eval().unwrap(), 6);
    test_len_exact(&parse("seq.repeat(0)").unwrap().eval().unwrap(), 0);
    test_skip_n(&parse("1.repeat").unwrap().eval().unwrap());
    test_skip_n(&parse("1.repeat(10^10)").unwrap().eval().unwrap());
    test_skip_n(&parse("[].repeat").unwrap().eval().unwrap());
    test_skip_n(&parse("[].repeat(10^10)").unwrap().eval().unwrap());
    test_skip_n(&parse("[1,2].repeat").unwrap().eval().unwrap());
    test_skip_n(&parse("[1,2].repeat(10^10)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(10^10).repeat(10^10)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.repeat").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.repeat(0)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.repeat(1)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.repeat(2)").unwrap().eval().unwrap());
}


#[derive(Clone)]
struct Shift {
    source: BoxedStream,
    args: Vec<Item>,
    env: Rc<Env>
}

impl Shift {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_args_nonempty()?);
        let source = match node.source {
            Some(Item::Stream(s)) if s.is_string().can_be_true() => s.into(),
            Some(ref item) => return Err(StreamError::new(format!("expected string, found {:?}", item), node)),
            None => return Err(StreamError::new("source required", node))
        };
        Ok(Item::new_stream(Shift{source, args: node.args, env: Rc::clone(env)}))
    }

    fn helper(base: &Char, items: &[Item], env: &Rc<Env>) -> Result<Item, BaseError> {
        let abc = env.alphabet();
        let (index, case) = abc.ord_case(base)?;
        let ans = items.iter().try_fold(index.into(),
            |a, e| {
                match e {
                    Item::Number(ref num) => Ok(a + num),
                    Item::Char(ref ch) => Ok(a + abc.ord_case(ch)?.0),
                    _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                }
            })?;
        Ok(Item::new_char(abc.chr_case(&ans, case)))
    }
}

impl Describe for Shift {
    fn describe(&self) -> String {
        self.env.wrap_describe(Node::describe_helper(&("shift".into()), Some(&self.source), &self.args))
    }
}

impl Stream for Shift {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let base = self.source.string_iter();
        let args_iter = self.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(ShiftIter{base, source: &*self.source, args: &self.args, args_iter, env: &self.env})
    }

    fn is_string(&self) -> TriState {
        TriState::True
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

struct ShiftIter<'node> {
    base: StringIterator<'node>,
    source: &'node (dyn Stream + 'static),
    args: &'node Vec<Item>,
    args_iter: Vec<Box<dyn SIterator + 'node>>,
    env: &'node Rc<Env>
}

impl ShiftIter<'_> {
    fn node(&self) -> ENode {
        ENode {
            head: "shift".into(),
            source: Some(Item::Stream(self.source.clone_box())),
            args: self.args.clone()
        }
    }
}


impl Iterator for ShiftIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn aux_node(base: Char, mut inputs: Vec<Item>) -> Node {
            inputs.insert(0, Item::Char(base));
            Node {
                head: Head::Oper("+".into()),
                source: None,
                args: inputs.into_iter().map(Expr::from).collect()
            }
        }

        let ch = match self.base.next() {
            None => return None,
            Some(Ok(ch)) => ch,
            Some(Err(err)) => return Some(Err(err))
        };
        if !self.env.alphabet().contains(&ch) {
            return Some(Ok(Item::Char(ch)));
        }

        let rest = self.args_iter.iter_mut()
            .map(Iterator::next)
            .collect::<Option<Result<Vec<_>, _>>>();
        match rest {
            None => Some(Err(StreamError::new("some operand ended earlier than the source", self.node()))),
            Some(Ok(inputs)) => {
                match Shift::helper(&ch, &inputs, self.env) {
                    Ok(item) => Some(Ok(item)),
                    Err(err) => Some(Err(StreamError::new(err, aux_node(ch, inputs))))
                }
            },
            Some(Err(err)) => Some(Err(err))
        }
    }
}

impl SIterator for ShiftIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        let args_iter = self.args_iter.iter_mut();
        let mut n = n.to_owned();
        let mut n_chars = Number::zero();
        while n.is_positive() {
            match self.base.next() {
                Some(Ok(ch)) => {
                    if self.env.alphabet().contains(&ch) {
                        n_chars.inc();
                    }
                },
                Some(Err(err)) => return Err(err),
                None => return Ok(Some(n))
            }
            n.dec();
        }
        for iter in args_iter {
            if iter.skip_n(&n_chars)?.is_some() {
                return Err(StreamError::new("another operand ended earlier than the first", self.node()));
            }
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        self.base.len_remain()
    }
}

#[test]
fn test_shift() {
    use crate::parser::parse;
    assert_eq!(parse("\"AbC\".shift(3,[0,10,20])").unwrap().eval().unwrap().to_string(), "\"DoZ\"");
    assert_eq!(parse("\"Test\".shift(13,13)").unwrap().eval().unwrap().to_string(), "\"Test\"");
    assert_eq!(parse(r#""ahoj".shift("bebe")"#).unwrap().eval().unwrap().to_string(), "\"cmqo\"");
    assert_eq!(parse(r#""Hello world!".shift(seq)"#).unwrap().eval().unwrap().to_string(), r#""Igopt cvzun!""#);
    assert_eq!(parse(r#""Hello world!".shift("ab")"#).unwrap().eval().unwrap().to_string(), r#""Ig<!>"#);
    assert_eq!(parse(r#"("Hello world!".shift("ab"))[2]"#).unwrap().eval().unwrap().to_string(), "'g'");
    assert!(parse(r#"("Hello world!".shift("ab"))[3]"#).unwrap().eval().is_err());
    assert_eq!(parse(r#""Hello world!".shift([])"#).unwrap().eval().unwrap().to_string(), r#""<!>"#);
    assert_eq!(parse(r#""Hello world!".shift("ab".repeat)"#).unwrap().eval().unwrap().to_string(), r#""Igmnp yptmf!""#);
    assert_eq!(parse(r#""ab".repeat.shift(seq)"#).unwrap().eval().unwrap().to_string(), r#""bddffhhjjllnnpprrttv..."#);
    assert_eq!(parse(r#"("ab".repeat.shift(seq))[20]"#).unwrap().eval().unwrap().to_string(), "'v'");
    assert_eq!(parse(r#""abc".shift(['d',5,true])"#).unwrap().eval().unwrap().to_string(), "\"eg<!>");
    test_len_exact(&parse("\"abc\".shift(seq)").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("\"a b c!\".shift(1..3, 1)").unwrap().eval().unwrap(), 6);
    test_len_exact(&parse("\"\".shift(seq)").unwrap().eval().unwrap(), 0);
    test_skip_n(&parse(r#""abcdefghijk".shift(seq, "abcdefghijklmn")"#).unwrap().eval().unwrap());
    test_skip_n(&parse(r#""ab".repeat(10).shift(seq)"#).unwrap().eval().unwrap());
    test_skip_n(&parse(r#""a b".repeat(10).shift(seq)"#).unwrap().eval().unwrap());
}


#[derive(Clone)]
struct SelfRef {
    body: Expr,
    env: Rc<Env>
}

impl SelfRef {
    fn eval(mut node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        if node.source.is_some() {
            return Err(StreamError::new("no source accepted", node));
        }
        let body = match node.args[..] {
            [ref mut body] => std::mem::take(body),
            _ => return Err(StreamError::new("exactly 1 argument expected", node))
        };
        Ok(Item::Stream(Box::new(SelfRef{body: Self::replace_ref(body), env: Rc::clone(env)})))
    }

    fn eval_real(&self) -> Result<(Box<dyn Stream>, Rc<CacheHistory>), StreamError> {
        let mut env = (*self.env).clone();
        let hist = Rc::new(RefCell::new(Vec::new()));
        env.cache = Rc::downgrade(&hist);
        let item = self.body.clone().eval_env(&Rc::new(env))?;
        let stm = try_with!(self.body.clone(), item.to_stream()?);
        Ok((stm, hist))
    }

    fn replace_ref(expr: Expr) -> Expr {
        match expr {
            Expr::Imm(_) => expr,
            Expr::Eval(node) => match node.head {
                Head::Repl('%', None) => Expr::new_node(LangItem::BackRef, vec![]),
                _ => Expr::Eval(Node {
                    head: node.head,
                    source: node.source.map(|expr| Box::new(Self::replace_ref(*expr))),
                    args: node.args.into_iter()
                        .map(Self::replace_ref)
                        .collect()
                })
            }
        }
    }
}

impl Describe for SelfRef {
    fn describe(&self) -> String {
        format!("self{{{}}}", self.body.describe())
    }
}

impl Stream for SelfRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let (stm, hist) = match self.eval_real() {
            Ok((stm, hist)) => (Box::into_pin(stm), hist),
            Err(err) => return Box::new(std::iter::once(Err(err)))
        };
        let iter = unsafe { std::mem::transmute::<&dyn Stream, &dyn Stream>(&*stm) }.iter();
        Box::new(SelfRefIter {
            inner: iter,
            _stm: stm,
            hist
        })
    }

    fn is_string(&self) -> TriState {
        match self.eval_real() {
            Ok((stm, _)) => stm.is_string(),
            Err(_) => TriState::False
        }
    }
}

pub(crate) type CacheHistory = RefCell<Vec<Item>>;

struct SelfRefIter<'node> {
    inner: Box<dyn SIterator + 'node>,
    _stm: Pin<Box<dyn Stream>>,
    hist: Rc<CacheHistory>
}

impl Iterator for SelfRefIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next()? {
            Ok(item) => {
                self.hist.borrow_mut().push(item.clone());
                Some(Ok(item))
            },
            Err(err) => Some(Err(err))
        }
    }
}

impl SIterator for SelfRefIter<'_> { }

#[derive(Clone)]
struct BackRef {
    parent: Weak<CacheHistory>
}

struct BackRefIter {
    vec: Rc<CacheHistory>,
    pos: usize
}

impl BackRef {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        if node.source.is_some() {
            return Err(StreamError::new("no source accepted", node));
        }
        if !node.args.is_empty() {
            return Err(StreamError::new("no arguments accepted", node));
        }
        Ok(Item::Stream(Box::new(BackRef{parent: Weak::clone(&env.cache)})))
    }
}

impl Describe for BackRef {
    fn describe(&self) -> String {
        "%".to_owned()
    }
}

impl Stream for BackRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        match Weak::upgrade(&self.parent) {
            Some(rc) => Box::new(BackRefIter{vec: rc, pos: 0}),
            None => Box::new(std::iter::once(Err(StreamError::new("back-reference detached from cache", 
                        Node::new("%", None, vec![])))))
        }
    }

    fn is_string(&self) -> TriState {
        TriState::Either
    }

    fn length(&self) -> Length {
        Length::Unknown
    }
}

impl Iterator for BackRefIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let opos = self.pos;
        self.pos += 1;
        self.vec.borrow().get(opos).cloned().map(Result::Ok)
    }
}

impl SIterator for BackRefIter {}

#[test]
fn test_selfref() {
    use crate::parser::parse;
    assert_eq!(parse("self(%)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("self(%+1)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("self(%.repeat)").unwrap().eval().unwrap().to_string(), "[]");
    assert_eq!(parse("self(1~(%+1))").unwrap().eval().unwrap().to_string(), "[1, 2, 3, 4, 5, ...]");
    assert_eq!(parse("self(0~(1-%))").unwrap().eval().unwrap().to_string(), "[0, 1, 0, 1, 0, ...]");
    assert_eq!(parse("self(1~[%+1])").unwrap().eval().unwrap().to_string(), "[1, [2, [3, ...]]]");
    assert_eq!(parse("self([%])").unwrap().eval().unwrap().to_string(), "[[[[[[...]]]]]]");
    assert_eq!(parse("self([%]~1)[2]").unwrap().eval().unwrap().to_string(), "1");
    assert_eq!(parse("self(seq+(5~%))").unwrap().eval().unwrap().to_string(), "[6, 8, 11, 15, 20, ...]");
    assert_eq!(parse("self(\"pokus\".shift(\"ab\"~%))").unwrap().eval().unwrap().to_string(), "\"qqblu\"");
    assert_eq!(parse("self(%[1])").unwrap().eval().unwrap().to_string(), "[<!>");
    assert_eq!(parse("self(%.len)").unwrap().eval().unwrap().to_string(), "[<!>");
    test_len_exact(&parse("self(%)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("self(%~%)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("self(%:{#})").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("self(%.riffle(%))").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("self(%.repeat)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("self(\"pokus\".shift(\"ab\"~%))").unwrap().eval().unwrap(), 5);
    test_skip_n(&parse("self(1~(%+1))").unwrap().eval().unwrap());
}


#[derive(Clone)]
struct Riffle {
    source: BoxedStream,
    filler: Item
}

impl Riffle {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        let filler = match &mut node.args[..] {
            [f] => f,
            _ => return Err(StreamError::new("exactly 1 argument required", node))
        };
        let source: BoxedStream = match node.source {
            Some(Item::Stream(s)) => s.into(),
            Some(ref item) => return Err(StreamError::new(format!("expected stream, found {:?}", item), node)),
            _ => return Err(StreamError::new("source required", node))
        };
        let filler = std::mem::take(filler);
        if source.is_empty() {
            if source.is_string().is_true() { Ok(Item::new_stream(EmptyString())) }
            else { Ok(Item::new_stream(EmptyStream())) }
        } else {
            Ok(Item::new_stream(Riffle{source, filler}))
        }
    }
}

impl Describe for Riffle {
    fn describe(&self) -> String {
        Node::describe_helper(&("riffle".into()), Some(&self.source), std::slice::from_ref(&self.filler))
    }
}

impl Stream for Riffle {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let mut source_iter = self.source.iter();
        let filler_iter = match &self.filler {
            Item::Stream(stm) => stm.iter(),
            item => Box::new(std::iter::repeat(Ok(item.clone())))
        };
        let source_next = source_iter.next();
        Box::new(RiffleIter {
            source: source_iter,
            filler: filler_iter,
            source_next,
            which: RiffleState::Source
        })
    }

    fn is_string(&self) -> TriState {
        self.source.is_string()
    }

    fn length(&self) -> Length {
        use Length::*;
        let len1 = self.source.length();
        let len2 = match &self.filler {
            Item::Stream(stm) => stm.length(),
            _ => Infinite
        };
        Length::intersection(&len1.map(|u| 2 * u - 1), &len2.map(|v| 2 * v + 1))
    }

    fn is_empty(&self) -> bool {
        false
    }
}

struct RiffleIter<'node> {
    source: Box<dyn SIterator + 'node>,
    filler: Box<dyn SIterator + 'node>,
    source_next: Option<Result<Item, StreamError>>,
    which: RiffleState
}

#[derive(PartialEq)]
enum RiffleState {
    Source,
    Filler
}

impl Iterator for RiffleIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        use RiffleState::*;
        match self.which {
            Source => {
                let next = self.source_next.take()?;
                self.which = Filler;
                Some(next)
            },
            Filler => {
                self.source_next = self.source.next();
                self.which = Source;
                if self.source_next.is_none() { None } else { self.filler.next() }
            }
        }
    }
}

impl SIterator for RiffleIter<'_> {
    fn skip_n(&mut self, n: &Number) -> Result<Option<Number>, StreamError> {
        let common = Length::intersection(&self.source.len_remain(), &self.filler.len_remain());
        let skip = match Length::intersection(&common, &Length::Exact(n / 2)) {
            Length::Exact(len) => len,
            _ => Number::zero()
        };
        let mut remain = if !skip.is_zero() {
            self.filler.skip_n(&skip)?;
            match self.which {
                RiffleState::Source => {
                    self.source.skip_n(&(&skip - 1))?;
                    self.source_next = self.source.next();
                },
                RiffleState::Filler => {
                    self.source.skip_n(&skip)?;
                }
            };
            n - 2 * skip
        } else {
            n.to_owned()
        };
        while !remain.is_zero() {
            if self.next().transpose()?.is_none() {
                return Ok(Some(remain));
            }
            remain.dec();
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        let len1 = self.source.len_remain();
        let len2 = self.filler.len_remain();
        let common = Length::intersection(&len1, &len2);
        match self.which {
            RiffleState::Source => {
                if self.source_next.is_none() {
                    Length::Exact(Number::zero())
                } else {
                    common.map(|x| 2 * x + 1)
                }
            },
            RiffleState::Filler => {
                common.map(|x| 2 * x)
            }
        }
    }
}

#[test]
fn test_riffle() {
    use crate::parser::parse;
    assert_eq!(parse("seq.riffle(seq + 3)").unwrap().eval().unwrap().to_string(), "[1, 4, 2, 5, 3, ...]");
    assert_eq!(parse("0.repeat.riffle(1)").unwrap().eval().unwrap().to_string(), "[0, 1, 0, 1, 0, ...]");
    assert_eq!(parse("[1,2,3].riffle('a')").unwrap().eval().unwrap().to_string(), "[1, 'a', 2, 'a', 3]");
    assert_eq!(parse("seq.riffle(['a'])").unwrap().eval().unwrap().to_string(), "[1, 'a', 2]");
    assert_eq!(parse("seq.riffle([])").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("[1,2].riffle(['a', 'b'])").unwrap().eval().unwrap().to_string(), "[1, 'a', 2]");
    assert_eq!(parse("['a','b'].riffle(seq)").unwrap().eval().unwrap().to_string(), "['a', 1, 'b']");
    assert_eq!(parse("\"abc\".riffle(',')").unwrap().eval().unwrap().to_string(), "\"a,b,c\"");
    assert_eq!(parse("\"abc\".riffle(0)").unwrap().eval().unwrap().to_string(), "\"a<!>");
    assert!(parse("1.riffle(2)").unwrap().eval().is_err());
    test_len_exact(&parse("[1,2,3].riffle('a')").unwrap().eval().unwrap(), 5);
    test_len_exact(&parse("[1,2,3].riffle(['a'])").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("[1,2,3].riffle([])").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("seq.riffle(['a'])").unwrap().eval().unwrap(), 3);
    test_len_exact(&parse("seq.riffle([])").unwrap().eval().unwrap(), 1);
    test_len_exact(&parse("[].riffle(0)").unwrap().eval().unwrap(), 0);
    test_len_exact(&parse("\"\".riffle(0)").unwrap().eval().unwrap(), 0);
    test_skip_n(&parse("seq.riffle(seq)").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.riffle(range(100))").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.riffle([])").unwrap().eval().unwrap());
    test_skip_n(&parse("seq.riffle(0)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(seq)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(0)").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(range(50))").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(range(99))").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(range(100))").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(range(101))").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle(range(200))").unwrap().eval().unwrap());
    test_skip_n(&parse("range(100).riffle([])").unwrap().eval().unwrap());
    test_skip_n(&parse("[].riffle(range(3))").unwrap().eval().unwrap());
    test_skip_n(&parse("[].riffle(range(1))").unwrap().eval().unwrap());
    test_skip_n(&parse("[].riffle([])").unwrap().eval().unwrap());
}


pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("seq", Seq::eval);
    keywords.insert("range", Range::eval);
    keywords.insert("..", Range::eval);
    keywords.insert("len", eval_len);
    keywords.insert("repeat", Repeat::eval);
    keywords.insert("shift", Shift::eval);
    keywords.insert("self", SelfRef::eval);
    keywords.insert("riffle", Riffle::eval);
    keywords.insert("$backref", BackRef::eval);
}
