use crate::base::*;
use crate::utils::unsign;

#[derive(Clone)]
pub struct Repeat {
    head: Head,
    item: Item,
    count: Option<UNumber>
}

impl Repeat {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        let (item, count) = match rnode {
            RNodeS { source, args: RArgs::Zero, .. }
                => (source, None),
            RNodeS { source, args: RArgs::One(Item::Number(count)), .. } if !count.is_negative()
                => (source, Some(unsign(count))),
            _ => return Err(StreamError::new("expected one of: source.repeat(), source.repeat(count)", rnode))
        };
        if let Item::Stream(ref stm) | Item::String(ref stm) = &item {
            if stm.is_empty() || count.as_ref().is_some_and(UNumber::is_zero) {
                return Ok(Item::empty_stream_or_string(item.is_string()));
            }
            if count.as_ref().is_some_and(One::is_one) {
                return Ok(item);
            }
        } else if count.as_ref().is_some_and(UNumber::is_zero) {
            return Ok(Item::empty_stream());
        }
        match item {
            Item::Char(_) | Item::String(_) => Ok(Item::new_string_stream(Repeat{head: rnode.head, item, count})),
            _ => Ok(Item::new_stream(Repeat{head: rnode.head, item, count}))
        }
    }
}

impl Stream for Repeat {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        match &self.item {
            Item::Stream(stream) | Item::String(stream) => Box::new(RepeatStreamIter {
                stream: &**stream,
                iter: stream.iter(),
                len: stream.length(),
                resets_rem: self.count.as_ref()
                    .map(|count| count - 1u32)
            }),
            item => match &self.count {
                Some(count) => Box::new(RepeatItemIter{item, count_rem: count.to_owned()}),
                None => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }
        }
    }

    fn length(&self) -> Length {
        use Length::*;
        if self.count == Some(UNumber::zero()) { return Exact(UNumber::zero()); }
        match &self.item {
            Item::Stream(stream) | Item::String(stream) => {
                if stream.is_empty() { return Exact(UNumber::zero()); }
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
}

impl Describe for Repeat {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.item), &self.count, prec, env)
    }
}

struct RepeatItemIter<'node> {
    item: &'node Item,
    count_rem: UNumber // None covered by std::iter::repeat_with
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
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if n > self.count_rem {
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
    resets_rem: Option<UNumber>
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
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
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
        let (full_length, mut n) = match self.iter.skip_n(n.clone())? {
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
        self.iter.skip_n(n)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repeat() {
        use crate::parser::parse;

        assert_eq!(parse("1.repeat").unwrap().eval_default().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
        assert_eq!(parse("1.repeat(1)").unwrap().eval_default().unwrap().to_string(), "[1]");
        assert_eq!(parse("1.repeat(3)").unwrap().eval_default().unwrap().to_string(), "[1, 1, 1]");
        assert_eq!(parse("1.repeat(0)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert!(parse("1.repeat(-1)").unwrap().eval_default().is_err());
        assert_eq!(parse("(1..2).repeat(2)").unwrap().eval_default().unwrap().to_string(), "[1, 2, 1, 2]");
        assert_eq!(parse("[1, 2].repeat(1)").unwrap().eval_default().unwrap().to_string(), "[1, 2]");
        assert_eq!(parse("'a'.repeat").unwrap().eval_default().unwrap().to_string(), "\"aaaaaaaaaaaaaaaaaaaa...");
        assert_eq!(parse("\"ab\".repeat").unwrap().eval_default().unwrap().to_string(), "\"abababababababababab...");
        assert_eq!(parse("\"ab\".repeat(3)").unwrap().eval_default().unwrap().to_string(), "\"ababab\"");
        assert_eq!(parse("\"ab\".repeat(0)").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("\"ab\".repeat(1)").unwrap().eval_default().unwrap().to_string(), "\"ab\"");
        assert_eq!(parse("seq.repeat(0)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("[].repeat(0)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("[].repeat(1)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("[].repeat(10)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("\"\".repeat(0)").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("\"\".repeat(1)").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("\"\".repeat(10)").unwrap().eval_default().unwrap().to_string(), "\"\"");

        assert_eq!(parse("\"abc\".repeat[10^10]").unwrap().eval_default().unwrap().to_string(), "'a'");
        assert_eq!(parse("[].repeat~1").unwrap().eval_default().unwrap().to_string(), "[1]");
        assert_eq!(parse(r#"("ab".repeat(10^10)~"cd".repeat(10^10))[4*10^10]"#).unwrap().eval_default().unwrap().to_string(), "'d'");

        assert!(parse("\"ab\".repeat.len").unwrap().eval_default().is_err());
        assert!(parse("1.repeat.len").unwrap().eval_default().is_err());
        test_len_exact(&parse("1.repeat(0)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("1.repeat(1)").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("1.repeat(3)").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("[].repeat").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("[].repeat(3)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("[1,2].repeat(0)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("[1,2].repeat(1)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("[1,2].repeat(3)").unwrap().eval_default().unwrap(), 6);
        test_len_exact(&parse("seq.repeat(0)").unwrap().eval_default().unwrap(), 0);
        test_skip_n(&parse("1.repeat").unwrap().eval_default().unwrap());
        test_skip_n(&parse("1.repeat(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("[].repeat").unwrap().eval_default().unwrap());
        test_skip_n(&parse("[].repeat(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("[1,2].repeat").unwrap().eval_default().unwrap());
        test_skip_n(&parse("[1,2].repeat(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^10).repeat(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq.repeat").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq.repeat(0)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq.repeat(1)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq.repeat(2)").unwrap().eval_default().unwrap());

        assert_eq!(parse("1.repeat").unwrap().eval_default().unwrap().describe(), "1.repeat");
        assert_eq!(parse("1.repeat(1)").unwrap().eval_default().unwrap().describe(), "1.repeat(1)");
        assert_eq!(parse("1.repeat(0)").unwrap().eval_default().unwrap().describe(), "[]");
        assert_eq!(parse("[1, 2].repeat(1)").unwrap().eval_default().unwrap().describe(), "[1, 2]");
        assert_eq!(parse("\"ab\".repeat(1)").unwrap().eval_default().unwrap().describe(), "\"ab\"");
        assert_eq!(parse("seq.repeat(1)").unwrap().eval_default().unwrap().describe(), "seq");
        assert_eq!(parse("[].repeat(0)").unwrap().eval_default().unwrap().describe(), "[]");
        assert_eq!(parse("[].repeat(1)").unwrap().eval_default().unwrap().describe(), "[]");
        assert_eq!(parse("[].repeat(10)").unwrap().eval_default().unwrap().describe(), "[]");
        assert_eq!(parse("\"\".repeat(0)").unwrap().eval_default().unwrap().describe(), "\"\"");
        assert_eq!(parse("\"\".repeat(1)").unwrap().eval_default().unwrap().describe(), "\"\"");
        assert_eq!(parse("\"\".repeat(10)").unwrap().eval_default().unwrap().describe(), "\"\"");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("repeat", Repeat::eval);
}
