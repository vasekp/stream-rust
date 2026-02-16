use crate::base::*;
use crate::utils::unsign;

fn eval_padl(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve_source()?;
    match node {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Two(Item::Number(len), item) }
        if !len.is_negative() =>
            Ok(Item::new_stream(PadLeft { source: stm.into(), len: unsign(len), padding: item, head })),
        RNodeS { head, source: Item::String(s), args: RArgs::Two(Item::Number(len), Item::Char(ch)) }
        if !len.is_negative() =>
            Ok(Item::new_string(PadLeft { source: s.into(), len: unsign(len), padding: ch, head })),
        _ => Err(StreamError::new("expected: stream.padleft(length, item) or string.padleft(length, char)", node))
    }
}

fn eval_padr(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve_source()?;
    match node {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::Two(Item::Number(len), item) }
        if !len.is_negative() =>
            Ok(Item::new_stream(PadRight { source: stm.into(), len: unsign(len), padding: item, head })),
        RNodeS { head, source: Item::String(s), args: RArgs::Two(Item::Number(len), Item::Char(ch)) }
        if !len.is_negative() =>
            Ok(Item::new_string(PadRight { source: s.into(), len: unsign(len), padding: ch, head })),
        _ => Err(StreamError::new("expected: stream.padright(length, item) or string.padright(length, char)", node))
    }
}

#[derive(Clone)]
struct PadLeft<I: ItemType> {
    source: BoxedStream<I>,
    len: UNumber,
    padding: I,
    head: Head
}

impl<I: ItemType> Describe for PadLeft<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.len)
            .push_arg(&self.padding)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for PadLeft<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        if self.source.len() == Length::Infinite {
            self.source.iter()
        } else {
            let len = match self.source.try_count() {
                Ok(count) => count,
                Err(err) => return Box::new(std::iter::once(Err(err)))
            };
            let pad_remain = if len < self.len { &self.len - &len } else { UNumber::zero() };
            Box::new(PadLeftIter {
                source: self.source.iter(),
                pad_remain,
                padding: &self.padding
            })
        }
    }

    fn len(&self) -> Length {
        match self.source.len() {
            Length::Exact(len) | Length::AtMost(len) if len <= self.len
                => Length::Exact(self.len.clone()),
            Length::Exact(len) => Length::Exact(len),
            Length::Infinite => Length::Infinite,
            Length::AtMost(_) | Length::UnknownFinite => Length::UnknownFinite,
            Length::Unknown => Length::Unknown
        }
    }
}

struct PadLeftIter<'node, I: ItemType> {
    source: Box<dyn SIterator<I> + 'node>,
    pad_remain: UNumber,
    padding: &'node I,
}

impl<I: ItemType> Iterator for PadLeftIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.pad_remain.is_zero() {
            self.pad_remain.dec();
            Some(Ok(self.padding.clone()))
        } else {
            self.source.next()
        }
    }
}

impl<I: ItemType> SIterator<I> for PadLeftIter<'_, I> {
    fn len_remain(&self) -> Length {
        self.source.len_remain() + &self.pad_remain
    }

    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if n < self.pad_remain {
            self.pad_remain -= n;
            Ok(None)
        } else {
            if !self.pad_remain.is_zero() {
                n -= &self.pad_remain;
                self.pad_remain = UNumber::zero();
            }
            self.source.advance(n)
        }
    }
}

#[derive(Clone)]
struct PadRight<I: ItemType> {
    source: BoxedStream<I>,
    len: UNumber,
    padding: I,
    head: Head
}

impl<I: ItemType> Describe for PadRight<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.len)
            .push_arg(&self.padding)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for PadRight<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        if self.source.len() == Length::Infinite {
            self.source.iter()
        } else {
            Box::new(PadRightIter {
                source: Some(self.source.iter()),
                len: &self.len,
                pos: UNumber::zero(),
                padding: &self.padding
            })
        }
    }

    fn len(&self) -> Length {
        match self.source.len() {
            Length::Exact(len) | Length::AtMost(len) if len <= self.len
                => Length::Exact(self.len.clone()),
            Length::Exact(len) => Length::Exact(len),
            Length::Infinite => Length::Infinite,
            Length::AtMost(_) | Length::UnknownFinite => Length::UnknownFinite,
            Length::Unknown => Length::Unknown
        }
    }
}

struct PadRightIter<'node, I: ItemType> {
    source: Option<Box<dyn SIterator<I> + 'node>>,
    len: &'node UNumber,
    pos: UNumber,
    padding: &'node I,
}

impl<I: ItemType> Iterator for PadRightIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ref mut iter) = self.source {
            if let Some(res) = iter.next() {
                self.pos.inc();
                return Some(res);
            } else {
                self.source = None;
            }
        }
        if &self.pos < self.len {
            self.pos.inc();
            Some(Ok(self.padding.clone()))
        } else {
            None
        }
    }
}

impl<I: ItemType> SIterator<I> for PadRightIter<'_, I> {
    fn len_remain(&self) -> Length {
        match &self.source {
            Some(iter) => iter.len_remain().map(|len|
                std::cmp::max(len, &(self.len - &self.pos)).to_owned()),
            None => Length::Exact(self.len - &self.pos)
        }
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.pos += &n;
        if let Some(ref mut iter) = self.source {
            if iter.advance(n)?.is_none() {
                return Ok(None);
            } else {
                self.source = None;
            }
        }
        if &self.pos > self.len {
            Ok(Some(&self.pos - self.len))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_padleft() {
        use super::*;
        test_eval!("[].padleft(0, 0)" => "[]");
        test_eval!("[].padleft(3, 0)" => "[0, 0, 0]");
        test_eval!("(1..2).padleft(3, '0')" => "['0', 1, 2]");
        test_eval!("(1..5).padleft(3, 0)" => "[1, 2, 3, 4, 5]");
        test_eval!("seq.padleft(3, 0)" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("range(3, 10^10).padleft(10^10, \"\")" => "[\"\", \"\", 3, 4, 5, ...]");
        test_eval!("\"ab\".padleft(3, '0')" => "\"0ab\"");
        test_eval!("\"ab\".padleft(3, 0)" => err);
        test_len!("(1..2).padleft(3, 0)" => 3);
        test_len!("(1..5).padleft(3, 0)" => 5);
        test_advance("[].padleft(100, 0)");
        test_advance("range(50,100).padleft(100, 0)");
        test_advance("range(100).padleft(100, 0)");
        test_describe!("(1..2).padleft(3, '0')" => "(1..2).padleft(3, '0')");

        test_eval!("[].padright(0, 0)" => "[]");
        test_eval!("[].padright(3, 0)" => "[0, 0, 0]");
        test_eval!("(1..2).padright(3, '0')" => "[1, 2, '0']");
        test_eval!("(1..5).padright(3, 0)" => "[1, 2, 3, 4, 5]");
        test_eval!("seq.padright(3, 0)" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("range(3).padright(10^10, [])" => "[1, 2, 3, [], [], ...]");
        test_eval!("range(3, 10^10).padright(10^10, []).last(5)"
            => "[9999999998, 9999999999, 10000000000, [], []]");
        test_eval!("\"ab\".padright(3, '0')" => "\"ab0\"");
        test_eval!("\"ab\".padright(3, 0)" => err);
        test_len!("(1..2).padright(3, 0)" => 3);
        test_len!("(1..5).padright(3, 0)" => 5);
        test_advance("[].padright(100, 0)");
        test_advance("range(50,100).padright(100, 0)");
        test_advance("range(100).padright(100, 0)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_with_docs(["padleft", "padl"], eval_padl, r#"
The input `stream` (or `string`) left-padded to `length` using copies of `item` (or `char`, 
respectively).
If the input length is already `length` or more, returns unchanged.
= stream.?(length, item)
= string.?(length, char)
> [1, 2].?(5, 0) => [0, 0, 0, 1, 2]
> "ab".?(5, ' ') => "   ab"
: padright
"#);
    symbols.insert_with_docs(["padright", "padr"], eval_padr, r#"
The input `stream` (or `string`) right-padded to `length` using copies of `item` (or `char`, 
respectively).
If the input length is already `length` or more, returns unchanged.
= stream.?(length, item)
= string.?(length, char)
> [1, 2].?(5, 0) => [1, 2, 0, 0, 0]
> "ab".?(5, ' ') => "ab   "
: padright
"#);
}
