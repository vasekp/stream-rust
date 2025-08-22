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

#[derive(Clone)]
struct PadLeft<I: ItemType> {
    source: BoxedStream<I>,
    len: UNumber,
    padding: I,
    head: Head
}

impl<I: ItemType> Describe for PadLeft<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source),
            [&Item::Number(self.len.clone().into()), &self.padding.clone().into()],
            prec, env)
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

#[cfg(test)]
mod tests {
    #[test]
    fn test_padleft() {
        use super::*;
        use crate::parser::parse;
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
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("padleft", eval_padl);
    keywords.insert("padl", eval_padl);
}
