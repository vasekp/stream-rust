use crate::base::*;

fn eval_padl(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let [Item::Number(len), item] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    let len = len.try_unsign()?;
    match (node.source_checked()?, item) {
        (Item::Stream(stm), item) =>
            Ok(Item::new_stream(PadLeft { source: Rc::clone(stm), len, padding: item.clone(), head: node.head.clone() })),
        (Item::String(stm), Item::Char(ch)) =>
            Ok(Item::new_string(PadLeft { source: Rc::clone(stm), len, padding: *ch, head: node.head.clone() })),
        _ => Err(StreamError::usage(&node.head))
    }
}

fn eval_padr(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let [Item::Number(len), item] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    let len = len.try_unsign()?;
    match (node.source_checked()?, item) {
        (Item::Stream(stm), item) =>
            Ok(Item::new_stream(PadRight { source: Rc::clone(stm), len, padding: item.clone(), head: node.head.clone() })),
        (Item::String(stm), Item::Char(ch)) =>
            Ok(Item::new_string(PadRight { source: Rc::clone(stm), len, padding: *ch, head: node.head.clone() })),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct PadLeft<I: ItemType> {
    source: Rc<dyn Stream<I>>,
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
    fn into_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        if self.source.len() == Length::Infinite {
            return self.source.iter();
        }
        let len = match self.source.try_count() {
            Ok(len) => len,
            Err(err) => return iter_error(err, &self),
        };
        let pad_remain = if len < self.len {
            &self.len - &len
        } else {
            UNumber::zero()
        };
        PadLeftIter {
            source: self.source.iter(),
            pad_remain,
            node: self,
        }.wrap()
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

struct PadLeftIter<I: ItemType> {
    node: Rc<PadLeft<I>>,
    source: Box<dyn SIterator<I>>,
    pad_remain: UNumber,
}

impl<I: ItemType> PreIterator<I> for PadLeftIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        if !self.pad_remain.is_zero() {
            self.pad_remain -= 1;
            Ok(Some(self.node.padding.clone()))
        } else {
            self.source.next()
        }
    }

    fn len_remain(&self) -> Length {
        self.source.len_remain() + &self.pad_remain
    }

    fn advance(&mut self, mut n: UNumber) -> SResult<Option<UNumber>> {
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

    fn origin(&self) -> &Rc<PadLeft<I>> {
        &self.node
    }
}

struct PadRight<I: ItemType> {
    source: Rc<dyn Stream<I>>,
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
    fn into_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        if self.source.len() == Length::Infinite {
            self.source.iter()
        } else {
            PadRightIter {
                source: Some(self.source.iter()),
                pos: UNumber::zero(),
                node: self,
            }.wrap()
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

struct PadRightIter<I: ItemType> {
    node: Rc<PadRight<I>>,
    source: Option<Box<dyn SIterator<I>>>,
    pos: UNumber,
}

impl<I: ItemType> PreIterator<I> for PadRightIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        if let Some(ref mut iter) = self.source {
            if let Some(res) = iter.next()? {
                self.pos += 1;
                return Ok(Some(res));
            } else {
                self.source = None;
            }
        }
        if &self.pos < &self.node.len {
            self.pos += 1;
            Ok(Some(self.node.padding.clone()))
        } else {
            Ok(None)
        }
    }

    fn len_remain(&self) -> Length {
        match &self.source {
            Some(iter) => iter.len_remain().map(|len|
                std::cmp::max(len, &(&self.node.len - &self.pos)).to_owned()),
            None => Length::Exact(&self.node.len - &self.pos)
        }
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.pos += &n;
        if let Some(ref mut iter) = self.source {
            if iter.advance(n)?.is_none() {
                return Ok(None);
            } else {
                self.source = None;
            }
        }
        if &self.pos > &self.node.len {
            Ok(Some(&self.pos - &self.node.len))
        } else {
            Ok(None)
        }
    }

    fn origin(&self) -> &Rc<PadRight<I>> {
        &self.node
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
    symbols.insert(["padleft", "padl"], eval_padl, r#"
Left-pads the input `stream` (or `string`) to `length` using copies of `item` (or `char`, respectively).
If the input length is already `length` or more, returns unchanged.
= stream.?(length, item)
= string.?(length, char)
> [1, 2].?(5, 0) => [0, 0, 0, 1, 2]
> "ab".?(5, ' ') => "   ab"
: padright
"#);
    symbols.insert(["padright", "padr"], eval_padr, r#"
Right-pads the input `stream` (or `string`) to `length` using copies of `item` (or `char`, respectively).
If the input length is already `length` or more, returns unchanged.
= stream.?(length, item)
= string.?(length, char)
> [1, 2].?(5, 0) => [1, 2, 0, 0, 0]
> "ab".?(5, ' ') => "ab   "
: padright
"#);
}
