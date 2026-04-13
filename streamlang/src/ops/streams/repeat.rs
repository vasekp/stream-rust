use crate::base::*;

fn eval_repeat(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let item = node.source_checked()?;
    let count = match &node.args[..] {
        [] => None,
        [Item::Number(count)] => Some(count.try_unsign()?),
        _ => return Err(StreamError::usage(&node.head))
    };
    match (&item, count.as_ref().and_then(|x| u32::try_from(x).ok())) {
        (Item::String(stm), _) if stm.is_empty()? => Ok(item.clone()),
        (Item::Stream(stm), _) if stm.is_empty()? => Ok(item.clone()),
        (Item::Char(_) | Item::String(_), Some(0)) => Ok(Item::empty_string()),
        (_, Some(0)) => Ok(Item::empty_stream()),
        (Item::Stream(_) | Item::String(_), Some(1)) => Ok(item.clone()),
        (Item::Stream(stm), _) => Ok(Item::new_stream(RepeatStream{head: node.head.clone(), stream: Rc::clone(stm), count})),
        (Item::String(stm), _) => Ok(Item::new_string(RepeatStream{head: node.head.clone(), stream: Rc::clone(stm), count})),
        (Item::Char(ch), _) => Ok(Item::new_string(RepeatItem{head: node.head.clone(), item: *ch, count})),
        (_, Some(1)) => Ok(Item::new_stream(List::from(vec![item.clone()]))),
        _ => Ok(Item::new_stream(RepeatItem{head: node.head.clone(), item: item.clone(), count}))
    }
}

pub struct RepeatItem<I: ItemType> {
    head: Head,
    item: I,
    count: Option<UNumber>
}

struct RepeatItemIter<I: ItemType> {
    node: Rc<RepeatItem<I>>,
    count_rem: UNumber // None covered by std::iter::repeat_with
}


impl<I: ItemType> Stream<I> for RepeatItem<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        match &self.count {
            Some(count) => RepeatItemIter{count_rem: count.to_owned(), node: self}.wrap(),
            None => Box::new(std::iter::repeat(Ok(self.item.clone()))),
        }
    }

    fn len(&self) -> Length {
        use Length::*;
        match &self.count {
            Some(count) => Exact(count.to_owned()),
            None => Infinite
        }
    }
}

impl<I: ItemType> Describe for RepeatItem<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.item)
            .push_args(&self.count)
            .finish(prec)
    }
}

impl<I: ItemType> PreIterator<I> for RepeatItemIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        if !self.count_rem.is_zero() {
            self.count_rem -= 1;
            Ok(Some(self.node.item.clone()))
        } else {
            Ok(None)
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        if n > &self.count_rem {
            Ok(Some(n - &self.count_rem))
        } else {
            self.count_rem -= n;
            Ok(None)
        }
    }

    fn origin(&self) -> &Rc<RepeatItem<I>> {
        &self.node
    }
}

pub struct RepeatStream<I: ItemType> {
    head: Head,
    stream: Rc<dyn Stream<I>>,
    count: Option<UNumber>
}

impl<I: ItemType> Stream<I> for RepeatStream<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        RepeatStreamIter {
            iter: self.stream.iter(),
            resets_rem: self.count.as_ref()
                .map(|count| count - 1u32),
            node: self,
        }.wrap()
    }

    fn len(&self) -> Length {
        use Length::*;
        if self.count == Some(UNumber::zero()) { return Exact(UNumber::zero()); }
        match (self.stream.len(), &self.count) {
            (_, None) | (Infinite, _) => Infinite,
            (Exact(len), Some(count)) => Exact(len * count),
            (AtMost(len), Some(count)) => AtMost(len * count),
            (UnknownFinite, Some(_)) => UnknownFinite,
            (Unknown, Some(_)) => Unknown
        }
    }
}

impl<I: ItemType> Describe for RepeatStream<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.stream)
            .push_args(&self.count)
            .finish(prec)
    }
}

struct RepeatStreamIter<I: ItemType> {
    node: Rc<RepeatStream<I>>,
    iter: Box<dyn SIterator<I>>,
    resets_rem: Option<UNumber>
}

impl<I: ItemType> PreIterator<I> for RepeatStreamIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        let next = self.iter.next()?;
        if next.is_some() {
            return Ok(next);
        }
        if let Some(ref mut count) = self.resets_rem {
            if count.is_zero() {
                return Ok(None);
            }
            *count -= 1;
        }
        self.iter = self.node.stream.iter();
        self.iter.next()
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let Some(n) = self.iter.advance(n)? else { return Ok(None); };

        // If advance returned Some, iter is depleted. Restart.
        if let Some(ref mut count) = self.resets_rem {
            if count.is_zero() {
                return Ok(Some(n));
            }
            *count -= 1;
        }
        self.iter = self.node.stream.iter();

        // This point is special: we know that iter() is now newly initiated, so we can use it to
        // determine the length regardless of whether it's statically known.
        let (full_length, mut n) = match self.iter.advance(&n)? {
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
            *count -= 1;
        }
        self.iter = self.node.stream.iter();
        debug_assert!(n < full_length);
        self.iter.advance(&n)
    }

    fn origin(&self) -> &Rc<RepeatStream<I>> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repeat() {
        test_eval!("1.repeat" => "[1, 1, 1, 1, 1, ...]");
        test_eval!("1.repeat(1)" => "[1]");
        test_eval!("1.repeat(3)" => "[1, 1, 1]");
        test_eval!("1.repeat(0)" => "[]");
        test_eval!("1.repeat(-1)" => err);
        test_eval!("(1..2).repeat(2)" => "[1, 2, 1, 2]");
        test_eval!("[1, 2].repeat(1)" => "[1, 2]");
        test_eval!("'a'.repeat" => "\"aaaaaaaaaaaaaaaaaaaa...");
        test_eval!("'a'.repeat(1)" => "\"a\"");
        test_eval!("'a'.repeat(0)" => "\"\"");
        test_eval!("\"ab\".repeat" => "\"abababababababababab...");
        test_eval!("\"ab\".repeat(3)" => "\"ababab\"");
        test_eval!("\"ab\".repeat(0)" => "\"\"");
        test_eval!("\"ab\".repeat(1)" => "\"ab\"");
        test_eval!("seq.repeat(0)" => "[]");
        test_eval!("[].repeat(0)" => "[]");
        test_eval!("[].repeat(1)" => "[]");
        test_eval!("[].repeat(10)" => "[]");
        test_eval!("[].repeat" => "[]");
        test_eval!("\"\".repeat(0)" => "\"\"");
        test_eval!("\"\".repeat(1)" => "\"\"");
        test_eval!("\"\".repeat(10)" => "\"\"");
        test_eval!("\"\".repeat" => "\"\"");

        test_eval!("\"abc\".repeat[10^10]" => "'a'");
        test_eval!("[].repeat~1" => "[1]");
        test_eval!(r#"("ab".repeat(10^10)~"cd".repeat(10^10))[4*10^10]"# => "'d'");

        test_eval!("\"ab\".repeat.len" => err);
        test_eval!("1.repeat.len" => err);
        test_len!("1.repeat(0)" => 0);
        test_len!("1.repeat(1)" => 1);
        test_len!("1.repeat(3)" => 3);
        test_len!("[].repeat" => 0);
        test_len!("[].repeat(3)" => 0);
        test_len!("[1,2].repeat(0)" => 0);
        test_len!("[1,2].repeat(1)" => 2);
        test_len!("[1,2].repeat(3)" => 6);
        test_len!("seq.repeat(0)" => 0);
        test_advance("1.repeat");
        test_advance("1.repeat(10^10)");
        test_advance("[].repeat");
        test_advance("[].repeat(10^10)");
        test_advance("[1,2].repeat");
        test_advance("[1,2].repeat(10^10)");
        test_advance("range(10^10).repeat(10^10)");
        test_advance("seq.repeat");
        test_advance("seq.repeat(0)");
        test_advance("seq.repeat(1)");
        test_advance("seq.repeat(2)");

        test_describe!("1.repeat" => "1.repeat");
        test_describe!("1.repeat(1)" => "[1]");
        test_describe!("1.repeat(0)" => "[]");
        test_describe!("[1, 2].repeat(1)" => "[1, 2]");
        test_describe!("\"ab\".repeat(1)" => "\"ab\"");
        test_describe!("'a'.repeat(0)" => "\"\"");
        test_describe!("seq.repeat(1)" => "seq");
        test_describe!("[].repeat(0)" => "[]");
        test_describe!("[].repeat(1)" => "[]");
        test_describe!("[].repeat(10)" => "[]");
        test_describe!("\"\".repeat(0)" => "\"\"");
        test_describe!("\"\".repeat(1)" => "\"\"");
        test_describe!("\"\".repeat(10)" => "\"\"");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("repeat", eval_repeat, r#"
The `item` repeated `count`times.
If the `item` is a stream, concatenates the repetitions in a single stream.
If the `item` is a character or a string, evaluates to a string.
If `count` is omitted, repeats indefinitely.
= item.?(count)
= item.?
> 1.? => [1, 1, 1, 1, 1, ...]
> 1.?(3) => [1, 1, 1]
> [1, 2].?(2) => [1, 2, 1, 2]
> 'a'.?(5) => "aaaaa"
> "ab".? => "abababababababababab...
"#);
}
