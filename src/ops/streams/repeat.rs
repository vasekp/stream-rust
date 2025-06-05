use crate::base::*;
use crate::utils::unsign;

fn eval_repeat(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (item, count) = match rnode {
        RNodeS { source, args: RArgs::Zero, .. }
            => (source, None),
        RNodeS { source, args: RArgs::One(Item::Number(count)), .. } if !count.is_negative()
            => (source, Some(unsign(count))),
        _ => return Err(StreamError::new("expected one of: source.repeat(), source.repeat(count)", rnode))
    };
    match (&item, count.as_ref().and_then(UNumber::to_u32)) {
        (Item::String(stm), _) if stm.is_empty() => Ok(item),
        (Item::Stream(stm), _) if stm.is_empty() => Ok(item),
        (Item::Char(_) | Item::String(_), Some(0)) => Ok(Item::empty_string()),
        (_, Some(0)) => Ok(Item::empty_stream()),
        (Item::Stream(_) | Item::String(_), Some(1)) => Ok(item),
        (Item::Stream(_), _) => {
            let Item::Stream(stm) = item else { unreachable!() };
            Ok(Item::new_stream(RepeatStream{head: rnode.head, stream: stm.into(), count}))
        },
        (Item::String(_), _) => {
            let Item::String(stm) = item else { unreachable!() };
            Ok(Item::new_string_stream(RepeatStream{head: rnode.head, stream: stm.into(), count}))
        },
        (Item::Char(_), _) => {
            let Item::Char(ch) = item else { unreachable!() };
            Ok(Item::new_string_stream(RepeatItem{head: rnode.head, item: ch, count}))
        },
        (_, Some(1)) => Ok(Item::new_stream(List::from(vec![item]))),
        _ => Ok(Item::new_stream(RepeatItem{head: rnode.head, item, count}))
    }
}

#[derive(Clone)]
pub struct RepeatItem<ItemType: ItemTypeT> {
    head: Head,
    item: ItemType,
    count: Option<UNumber>
}

struct RepeatItemIter<'node, ItemType: ItemTypeT> {
    item: &'node ItemType,
    count_rem: UNumber // None covered by std::iter::repeat_with
}


impl<ItemType: ItemTypeT> Stream<ItemType> for RepeatItem<ItemType> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<ItemType> + 'node> {
        match &self.count {
            Some(count) => Box::new(RepeatItemIter{item: &self.item, count_rem: count.to_owned()}),
            None => Box::new(std::iter::repeat_with(|| Ok(self.item.clone())))
        }
    }

    fn length(&self) -> Length {
        use Length::*;
        match &self.count {
            Some(count) => Exact(count.to_owned()),
            None => Infinite
        }
    }
}

impl<ItemType: ItemTypeT> Describe for RepeatItem<ItemType> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.item), &self.count, prec, env)
    }
}

impl<ItemType: ItemTypeT> Iterator for RepeatItemIter<'_, ItemType> {
    type Item = Result<ItemType, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.count_rem.is_zero() {
            self.count_rem.dec();
            Some(Ok(self.item.clone()))
        } else {
            None
        }
    }
}

impl<ItemType: ItemTypeT> SIterator<ItemType> for RepeatItemIter<'_, ItemType> {
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

#[derive(Clone)]
pub struct RepeatStream<ItemType: ItemTypeT> {
    head: Head,
    stream: BoxedStream<ItemType>,
    count: Option<UNumber>
}

impl<ItemType: ItemTypeT> Stream<ItemType> for RepeatStream<ItemType> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<ItemType> + 'node> {
        Box::new(RepeatStreamIter {
            stream: &*self.stream,
            iter: self.stream.iter(),
            len: self.stream.length(),
            resets_rem: self.count.as_ref()
                .map(|count| count - 1u32)
        })
    }

    fn length(&self) -> Length {
        use Length::*;
        if self.count == Some(UNumber::zero()) { return Exact(UNumber::zero()); }
        match (self.stream.length(), &self.count) {
            (_, None) | (Infinite, _) => Infinite,
            (Exact(len), Some(count)) => Exact(len * count),
            (AtMost(len), Some(count)) => AtMost(len * count),
            (UnknownFinite, _) => UnknownFinite,
            (Unknown, _) => Unknown
        }
    }
}

impl<ItemType: ItemTypeT> Describe for RepeatStream<ItemType> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.stream), &self.count, prec, env)
    }
}

struct RepeatStreamIter<'node, ItemType: ItemTypeT> {
    stream: &'node dyn Stream<ItemType>,
    iter: Box<dyn SIterator<ItemType> + 'node>,
    len: Length,
    resets_rem: Option<UNumber>
}

impl<ItemType: ItemTypeT> Iterator for RepeatStreamIter<'_, ItemType> {
    type Item = Result<ItemType, StreamError>;

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

impl<ItemType: ItemTypeT> SIterator<ItemType> for RepeatStreamIter<'_, ItemType> {
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
        test_eval!("\"\".repeat(0)" => "\"\"");
        test_eval!("\"\".repeat(1)" => "\"\"");
        test_eval!("\"\".repeat(10)" => "\"\"");

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
        test_skip_n("1.repeat");
        test_skip_n("1.repeat(10^10)");
        test_skip_n("[].repeat");
        test_skip_n("[].repeat(10^10)");
        test_skip_n("[1,2].repeat");
        test_skip_n("[1,2].repeat(10^10)");
        test_skip_n("range(10^10).repeat(10^10)");
        test_skip_n("seq.repeat");
        test_skip_n("seq.repeat(0)");
        test_skip_n("seq.repeat(1)");
        test_skip_n("seq.repeat(2)");

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

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("repeat", eval_repeat);
}
