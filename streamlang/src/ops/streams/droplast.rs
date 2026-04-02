use crate::base::*;

use std::collections::VecDeque;

fn eval_droplast(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let count = match &node.args[..] {
        [] => 1u32.into(),
        [Item::Number(count)] => count.try_unsign()?,
        _ => return Err(StreamError::usage(&node.head))
    };
    match node.source_checked()? {
        Item::Stream(stm) => droplast_impl(&node.head, stm, count),
        Item::String(stm) => droplast_impl(&node.head, stm, count),
        _ => Err(StreamError::usage(&node.head))
    }
}

fn droplast_impl<I: ItemType>(head: &Head, stm: &Rc<dyn Stream<I>>, count: UNumber) -> SResult<Item> {
    if count.is_zero() {
        return Ok(stm.into());
    }
    match stm.len() {
        Length::Exact(len) if len < count => Ok(I::empty()),
        Length::Exact(len) => {
            Ok(Item::from(Rc::new(KnownLen {
                head: head.clone(),
                source: Rc::clone(stm),
                ceiling: len - &count,
                count
            }) as Rc<dyn Stream<I>>))
        },
        Length::Infinite => Err("stream is infinite".into()),
        _ => {
            Ok(Item::from(Rc::new(DropLast {
                head: head.clone(),
                source: Rc::clone(stm),
                count: count.try_cast()?,
            }) as Rc<dyn Stream<I>>))
        }
    }
}

struct KnownLen<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    count: UNumber,
    ceiling: UNumber,
}

impl<I: ItemType> Stream<I> for KnownLen<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        KnownLenIter{
            source: self.source.iter(),
            count_rem: self.ceiling.clone(),
            node: self
        }.wrap()
    }

    fn len(&self) -> Length {
        Length::Exact(self.ceiling.clone())
    }
}

impl<I: ItemType> Describe for KnownLen<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.count)
            .finish(prec)
    }
}

struct KnownLenIter<I: ItemType> {
    node: Rc<KnownLen<I>>,
    source: Box<dyn SIterator<I>>,
    count_rem: UNumber
}

impl<I: ItemType> PreIterator<I> for KnownLenIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        if !self.count_rem.is_zero() {
            self.count_rem -= 1;
            self.source.next()
        } else {
            Ok(None)
        }
    }

    fn len_remain(&self) -> Length {
        Length::Exact(self.count_rem.to_owned())
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        if n > &self.count_rem {
            Ok(Some(n - std::mem::take(&mut self.count_rem)))
        } else {
            self.count_rem -= n;
            debug_assert!(matches!(self.source.advance(n), Ok(None)));
            Ok(None)
        }
    }

    fn origin(&self) -> &Rc<KnownLen<I>> {
        &self.node
    }
}


struct DropLast<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    count: usize,
}

impl<I: ItemType> Stream<I> for DropLast<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        DropLastIter::from(self)
    }

    fn len(&self) -> Length {
        self.source.len().map(|len|
            if len < &UNumber::from(self.count) {
                UNumber::zero()
            } else {
                len - self.count
            }
        )
    }
}

impl<I: ItemType> Describe for DropLast<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.count)
            .finish(prec)
    }
}

struct DropLastIter<I: ItemType> {
    source: Box<dyn SIterator<I>>,
    queue: VecDeque<I>,
    node: Rc<DropLast<I>>,
}

impl<I: ItemType> DropLastIter<I> {
    fn from(node: Rc<DropLast<I>>) -> Box<dyn SIterator<I>> {
        let mut iter = node.source.iter();
        let mut queue = VecDeque::with_capacity(node.count);
        for _ in 0..node.count {
            check_stop!(iter: node);
            match iter.next() {
                Ok(Some(item)) => queue.push_back(item),
                Ok(None) => return Box::new(std::iter::empty()),
                Err(err) => return iter_error(err, &node),
            }
        }
        Self{source: iter, queue, node}.wrap()
    }
}

impl<I: ItemType> PreIterator<I> for DropLastIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        let next = self.queue.pop_front()
            .expect("queue should be full by precondition");
        self.queue.push_back(iter_try!(self.source.next()));
        Ok(Some(next))
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let n = match usize::try_from(n) {
            Ok(n0) if n0 < self.node.count => {
                self.queue.drain(0..n0);
                0u32.into()
            },
            _ => {
                self.queue.clear();
                n - self.node.count
            }
        };
        for _ in self.queue.len() .. self.node.count {
            match self.source.next()? {
                Some(item) => self.queue.push_back(item),
                None => return Ok(Some(n + (self.node.count - self.queue.len()))),
            }
        }
        Ok(None)
    }

    fn origin(&self) -> &Rc<DropLast<I>> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_droplast() {
        test_eval!("(1..3).droplast" => "[1, 2]");
        test_eval!("(1..3).droplast(0)" => "[1, 2, 3]");
        test_eval!("(1..3).droplast(1)" => "[1, 2]");
        test_eval!("(1..3).droplast(2)" => "[1]");
        test_eval!("(1..3).droplast(3)" => "[]");
        test_eval!("(1..3).droplast(4)" => "[]");
        test_eval!("\"abc\".droplast" => "\"ab\"");
        test_eval!("\"abc\".droplast(0)" => "\"abc\"");
        test_eval!("\"abc\".droplast(1)" => "\"ab\"");
        test_eval!("\"abc\".droplast(4)" => "\"\"");
        test_eval!("[].$lenUF.droplast" => "[]");
        test_eval!("(1..5).$lenUF.droplast" => "[1, 2, 3, 4]");
        test_eval!("[].$lenUF.droplast(3)" => "[]");
        test_eval!("[1,2].$lenUF.droplast(3)" => "[]");
        test_eval!("(1..5).$lenUF.droplast(3)" => "[1, 2]");
        test_eval!("seq.droplast(10^10)" => err);
        test_eval!("range(10^9).droplast(10^10).len" => "0");
        test_eval!("range(10^11).droplast(10^10).len" => "90000000000");
        test_eval!("range(10^10).droplast(10^9).len" => "9000000000");
        test_eval!("range(10^10).droplast(10^9).last" => "9000000000");
        test_len!("(1..3).droplast(0)" => 3);
        test_len!("(1..3).droplast(1)" => 2);
        test_len!("(1..3).droplast(2)" => 1);
        test_len!("(1..3).droplast(3)" => 0);
        test_len!("(1..3).droplast(4)" => 0);
        test_advance("range(10^9).droplast(10^10)");
        test_advance("range(10^11).droplast(10^10)");
        test_describe!("(1..3).droplast(0)" => "1..3");
        test_describe!("(1..3).droplast(4)" => "[]");
        test_describe!("(1..3).droplast(2)" => "(1..3).droplast(2)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("droplast", eval_droplast, r#"
Skips last `count` items of `stream` ot last `count` characters of `string`.
If `count` is omitted, it defaults to one.
If `count` is longer than `stream`, returns an empty stream (string).
= stream.?
= stream.?(count)
= string.?
= string.?(count)
> ?range(5).? => [1, 2, 3, 4]
> ?seq.?(10) => !stream is infinite
> "Hello".?(2) => "Hel"
> "Hello".?(5) => ""
: skip
"#);
}
