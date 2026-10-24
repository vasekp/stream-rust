use crate::base::*;

fn eval_partition(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let size = match &node.args[..] {
        [Item::Number(size)] => Size::Fixed(size.try_cast_within(1usize..)?),
        [Item::Stream(stm)] => Size::Stream(Rc::clone(stm)),
        _ => return Err(StreamError::usage(&node.head)),
    };
    match node.source_checked()? {
        Item::Stream(stm) => Ok(Item::new_stream(Partition{
            head: node.head.clone(),
            source: Rc::clone(stm),
            size})),
        Item::String(stm) => Ok(Item::new_stream(Partition{
            head: node.head.clone(),
            source: Rc::clone(stm),
            size})),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct Partition<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    size: Size,
}

#[derive(Clone)]
enum Size {
    Fixed(usize),
    Stream(Rc<dyn Stream>),
}

impl<I: ItemType> Describe for Partition<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        let mut db = DescribeBuilder::new(&self.head, env);
        db.set_source(&self.source);
        match &self.size {
            Size::Fixed(size) => db.push_arg(&size),
            Size::Stream(stm) => db.push_arg(&stm),
        };
        db.finish(prec)
    }
}

impl<I: ItemType> Stream for Partition<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iter = self.source.iter();
        match &self.size {
            Size::Fixed(size) => HomPartitionIter{iter, size: *size, node: self}.wrap(),
            Size::Stream(stm) => {
                let size_iter = stm.iter();
                HetPartitionIter{
                    iter,
                    size_iter,
                    consumed: Some(UNumber::zero()),
                    node: self
                }.wrap()
            }
        }
    }

    fn len(&self) -> Length {
        match &self.size {
            Size::Fixed(size) =>
                self.source.len().map(|len| (len + size - 1) / size),
            Size::Stream(_) => Length::Unknown,
        }
    }
}

struct HomPartitionIter<I: ItemType> {
    node: Rc<Partition<I>>,
    iter: Box<dyn SIterator<I>>,
    size: usize,
}

impl<I: ItemType> PreIterator for HomPartitionIter<I> {
    fn next(&mut self) -> SResult<Option<Item>> {
        let mut vec = Vec::with_capacity(std::cmp::min(self.size, RESERVE_CAP));
        for _ in 0..self.size {
            check_stop!();
            let Some(next) = self.iter.next()? else { break; };
            vec.push(next);
        }
        if vec.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Item::from(vec)))
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mul = n * UNumber::from(self.size);
        match self.iter.advance(&mul)? {
            Some(rem) => Ok(Some(rem / self.size)),
            None => Ok(None)
        }
    }

    fn origin(&self) -> &Rc<Partition<I>> {
        &self.node
    }
}

struct HetPartitionIter<I: ItemType> {
    node: Rc<Partition<I>>,
    iter: Box<dyn SIterator<I>>,
    size_iter: Box<dyn SIterator>,
    consumed: Option<UNumber>,
}

impl<I: ItemType> PreIterator for HetPartitionIter<I> {
    fn next(&mut self) -> SResult<Option<Item>> {
        let Some(consumed) = self.consumed.as_mut() else {
            return Ok(None);
        };
        let size = iter_try!(self.size_iter.next());
        if let Item::Stream(stm) = &size && stm.is_empty()? {
            let expr = Expr::from(Item::from(&self.node.source))
                .chain(Link::new("skip",
                    vec![Item::new_number(&*consumed).into()]
                ));
            self.consumed = None;
            return Ok(Some(expr.eval_default()?));
        }
        let size: usize = size.to_num()?.try_cast()?;
        *consumed += size;
        if size.is_zero() {
            return Ok(Some(I::empty()));
        }
        let mut vec = Vec::with_capacity(std::cmp::min(size, RESERVE_CAP));
        for _ in 0..size {
            check_stop!();
            let Some(next) = self.iter.next()? else {
                self.consumed = None;
                break;
            };
            vec.push(next);
        }
        if vec.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Item::from(vec)))
        }
    }

    fn origin(&self) -> &Rc<Partition<I>> {
        &self.node
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["partition", "ptt", "chunks"], eval_partition, r#"
A stream of non-overlapping arrays from the input `stream` up to a given size, or substrings of `string` of up to such length.
The size may be given as a number or a stream of numbers. The special element of `[]` means "the rest of input".
= stream.?(number)
= stream.?(stream)
= string.?(number)
= string.?(stream)
> ?seq.?(3) : 10 => [[1, 2, 3], [4, 5, 6], [7, ...], ...]
> (1..5).?(2) : 10 => [[1, 2], [3, 4], [5]] ; last group may be incomplete
> (1..5).?([1, 1, []]) : 10 => [[1], [2], [3, 4, 5]]
> "Hello world".?(3) => ["Hel", "lo ", "wor", "ld"]
> "Hello world".?([3, []]) => ["Hel", "lo world"]
> ?seq.?([1, 2].?repeat) : 10 => [[1], [2, 3], [4], [5, 6], ...]
: groups
: unweave
"#);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_partition() {
        use super::*;
        test_eval!("(1..7).partition(3)" : 15 => "[[1, 2, 3], [4, 5, 6], [7]]");
        test_eval!("seq.partition(3)" : 10 => "[[1, 2, 3], [4, 5, 6], [7, ...], ...]");
        test_eval!("seq.partition(2)" : 10 => "[[1, 2], [3, 4], [5, 6], [...], ...]");
        test_eval!("seq.partition(1)" : 10 => "[[1], [2], [3], [4], [5], ...]");
        test_eval!("seq.partition(0)" => err);
        test_eval!("seq.partition(seq)" : 10 => "[[1], [2, 3], [4, 5, 6], [...], ...]");
        test_eval!("seq.partition([1, 0].repeat)" => "[[1], [], [2], ...]");
        test_eval!("\"hello\".partition([2, 1, 0])" => "[\"he\", \"l\", \"\"]");
        test_len!("(1..10).partition(3)" => 4);
        test_len!("(1..10).partition(5)" => 2);
        test_len!("(1..10).partition(9)" => 2);
        test_len!("(1..10).partition(10)" => 1);
        test_eval!("(1..5).partition(4)[1]" => "[1, 2, 3, 4]");
        test_eval!("(1..5).partition(4)[2]" => "[5]");
        test_eval!("(1..5).partition(4)[3]" => err);
        test_eval!("(1..5).partition([2,[]])" : 10 => "[[1, 2], [3, 4, 5]]");
        test_eval!("(1..5).partition([2,2,[]])" : 10 => "[[1, 2], [3, 4], [5]]");
        test_eval!("(1..5).partition([2,2,2,[]])" : 10 => "[[1, 2], [3, 4], [5]]");
        test_eval!("(1..5).partition([2,2,1,[]])" : 10 => "[[1, 2], [3, 4], [5], []]");
        test_eval!("(1..5).partition([2,2,[],[]])" : 10 => "[[1, 2], [3, 4], [5]]");
        test_eval!("(1..5).partition([2,2,[],0])" : 10 => "[[1, 2], [3, 4], [5]]");
        test_eval!("seq.partition([2,[]])" : 10 => "[[1, 2], [3, 4, 5, 6, 7, 8, ...]]");
        test_eval!("seq.partition([2,[],[]])" : 10 => "[[1, 2], [3, 4, 5, 6, 7, 8, ...]]");
        test_eval!("seq.partition([2,[],true])" : 10 => "[[1, 2], [3, 4, 5, 6, 7, 8, ...]]");
        test_eval!("seq.partition([2,[],[]])[3]" => err);
        test_eval!("seq.partition([2,-1])" => "[[1, 2], <!>");
        test_eval!("seq.partition([2,[1]])" => "[[1, 2], <!>");
        test_eval!("\"abcdefgh\".partition(3)" => "[\"abc\", \"def\", \"gh\"]");
        test_advance("(1..(10^5)).partition(3)");
        test_advance("(1..(10^5)).partition(5)");
        test_len!("(1..5).partition([2,2,2,[]])" => 3);
        test_len!("(1..5).partition([2,2,1,[]])" => 4);
        test_len!("(1..5).partition([2,2,[],[]])" => 3);
    }
}
