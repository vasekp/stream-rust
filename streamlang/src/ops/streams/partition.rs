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
                HetPartitionIter{iter, size_iter, node: self}.wrap()
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
        let mut vec = Vec::with_capacity(self.size);
        for _ in 0..self.size {
            let Some(next) = self.iter.next()? else { break; };
            vec.push(next);
        }
        if vec.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Item::from(vec)))
        }
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        let mul = &n * UNumber::from(self.size);
        match self.iter.advance(mul.clone())? {
            Some(rem) => Ok(Some(rem / self.size)),
            None => Ok(None)
        }
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain().map(|len| (len + self.size - 1) / self.size)
    }

    fn origin(&self) -> &Rc<Partition<I>> {
        &self.node
    }
}

struct HetPartitionIter<I: ItemType> {
    node: Rc<Partition<I>>,
    iter: Box<dyn SIterator<I>>,
    size_iter: Box<dyn SIterator>,
}

impl<I: ItemType> PreIterator for HetPartitionIter<I> {
    fn next(&mut self) -> SResult<Option<Item>> {
        let size: usize = iter_try!(self.size_iter.next())
            .to_num()?.try_cast()?;
        if size.is_zero() {
            return Ok(Some(Item::from(Vec::<I>::new())));
        }
        let mut vec = Vec::with_capacity(size);
        for _ in 0..size {
            let Some(next) = self.iter.next()? else { break; };
            vec.push(next);
        }
        if vec.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Item::from(vec)))
        }
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }

    fn origin(&self) -> &Rc<Partition<I>> {
        &self.node
    }
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
        test_eval!("\"abcdefgh\".partition(3)" => "[\"abc\", \"def\", \"gh\"]");
        test_advance("(1..(10^5)).partition(3)");
        test_advance("(1..(10^5)).partition(5)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["partition", "ptt", "chunks"], eval_partition, r#"
A stream of non-overlapping arrays from the input `stream` up to size `size`, or substrings of `string` of up to length `size`.
`size` may be given as a number or a stream of numbers.
= stream.?(size)
= string.?(size)
> ?seq.?(3) : 10 => [[1, 2, 3], [4, 5, 6], [7, ...], ...]
> (1..5).?(2) : 10 => [[1, 2], [3, 4], [5]] ; last group may be incomplete
> "Hello world".?(3) => ["Hel", "lo ", "wor", "ld"]
> ?seq.?([1, 2].?repeat) : 10 => [[1], [2, 3], [4], [5, 6], ...]
: groups
"#);
}
