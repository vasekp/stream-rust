use crate::base::*;

fn eval_skip(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let count = match &node.args[..] {
        [] => None,
        [Item::Number(count)] => Some(count.try_unsign()?),
        _ => return Err(StreamError::usage(&node.head))
    };
    match node.source_checked()? {
        Item::Stream(stm) => Ok(Item::new_stream(Skip{head: node.head.clone(), source: Rc::clone(stm), count })),
        Item::String(stm) => Ok(Item::new_string(Skip{head: node.head.clone(), source: Rc::clone(stm), count })),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct Skip<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    count: Option<UNumber>
}

impl<I: ItemType> Stream<I> for Skip<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        let mut iter = self.source.iter();
        let one = UNumber::one();
        match iter.advance(self.count.as_ref().unwrap_or(&one)) {
            Ok(None) => iter,
            Ok(Some(_)) => Box::new(std::iter::empty()),
            Err(err) => iter_error(err, &self),
        }
    }

    fn len(&self) -> Length {
        self.source.len()
            .map(|x| match &self.count {
                Some(count) => if count < x { x - count } else { UNumber::zero() },
                None => if x.is_zero() { x.to_owned() } else { x - 1u32 }
            })
    }
}

impl<I: ItemType> Describe for Skip<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.count)
            .finish(prec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip() {
        test_eval!("(1..3).skip(0)" => "[1, 2, 3]");
        test_eval!("(1..3).skip(1)" => "[2, 3]");
        test_eval!("(1..3).skip(2)" => "[3]");
        test_eval!("(1..3).skip(3)" => "[]");
        test_eval!("(1..3).skip(4)" => "[]");
        test_eval!("(1..3).skip(5)" => "[]");
        test_eval!("(1..3).skip" => "[2, 3]");
        test_eval!("\"abc\".skip" => "\"bc\"");
        test_eval!("\"abc\".skip(3)" => "\"\"");
        test_eval!("(1..3).skip(-1)" => err);
        test_eval!("seq.skip(10^10)" => "[10000000001, 10000000002, 10000000003, 10000000004, 10000000005, ...]");
        test_eval!("seq.skip(10^10).skip(10^10)" => "[20000000001, 20000000002, 20000000003, 20000000004, 20000000005, ...]");
        test_len!("(1..3).skip(0)" => 3);
        test_len!("(1..3).skip(1)" => 2);
        test_len!("(1..3).skip(2)" => 1);
        test_len!("(1..3).skip(3)" => 0);
        test_len!("(1..3).skip(4)" => 0);
        test_len!("(1..3).skip(5)" => 0);
        test_advance("seq.skip(100)");
        test_advance("(1..10^10).skip(10^9)");
        test_describe!("(1..3).skip(0)" => "(1..3).skip(0)");
        test_describe!("(1..3).skip(4)" => "(1..3).skip(4)");
        test_describe!("\"abc\".skip" => "\"abc\".skip");
        test_describe!("\"abc\".skip(4)" => "\"abc\".skip(4)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["skip", "dropfirst"], eval_skip, r#"
Skips first `count` items of `stream` ot first `count` characters of `string`.
If `count` is omitted, it defaults to one.
If `count` is longer than `stream`, returns an empty stream (string).
= stream.?
= stream.?(count)
= string.?
= string.?(count)
= (stream|string).?
> ?range(5).? => [2, 3, 4, 5]
> ?seq.?(10) => [11, 12, 13, 14, 15, ...]
> "Hello".?(2) => "llo"
> "Hello".?(5) => ""
: droplast
"#);
}
