use crate::base::*;

fn eval_join(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    try_with!(node, node.check_args_nonempty()?);

    let is_string = node.args.iter()
        .all(|item| matches!(item, Item::Char(_) | Item::String(_)));

    if is_string {
        let elems = node.args.into_iter()
            .map(|item| match item {
                Item::Char(ch) => Joinable::Single(ch),
                Item::String(stm) => Joinable::Stream(stm.into()),
                _ => unreachable!()
            })
            .collect::<Vec<_>>();
        Ok(Item::new_string(Join{head: node.head, elems}))
    } else {
        let elems = node.args.into_iter()
            .map(|item| match item {
                Item::Stream(stm) => Joinable::Stream(stm.into()),
                _ => Joinable::Single(item)
            })
            .collect::<Vec<_>>();
        Ok(Item::new_stream(Join{head: node.head, elems}))
    }
}

#[derive(Clone)]
enum Joinable<I: ItemType> {
    Single(I),
    Stream(BoxedStream<I>)
}

impl<I: ItemType> Describe for Joinable<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        match self {
            Joinable::Single(item) => item.describe_inner(prec, env),
            Joinable::Stream(stm) => stm.describe_inner(prec, env),
        }
    }
}

#[derive(Clone)]
struct Join<I: ItemType> {
    head: Head,
    elems: Vec<Joinable<I>>
}

impl<I: ItemType> Describe for Join<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .push_args(&self.elems)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for Join<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(JoinIter{elems: &self.elems, index: 0, inner: None})
    }

    fn len(&self) -> Length {
        self.elems.iter()
            .map(|item| match item {
                Joinable::Single(_) => Length::Exact(UNumber::one()),
                Joinable::Stream(stm) => stm.len()
            })
            .reduce(|acc, e| acc + e).unwrap() // args checked to be nonempty in eval()
    }

    fn is_empty(&self) -> bool {
        self.elems.iter()
            .all(|item| match item {
                Joinable::Stream(stm) => stm.is_empty(),
                _ => false
            })
    }
}

struct JoinIter<'node, I: ItemType> {
    elems: &'node Vec<Joinable<I>>,
    index: usize,
    inner: Option<Box<dyn SIterator<I> + 'node>>
}

impl<I: ItemType> Iterator for JoinIter<'_, I> {
    type Item = Result<I, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            if let Some(inner) = &mut self.inner {
                if let Some(res) = inner.next() { return Some(res); }
                else { self.inner = None; }
            }
            self.index += 1;
            match self.elems.get(self.index - 1)? {
                Joinable::Single(item) => return Some(Ok(item.clone())),
                Joinable::Stream(stm) => self.inner = Some(stm.iter())
            }
        }
    }
}

impl<I: ItemType> SIterator<I> for JoinIter<'_, I> {
    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if let Some(inner) = &mut self.inner {
                let Some(m) = inner.advance(n)? else { return Ok(None); };
                n = m;
            }
            self.index += 1;
            let Some(next) = self.elems.get(self.index - 1) else { return Ok(Some(n)); };
            match next {
                Joinable::Single(_) => n.dec(),
                Joinable::Stream(stm) => self.inner = Some(stm.iter())
            }
        }
    }

    fn len_remain(&self) -> Length {
        let len = if let Some(inner) = &self.inner { inner.len_remain() } else { Length::Exact(UNumber::zero()) };
        self.elems[self.index..].iter()
            .map(|item| match item {
                Joinable::Single(_) => Length::Exact(UNumber::one()),
                Joinable::Stream(stm) => stm.len()
            })
            .fold(len, |acc, e| acc + e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_join() {
        test_eval!("[10]~seq" => "[10, 1, 2, 3, 4, ...]");
        test_eval!("range(2)~seq" => "[1, 2, 1, 2, 3, ...]");
        test_eval!("range(10^10).{#~#~#}.len" => "30000000000");
        test_eval!("([5]~seq).len" => err);
        test_eval!("(range(10^10)~seq)[10^11]" => "90000000000");

        test_eval!("(\"ab\"~\"cd\").len" => "4");

        test_eval!("[1]~[2]" => "[1, 2]");
        test_eval!("[1]~[[2]]" => "[1, [2]]");
        test_eval!("[1]~[]" => "[1]");
        test_eval!("[1]~2~'c'" => "[1, 2, 'c']");
        test_eval!("1~2~3" => "[1, 2, 3]");
        test_eval!("1~[]" => "[1]");
        test_eval!("10~seq" => "[10, 1, 2, 3, 4, ...]");
        test_eval!("(0~1..0~2)" => "[0, 2]");
        test_eval!("(0~1..3~4)[3]" => "2");
        test_eval!("(0~1..3~4)[4]" => "3");
        test_eval!("\"ab\"~\"cd\"" => "\"abcd\"");
        test_eval!("\"ab\"~'c'" => "\"abc\"");
        test_eval!("'a'~'b'" => "\"ab\"");
        test_eval!("'a'~\"b\"~'c'" => "\"abc\"");
        test_eval!("join([1],[2])" => "[1, 2]");
        test_eval!("[1].join([1],[2])" => err);
        test_eval!("\"a\"~1" => "[\"a\", 1]");
        test_eval!("\"a\"~[1]" => "[\"a\", 1]");
        test_eval!("\"a\"~['b']" => "[\"a\", 'b']");
        test_eval!("[1]~\"a\"" => "[1, \"a\"]");
        test_eval!("\"\"~[]" => "[\"\"]");

        test_len!("[1,2,3]~4~[5]~[[5,6]]" => 6);
        test_len!("1~2~3" => 3);
        test_len!("0~1..2~3" => 4);
        test_len!("0~1..0~3" => 2);
        test_len!("\"ab\"~\"cd\"" => 4);
        test_len!("\"ab\"~'ch'" => 3);
        test_len!("\"ab\"~'ch'" => 3);
        test_len!("\"ab\"~1" => 2);
        test_len!("\"\"~\"\"" => 0);
        test_len!("[]~[]" => 0);
        test_len!("\"\"~[]" => 1);
        test_advance("range(10^10)~range(10^9)");
        test_advance("range(10^10)~range(-10^10)~range(10^9)");
        test_advance("('a'..'z').repeat(10^10)~['A'].repeat(10^10)");

        test_describe!("1~2" => "1~2");
        test_describe!("[1]~[2]" => "[1]~[2]");
        test_describe!("join([1],[2])" => "join([1], [2])");
        test_describe!("\"ab\"~'c'" => "\"ab\"~'c'");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_with_docs(["join", "~"], eval_join, r#"
The concatenation of all `input`s. If all of them are strings or single characters, the result is a string, otherwise, a stream.
The shorthand for `?(a, b, c, ...)` is `a~b~c~...`.
= ?(input, input, ...)
= input~input~...
> ['a', 'b', 'c']~?seq => ['a', 'b', 'c', 1, 2, ...]
> [1, 2]~3~"abc" => [1, 2, 3, "abc"]
> "Hello"~' '~"world" => "Hello world"
: cat
"#);
}
