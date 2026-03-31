use crate::base::*;

fn eval_join(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    node.check_args_nonempty()?;

    let is_string = node.args.iter()
        .all(|item| matches!(item, Item::Char(_) | Item::String(_)));

    if is_string {
        let elems = node.args.into_iter()
            .map(|item| match item {
                Item::Char(ch) => Joinable::Single(ch),
                Item::String(stm) => Joinable::Stream(stm),
                _ => unreachable!()
            })
            .collect::<Vec<_>>();
        Ok(Item::new_string(Join{head: node.head, elems}))
    } else {
        let elems = node.args.into_iter()
            .map(|item| match item {
                Item::Stream(stm) => Joinable::Stream(stm),
                _ => Joinable::Single(item)
            })
            .collect::<Vec<_>>();
        Ok(Item::new_stream(Join{head: node.head, elems}))
    }
}

enum Joinable<I: ItemType> {
    Single(I),
    Stream(Rc<dyn Stream<I>>)
}

impl<I: ItemType> Describe for Joinable<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        match self {
            Joinable::Single(item) => item.describe_inner(prec, env),
            Joinable::Stream(stm) => stm.describe_inner(prec, env),
        }
    }
}

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
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        let mut iters = Vec::with_capacity(self.elems.len());
        for elem in &self.elems {
            match elem {
                Joinable::Stream(stm) => iters.push(stm.iter()),
                Joinable::Single(item) => iters.push(Box::new(std::iter::once(Ok(item.clone())))),
            }
        }
        JoinIter{iters, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.elems.iter()
            .map(|item| match item {
                Joinable::Single(_) => Length::Exact(UNumber::one()),
                Joinable::Stream(stm) => stm.len()
            })
            .reduce(|acc, e| acc + e).unwrap() // args checked to be nonempty in eval()
    }
}

struct JoinIter<I: ItemType> {
    node: Rc<Join<I>>,
    iters: Vec<Box<dyn SIterator<I>>>,
}

impl<I: ItemType> PreIterator<I> for JoinIter<I> {
    fn next(&mut self) -> SResult<Option<I>> {
        loop {
            let Some(iter) = self.iters.first_mut() else {
                return Ok(None);
            };
            if let Some(next) = iter.next()? {
                return Ok(Some(next));
            } else {
                self.iters.remove(0);
            }
        }
    }

    fn advance(&mut self, mut n: UNumber) -> SResult<Option<UNumber>> {
        loop {
            check_stop!();
            if n.is_zero() { return Ok(None); }
            let Some(iter) = self.iters.first_mut() else {
                return Ok(Some(n));
            };
            if let Some(m) = iter.advance(n)? {
                self.iters.remove(0);
                n = m;
            } else {
                return Ok(None);
            };
        }
    }

    fn len_remain(&self) -> Length {
        self.iters.iter()
            .map(|iter| iter.len_remain())
            .fold(Length::empty(), |acc, e| acc + e)
    }

    fn origin(&self) -> &Rc<Join<I>> {
        &self.node
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
        test_advance("(1..5)~6~(8..10)");
        test_advance("(1..4)~6~7~(8..10)");

        test_describe!("1~2" => "1~2");
        test_describe!("[1]~[2]" => "[1]~[2]");
        test_describe!("join([1],[2])" => "join([1], [2])");
        test_describe!("\"ab\"~'c'" => "\"ab\"~'c'");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("join", eval_join, r#"
The concatenation of all `input`s. If all of them are strings or single characters, the result is a string, otherwise, a stream.
The shorthand for `?(a, b, c, ...)` is `a~b~c~...`.
= ?(input1, input2, ...)
> ["ab", "cd", "ef"].?windows(2, ?) => ["abcd", "cdef"]
> ?("Hello", ' ', "world") => "Hello world"
: ~
: cat
: flatten
"#);
    symbols.insert("~", eval_join, r#"
The concatenation of all `input`s. If all of them are strings or single characters, the result is a string, otherwise, a stream.
= input1~input2~...
> ['a', 'b', 'c']~?seq => ['a', 'b', 'c', 1, 2, ...]
> [1, 2]~3~"abc" => [1, 2, 3, "abc"]
> "Hello"~' '~"world" => "Hello world"
: join
: cat
: flatten
"#);
}
