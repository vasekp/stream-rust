use crate::base::*;
use crate::utils::TriState;

fn eval_join(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    try_with!(node, node.check_args_nonempty()?);

    let is_string = try_with!(node, node.args.iter()
        .map(|item| match item {
            Item::String(_) => TriState::True,
            Item::Char(_) => TriState::Either,
            _ => TriState::False
        })
        .try_fold(TriState::Either, TriState::join)
        .map_err(|()| BaseError::from("mixed strings and non-strings"))?)
        .can_be_true();

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
        Node::describe_helper(&self.head, None::<&Item>, &self.elems, prec, env)
    }
}

impl<I: ItemType> Stream<I> for Join<I> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<I> + 'node> {
        Box::new(JoinIter{elems: &self.elems, index: 0, inner: None})
    }

    fn length(&self) -> Length {
        self.elems.iter()
            .map(|item| match item {
                Joinable::Single(_) => Length::Exact(UNumber::one()),
                Joinable::Stream(stm) => stm.length()
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
    fn skip_n(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if let Some(inner) = &mut self.inner {
                let Some(m) = inner.skip_n(n)? else { return Ok(None); };
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
                Joinable::Stream(stm) => stm.length()
            })
            .fold(len, |acc, e| acc + e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_join() {
        use crate::parser::parse;

        test_eval!("[10]~seq" => "[10, 1, 2, 3, 4, ...]");
        test_eval!("range(2)~seq" => "[1, 2, 1, 2, 3, ...]");
        test_eval!("range(10^10).{#~#~#}.len" => "30000000000");
        test_eval!("([5]~seq).len" => err);
        test_eval!("(range(10^10)~seq)[10^11]" => "90000000000");

        test_eval!("(\"ab\"~\"cd\").len" => "4");

        test_eval!("[1]~[2]" => "[1, 2]");
        test_eval!("[1]~[[2]]" => "[1, [2]]");
        test_eval!("[1]~2~'c'" => "[1, 2, 'c']");
        test_eval!("1~2~3" => "[1, 2, 3]");
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
        test_eval!("\"a\"~1" => err);
        test_eval!("\"a\"~[1]" => err);
        test_eval!("\"a\"~['b']" => err);
        test_eval!("[1]~\"a\"" => err);

        test_len!("[1,2,3]~4~[5]~[[5,6]]" => 6);
        test_len!("1~2~3" => 3);
        test_len!("0~1..2~3" => 4);
        test_len!("0~1..0~3" => 2);
        test_len!("\"ab\"~\"cd\"" => 4);
        test_len!("\"ab\"~'ch'" => 3);
        test_len!("\"ab\"~'ch'" => 3);
        test_len!("\"\"~\"\"" => 0);
        test_skip_n("range(10^10)~range(10^9)");
        test_skip_n("range(10^10)~range(-10^10)~range(10^9)");
        test_skip_n("('a'..'z').repeat(10^10)~['A'].repeat(10^10)");

        test_describe!("1~2" => "1~2");
        test_describe!("[1]~[2]" => "[1]~[2]");
        test_describe!("join([1],[2])" => "join([1], [2])");
        test_describe!("\"ab\"~'c'" => "\"ab\"~'c'");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("join", eval_join);
    keywords.insert("~", eval_join);
}
