use crate::base::*;
use crate::utils::TriState;

#[derive(Clone)]
struct Join {
    node: ENode,
}

impl Join {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);

        let string = try_with!(node, node.args.iter()
            .map(|item| match item {
                Item::String(_) => TriState::True,
                Item::Char(_) => TriState::Either,
                _ => TriState::False
            })
            .try_fold(TriState::Either, TriState::join)
            .map_err(|()| BaseError::from("mixed strings and non-strings"))?)
            .is_true();

        if string {
            Ok(Item::new_string_stream(Join{node}))
        } else {
            Ok(Item::new_stream(Join{node}))
        }
    }
}

impl Describe for Join {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        self.node.describe_inner(prec, env)
    }
}

impl Stream for Join {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let first = match &self.node.args[0] {
            Item::Stream(stm) | Item::String(stm) => stm.iter(),
            item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
        };
        Box::new(JoinIter{node: &self.node, index: 0, cur: first})
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) | Item::String(stm) => stm.length(),
                _ => Length::Exact(UNumber::one())
            })
            .reduce(|acc, e| acc + e).unwrap() // args checked to be nonempty in eval()
    }

    fn is_empty(&self) -> bool {
        self.node.args.iter()
            .all(|item| match item {
                Item::Stream(stm) | Item::String(stm) => stm.is_empty(),
                _ => false
            })
    }
}

struct JoinIter<'node> {
    node: &'node ENode,
    index: usize,
    cur: Box<dyn SIterator + 'node>
}

impl Iterator for JoinIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.cur.next();
            if next.is_some() {
                return next;
            } else {
                self.index += 1;
                self.cur = match self.node.args.get(self.index)? {
                    Item::Stream(stm) | Item::String(stm) => stm.iter(),
                    item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
                };
            }
        }
    }
}

impl SIterator for JoinIter<'_> {
    fn skip_n(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            let Some(m) = self.cur.skip_n(n)?
                else { return Ok(None); };
            n = m;
            self.index += 1;
            let Some(next) = self.node.args.get(self.index)
                else { return Ok(Some(n)); };
            self.cur = match next {
                Item::Stream(stm) | Item::String(stm) => stm.iter(),
                item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
            };
        }
    }

    fn len_remain(&self) -> Length {
        let mut len = self.cur.len_remain();
        if matches!(len, Length::Infinite | Length::Unknown | Length::UnknownFinite) {
            return len;
        }
        for i in (self.index + 1)..self.node.args.len() {
            match &self.node.args[i] {
                Item::Stream(stm) | Item::String(stm) => len = len + stm.length(),
                _ => len += UNumber::one()
            }
        }
        len
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
    keywords.insert("~", Join::eval);
    keywords.insert("join", Join::eval);
}
