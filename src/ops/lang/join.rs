use crate::base::*;

#[derive(Clone)]
struct Join {
    node: ENode,
    string: bool
}

impl Join {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        try_with!(node, node.check_args_nonempty()?);

        fn is_string(item: &Item) -> TriState {
            match item {
                Item::Stream(stm) => stm.is_string(),
                Item::Char(_) => TriState::Either,
                _ => TriState::False
            }
        }

        let string = try_with!(node, node.args.iter()
            .map(is_string)
            .try_fold(TriState::Either, TriState::join)
            .map_err(|()| BaseError::from("mixed strings and non-strings"))?)
            .is_true();
        Ok(Item::new_stream(Join{node, string}))
    }
}

impl Describe for Join {
    fn describe_prec(&self, prec: u32) -> String {
        self.node.describe_prec(prec)
    }
}

impl Stream for Join {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let first = match &self.node.args[0] {
            Item::Stream(stm) => stm.iter(),
            item => Box::new(std::iter::once(Ok::<Item, StreamError>(item.clone())))
        };
        Box::new(JoinIter{node: &self.node, index: 0, cur: first})
    }

    fn is_string(&self) -> TriState {
        self.string.into()
    }

    fn length(&self) -> Length {
        self.node.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.length(),
                _ => Length::Exact(UNumber::one())
            })
            .reduce(|acc, e| acc + e).unwrap() // args checked to be nonempty in eval()
    }

    fn is_empty(&self) -> bool {
        self.node.args.iter()
            .all(|item| match item {
                Item::Stream(stm) => stm.is_empty(),
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
                    Item::Stream(stm) => stm.iter(),
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
                Item::Stream(stm) => stm.iter(),
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
                Item::Stream(stm) => len = len + stm.length(),
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

        assert_eq!(parse("[10]~seq").unwrap().eval().unwrap().to_string(), "[10, 1, 2, 3, 4, ...]");
        assert_eq!(parse("range(2)~seq").unwrap().eval().unwrap().to_string(), "[1, 2, 1, 2, 3, ...]");
        assert_eq!(parse("range(10^10).{#~#~#}.len").unwrap().eval().unwrap().to_string(), "30000000000");
        assert!(parse("([5]~seq).len").unwrap().eval().is_err());
        assert_eq!(parse("(range(10^10)~seq)[10^11]").unwrap().eval().unwrap().to_string(), "90000000000");

        assert_eq!(parse("(\"ab\"~\"cd\").len").unwrap().eval().unwrap().to_string(), "4");

        assert_eq!(parse("[1]~[2]").unwrap().eval().unwrap().to_string(), "[1, 2]");
        assert_eq!(parse("[1]~[[2]]").unwrap().eval().unwrap().to_string(), "[1, [2]]");
        assert_eq!(parse("[1]~2~'c'").unwrap().eval().unwrap().to_string(), "[1, 2, 'c']");
        assert_eq!(parse("1~2~3").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("10~seq").unwrap().eval().unwrap().to_string(), "[10, 1, 2, 3, 4, ...]");
        assert_eq!(parse("(0~1..0~2)").unwrap().eval().unwrap().to_string(), "[0, 2]");
        assert_eq!(parse("(0~1..3~4)[3]").unwrap().eval().unwrap().to_string(), "2");
        assert_eq!(parse("(0~1..3~4)[4]").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("\"ab\"~\"cd\"").unwrap().eval().unwrap().to_string(), "\"abcd\"");
        assert_eq!(parse("\"ab\"~'c'").unwrap().eval().unwrap().to_string(), "\"abc\"");
        assert_eq!(parse("'a'~\"b\"~'c'").unwrap().eval().unwrap().to_string(), "\"abc\"");
        assert!(parse("\"a\"~1").unwrap().eval().is_err());
        assert!(parse("\"a\"~[1]").unwrap().eval().is_err());
        assert!(parse("\"a\"~['b']").unwrap().eval().is_err());
        assert!(parse("[1]~\"a\"").unwrap().eval().is_err());

        test_len_exact(&parse("[1,2,3]~4~[5]~[[5,6]]").unwrap().eval().unwrap(), 6);
        test_len_exact(&parse("1~2~3").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("0~1..2~3").unwrap().eval().unwrap(), 4);
        test_len_exact(&parse("0~1..0~3").unwrap().eval().unwrap(), 2);
        test_len_exact(&parse("\"ab\"~\"cd\"").unwrap().eval().unwrap(), 4);
        test_len_exact(&parse("\"ab\"~'ch'").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("\"ab\"~'ch'").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("\"\"~\"\"").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse("range(10^10)~range(10^9)").unwrap().eval().unwrap());
        test_skip_n(&parse("range(10^10)~range(-10^10)~range(10^9)").unwrap().eval().unwrap());
        test_skip_n(&parse("('a'..'z').repeat(10^10)~['A'].repeat(10^10)").unwrap().eval().unwrap());

        assert_eq!(parse("1~2").unwrap().eval().unwrap().describe(), "1~2");
        assert_eq!(parse("[1]~[2]").unwrap().eval().unwrap().describe(), "[1]~[2]");
        assert_eq!(parse("\"ab\"~'c'").unwrap().eval().unwrap().describe(), "\"ab\"~'c'");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("~", Join::eval);
}
