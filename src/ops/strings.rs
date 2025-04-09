use crate::base::*;

#[derive(Clone)]
struct Shift {
    head: Head,
    source: BoxedStream,
    args: Vec<Item>,
    env: Rc<Env>
}

impl Shift {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        try_with!(node, node.check_args_nonempty()?);
        let source = match node.source {
            Some(Item::Stream(s)) if s.is_string().can_be_true() => s.into(),
            Some(ref item) => return Err(StreamError::new(format!("expected string, found {:?}", item), node)),
            None => return Err(StreamError::new("source required", node))
        };
        Ok(Item::new_stream(Shift{head: node.head, source, args: node.args, env: Rc::clone(env)}))
    }

    fn helper(base: &Char, items: &[Item], env: &Rc<Env>) -> Result<Item, BaseError> {
        let abc = env.alphabet();
        let (index, case) = abc.ord_case(base)?;
        let ans = items.iter().try_fold(index.into(),
            |a, e| {
                match e {
                    Item::Number(ref num) => Ok(a + num),
                    Item::Char(ref ch) => Ok(a + abc.ord_case(ch)?.0),
                    _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                }
            })?;
        Ok(Item::new_char(abc.chr_case(&ans, case)))
    }
}

impl Describe for Shift {
    fn describe(&self) -> String {
        self.env.wrap_describe(Node::describe_helper(&self.head, Some(&self.source), &self.args))
    }
}

impl Stream for Shift {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let base = self.source.string_iter();
        let args_iter = self.args.iter()
            .map(|item| match item {
                Item::Stream(stm) => stm.iter(),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(ShiftIter{base, source: &*self.source, args: &self.args, args_iter, env: &self.env})
    }

    fn is_string(&self) -> TriState {
        TriState::True
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

struct ShiftIter<'node> {
    base: StringIterator<'node>,
    source: &'node (dyn Stream + 'static),
    args: &'node Vec<Item>,
    args_iter: Vec<Box<dyn SIterator + 'node>>,
    env: &'node Rc<Env>
}

impl ShiftIter<'_> {
    fn node(&self) -> ENode {
        ENode {
            head: "shift".into(),
            source: Some(Item::Stream(self.source.clone_box())),
            args: self.args.clone()
        }
    }
}


impl Iterator for ShiftIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        fn aux_node(base: Char, mut inputs: Vec<Item>) -> Node {
            inputs.insert(0, Item::Char(base));
            Node {
                head: Head::Oper("+".into()),
                source: None,
                args: inputs.into_iter().map(Expr::from).collect()
            }
        }

        let ch = match self.base.next() {
            None => return None,
            Some(Ok(ch)) => ch,
            Some(Err(err)) => return Some(Err(err))
        };
        if !self.env.alphabet().contains(&ch) {
            return Some(Ok(Item::Char(ch)));
        }

        let rest = self.args_iter.iter_mut()
            .map(Iterator::next)
            .collect::<Option<Result<Vec<_>, _>>>();
        match rest {
            None => Some(Err(StreamError::new("some operand ended earlier than the source", self.node()))),
            Some(Ok(inputs)) => {
                match Shift::helper(&ch, &inputs, self.env) {
                    Ok(item) => Some(Ok(item)),
                    Err(err) => Some(Err(StreamError::new(err, aux_node(ch, inputs))))
                }
            },
            Some(Err(err)) => Some(Err(err))
        }
    }
}

impl SIterator for ShiftIter<'_> {
    fn skip_n(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let args_iter = self.args_iter.iter_mut();
        let mut n_chars = UNumber::zero();
        while !n.is_zero() {
            match self.base.next() {
                Some(Ok(ch)) => {
                    if self.env.alphabet().contains(&ch) {
                        n_chars.inc();
                    }
                },
                Some(Err(err)) => return Err(err),
                None => return Ok(Some(n))
            }
            n.dec();
        }
        for iter in args_iter {
            if iter.skip_n(n_chars.clone())?.is_some() {
                return Err(StreamError::new("another operand ended earlier than the first", self.node()));
            }
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        self.base.len_remain()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shift() {
        use crate::parser::parse;
        assert_eq!(parse("\"AbC\".shift(3,[0,10,20])").unwrap().eval().unwrap().to_string(), "\"DoZ\"");
        assert_eq!(parse("\"Test\".shift(13,13)").unwrap().eval().unwrap().to_string(), "\"Test\"");
        assert_eq!(parse(r#""ahoj".shift("bebe")"#).unwrap().eval().unwrap().to_string(), "\"cmqo\"");
        assert_eq!(parse(r#""Hello world!".shift(seq)"#).unwrap().eval().unwrap().to_string(), r#""Igopt cvzun!""#);
        assert_eq!(parse(r#""Hello world!".shift("ab")"#).unwrap().eval().unwrap().to_string(), r#""Ig<!>"#);
        assert_eq!(parse(r#"("Hello world!".shift("ab"))[2]"#).unwrap().eval().unwrap().to_string(), "'g'");
        assert!(parse(r#"("Hello world!".shift("ab"))[3]"#).unwrap().eval().is_err());
        assert_eq!(parse(r#""Hello world!".shift([])"#).unwrap().eval().unwrap().to_string(), r#""<!>"#);
        assert_eq!(parse(r#""Hello world!".shift("ab".repeat)"#).unwrap().eval().unwrap().to_string(), r#""Igmnp yptmf!""#);
        assert_eq!(parse(r#""ab".repeat.shift(seq)"#).unwrap().eval().unwrap().to_string(), r#""bddffhhjjllnnpprrttv..."#);
        assert_eq!(parse(r#"("ab".repeat.shift(seq))[20]"#).unwrap().eval().unwrap().to_string(), "'v'");
        assert_eq!(parse(r#""abc".shift(['d',5,true])"#).unwrap().eval().unwrap().to_string(), "\"eg<!>");
        test_len_exact(&parse("\"abc\".shift(seq)").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("\"a b c!\".shift(1..3, 1)").unwrap().eval().unwrap(), 6);
        test_len_exact(&parse("\"\".shift(seq)").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse(r#""abcdefghijk".shift(seq, "abcdefghijklmn")"#).unwrap().eval().unwrap());
        test_skip_n(&parse(r#""ab".repeat(10).shift(seq)"#).unwrap().eval().unwrap());
        test_skip_n(&parse(r#""a b".repeat(10).shift(seq)"#).unwrap().eval().unwrap());
        assert_eq!(parse("\"AbC\".shift(3,[0,10,20])").unwrap().eval().unwrap().describe(), "\"AbC\".shift(3, [0, 10, 20])");
        assert_eq!(parse("\"a b c!\".shift(1..3, 1)").unwrap().eval().unwrap().describe(), "\"a b c!\".shift((1..3), 1)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("shift", Shift::eval);
}
