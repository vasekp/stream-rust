use crate::base::*;

#[derive(Clone)]
struct Shift {
    head: Head,
    source: BoxedStream,
    args: Vec<Item>,
    alpha: Rc<Alphabet>
}

impl Shift {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        let source = match rnode {
            RNodeS { args: RArgs::Zero, .. } => return Err(StreamError::new("expected at least one argument", rnode)),
            RNodeS { source: Item::String(stm), .. } => stm.into(),
            _ => return Err(StreamError::new(format!("expected string, found {:?}", rnode.source), rnode))
        };
        Ok(Item::new_string2(Shift{
            head: rnode.head,
            source,
            args: rnode.args.into(),
            alpha: Rc::clone(env.alphabet())
        }))
    }

    fn helper(base: &Char, items: &[Item], alpha: &Rc<Alphabet>) -> Result<Item, BaseError> {
        let (index, case) = alpha.ord_case(base)?;
        let ans = items.iter().try_fold(index.into(),
            |a, e| {
                match e {
                    Item::Number(ref num) => Ok(a + num),
                    Item::Char(ref ch) => Ok(a + alpha.ord_case(ch)?.0),
                    _ => Err(BaseError::from(format!("expected number or character, found {:?}", e)))
                }
            })?;
        Ok(Item::new_char(alpha.chr_case(&ans, case)))
    }
}

impl Describe for Shift {
    fn describe_prec(&self, prec: u32) -> String {
        self.alpha.wrap_describe(|prec| Node::describe_helper(&self.head, Some(&self.source), &self.args, prec), prec)
    }
}

impl Stream for Shift {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let base = self.source.string_iter();
        let args_iter = self.args.iter()
            .map(|item| match item {
                Item::Stream(stm) | Item::String(stm) => stm.iter(),
                item => Box::new(std::iter::repeat_with(|| Ok(item.clone())))
            }).collect();
        Box::new(ShiftIter{base, source: &*self.source, args: &self.args, args_iter, alpha: &self.alpha})
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
    alpha: &'node Rc<Alphabet>
}

impl ShiftIter<'_> {
    fn node(&self) -> ENode {
        ENode {
            head: "shift".into(),
            source: Some(Item::Stream(self.source.clone_box())),
            args: self.args.clone() // TODO
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
        if !self.alpha.contains(&ch) {
            return Some(Ok(Item::Char(ch)));
        }

        let rest = self.args_iter.iter_mut()
            .map(Iterator::next)
            .collect::<Option<Result<Vec<_>, _>>>();
        match rest {
            None => Some(Err(StreamError::new("some operand ended earlier than the source", self.node()))),
            Some(Ok(inputs)) => {
                match Shift::helper(&ch, &inputs, self.alpha) {
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
                    if self.alpha.contains(&ch) {
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
        assert_eq!(parse("\"AbC\".shift(3,[0,10,20])").unwrap().eval_default().unwrap().to_string(), "\"DoZ\"");
        assert_eq!(parse("\"Test\".shift(13,13)").unwrap().eval_default().unwrap().to_string(), "\"Test\"");
        assert_eq!(parse(r#""ahoj".shift("bebe")"#).unwrap().eval_default().unwrap().to_string(), "\"cmqo\"");
        assert_eq!(parse(r#""Hello world!".shift(seq)"#).unwrap().eval_default().unwrap().to_string(), r#""Igopt cvzun!""#);
        assert_eq!(parse(r#""Hello world!".shift("ab")"#).unwrap().eval_default().unwrap().to_string(), r#""Ig<!>"#);
        assert_eq!(parse(r#"("Hello world!".shift("ab"))[2]"#).unwrap().eval_default().unwrap().to_string(), "'g'");
        assert!(parse(r#"("Hello world!".shift("ab"))[3]"#).unwrap().eval_default().is_err());
        assert_eq!(parse(r#""Hello world!".shift([])"#).unwrap().eval_default().unwrap().to_string(), r#""<!>"#);
        assert_eq!(parse(r#""Hello world!".shift("ab".repeat)"#).unwrap().eval_default().unwrap().to_string(), r#""Igmnp yptmf!""#);
        assert_eq!(parse(r#""ab".repeat.shift(seq)"#).unwrap().eval_default().unwrap().to_string(), r#""bddffhhjjllnnpprrttv..."#);
        assert_eq!(parse(r#"("ab".repeat.shift(seq))[20]"#).unwrap().eval_default().unwrap().to_string(), "'v'");
        assert_eq!(parse(r#""abc".shift(['d',5,true])"#).unwrap().eval_default().unwrap().to_string(), "\"eg<!>");
        test_len_exact(&parse("\"abc\".shift(seq)").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("\"a b c!\".shift(1..3, 1)").unwrap().eval_default().unwrap(), 6);
        test_len_exact(&parse("\"\".shift(seq)").unwrap().eval_default().unwrap(), 0);
        test_skip_n(&parse(r#""abcdefghijk".shift(seq, "abcdefghijklmn")"#).unwrap().eval_default().unwrap());
        test_skip_n(&parse(r#""ab".repeat(10).shift(seq)"#).unwrap().eval_default().unwrap());
        test_skip_n(&parse(r#""a b".repeat(10).shift(seq)"#).unwrap().eval_default().unwrap());
        assert_eq!(parse("\"AbC\".shift(3,[0,10,20])").unwrap().eval_default().unwrap().describe(), "\"AbC\".shift(3, [0, 10, 20])");
        assert_eq!(parse("\"a b c!\".shift(1..3, 1)").unwrap().eval_default().unwrap().describe(), "\"a b c!\".shift(1..3, 1)");
        assert_eq!(parse("alpha(\"bac\", \"abc\".shift(1))").unwrap().eval_default().unwrap().to_string(), "\"cab\"");
        assert_eq!(parse("alpha(\"bac\", \"abc\".shift(1))").unwrap().eval_default().unwrap().describe(), "alpha(['b', 'a', 'c'], \"abc\".shift(1))");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("shift", Shift::eval);
}
