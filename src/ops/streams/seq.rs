use crate::base::*;

#[derive(Clone)]
pub struct Seq {
    head: Head,
    from: Option<Number>,
    step: Option<Number>
}

struct SeqIter<'node> {
    value: Number,
    step: &'node Option<Number>
}

impl Stream for Seq {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SeqIter{
            value: match &self.from {
                Some(from) => from.clone(),
                None => Number::one()
            },
            step: &self.step
        })
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Seq {
    fn eval(node: Node, env: &std::rc::Rc<Env>) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        try_with!(node, node.check_no_source()?);
        let (from, step) = try_with!(node, match node.args[..] {
            [] => (None, None),
            [Item::Number(ref mut from)]
                => (Some(std::mem::take(from)), None),
            [Item::Number(ref mut from), Item::Number(ref mut step)]
                => (Some(std::mem::take(from)), Some(std::mem::take(step))),
            _ => return Err("expected one of: seq(), seq(number), seq(number, number)".into())
        });
        Ok(Item::new_stream(Seq{head: node.head, from, step}))
    }
}

impl Describe for Seq {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, None::<&Item>,
            [self.from.as_ref(), self.step.as_ref()].into_iter().flatten())
    }
}

impl Iterator for SeqIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = Item::new_number(self.value.clone());
        match self.step {
            Some(step) => self.value += step,
            None => self.value.inc()
        }
        Some(Ok(ret))
    }
}

impl SIterator for SeqIter<'_> {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        match self.step {
            Some(step) => self.value += step * Number::from(n),
            None => self.value += Number::from(n)
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_seq() {
        use crate::parser::parse;
        // in addition to doc tests
        assert!(parse("1.seq").unwrap().eval().is_err());
        assert_eq!(parse("seq(0)").unwrap().eval().unwrap().to_string(), "[0, 1, 2, 3, 4, ...]");
        assert_eq!(parse("seq(2, 3)").unwrap().eval().unwrap().to_string(), "[2, 5, 8, 11, 14, ...]");
        assert_eq!(parse("seq(2, 0)").unwrap().eval().unwrap().to_string(), "[2, 2, 2, 2, 2, ...]");
        assert_eq!(parse("seq(2, -3)").unwrap().eval().unwrap().to_string(), "[2, -1, -4, -7, -10, ...]");
        assert_eq!(parse("seq(2, 3)[10^10]").unwrap().eval().unwrap().to_string(), "29999999999");
        assert_eq!(parse("seq(2, 0)[10^10]").unwrap().eval().unwrap().to_string(), "2");
        test_skip_n(&parse("seq(2,0)").unwrap().eval().unwrap());
        test_skip_n(&parse("seq(2,3)").unwrap().eval().unwrap());
        assert_eq!(parse("seq()").unwrap().eval().unwrap().describe(), "seq");
        assert_eq!(parse("seq(0)").unwrap().eval().unwrap().describe(), "seq(0)");
        assert_eq!(parse("seq(1,2)").unwrap().eval().unwrap().describe(), "seq(1, 2)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("seq", Seq::eval);
}
