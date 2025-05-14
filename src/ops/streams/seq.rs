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
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_no_source()?;
        let (from, step) = match rnode.args {
            RArgs::Zero => (None, None),
            RArgs::One(Item::Number(from)) => (Some(from), None),
            RArgs::Two(Item::Number(from), Item::Number(to)) => (Some(from), Some(to)),
            _ => return Err(StreamError::new("expected one of: seq(), seq(number), seq(number, number)", rnode))
        };
        Ok(Item::new_stream(Seq{head: rnode.head, from, step}))
    }
}

impl Describe for Seq {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, None::<&Item>,
            [self.from.as_ref(), self.step.as_ref()].into_iter().flatten(), prec)
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
        assert!(parse("1.seq").unwrap().eval_default().is_err());
        assert_eq!(parse("seq(0)").unwrap().eval_default().unwrap().to_string(), "[0, 1, 2, 3, 4, ...]");
        assert_eq!(parse("seq(2, 3)").unwrap().eval_default().unwrap().to_string(), "[2, 5, 8, 11, 14, ...]");
        assert_eq!(parse("seq(2, 0)").unwrap().eval_default().unwrap().to_string(), "[2, 2, 2, 2, 2, ...]");
        assert_eq!(parse("seq(2, -3)").unwrap().eval_default().unwrap().to_string(), "[2, -1, -4, -7, -10, ...]");
        assert_eq!(parse("seq(2, 3)[10^10]").unwrap().eval_default().unwrap().to_string(), "29999999999");
        assert_eq!(parse("seq(2, 0)[10^10]").unwrap().eval_default().unwrap().to_string(), "2");
        test_skip_n(&parse("seq(2,0)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq(2,3)").unwrap().eval_default().unwrap());
        assert_eq!(parse("seq()").unwrap().eval_default().unwrap().describe(), "seq");
        assert_eq!(parse("seq(-1)").unwrap().eval_default().unwrap().describe(), "seq(-1)");
        assert_eq!(parse("seq(-1,2)").unwrap().eval_default().unwrap().describe(), "seq(-1, 2)");
        assert_eq!(parse("iota(-1,2)").unwrap().eval_default().unwrap().describe(), "iota(-1, 2)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("seq", Seq::eval);
    keywords.insert("iota", Seq::eval);
}
