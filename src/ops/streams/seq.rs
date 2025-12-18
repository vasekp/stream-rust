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

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Seq {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
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
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, None::<&Item>,
            [self.from.as_ref(), self.step.as_ref()].into_iter().flatten(), prec, env)
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

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
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
        test_eval!("1.seq" => err);
        test_eval!("seq(0)" => "[0, 1, 2, 3, 4, ...]");
        test_eval!("seq(2, 3)" => "[2, 5, 8, 11, 14, ...]");
        test_eval!("seq(2, 0)" => "[2, 2, 2, 2, 2, ...]");
        test_eval!("seq(2, -3)" => "[2, -1, -4, -7, -10, ...]");
        test_eval!("seq(2, 3)[10^10]" => "29999999999");
        test_eval!("seq(2, 0)[10^10]" => "2");
        test_advance("seq(2,0)");
        test_advance("seq(2,3)");
        test_describe!("seq()" => "seq");
        test_describe!("seq(-1)" => "seq(-1)");
        test_describe!("seq(-1,2)" => "seq(-1, 2)");
        test_describe!("iota(-1,2)" => "iota(-1, 2)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_with_docs(["seq", "iota"], Seq::eval, crate::docs::parse_docs("
A ?stream of consecutive numbers. If `from` or `step` are not given, they default to 1.
= ??
= ??(from)
= ??(from, step)
> ?? => [1, 2, 3, 4, 5, ...]
> ??(0) => [0, 1, 2, 3, 4, ...]
> ??(0, 2) => [0, 2, 4, 6, 8, ...]
> ??(3, -1) => [3, 2, 1, 0, -1, ...]
> ??('a', 1) => !unsupported arguments
: range
"));
}
