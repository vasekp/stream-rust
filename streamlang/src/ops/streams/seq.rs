use crate::base::*;

fn eval_seq(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let (from, step) = match &node.args[..] {
        [] => (None, None),
        [Item::Number(from)] => (Some(from), None),
        [Item::Number(from), Item::Number(to)] => (Some(from), Some(to)),
        _ => return Err(StreamError::usage(&node.head))
    };
    Ok(Item::new_stream(Seq{head: node.head, from: from.cloned(), step: step.cloned() }))
}

pub struct Seq {
    head: Head,
    from: Option<Number>,
    step: Option<Number>
}

impl Stream for Seq {
    fn into_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        SeqIter{
            value: match &self.from {
                Some(from) => from.clone(),
                None => Number::one()
            },
            node: self,
        }.wrap()
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Describe for Seq {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .push_args(&self.from)
            .push_args(&self.step)
            .finish(prec)
    }
}

struct SeqIter {
    node: Rc<Seq>,
    value: Number,
}

impl PreIterator for SeqIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let ret = Item::new_number(self.value.clone());
        match &self.node.step {
            Some(step) => self.value += step,
            None => self.value += 1
        }
        Ok(Some(ret))
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        match &self.node.step {
            Some(step) => self.value += step * Number::from(n),
            None => self.value += Number::from(n)
        }
        Ok(None)
    }

    fn origin(&self) -> &Rc<Seq> {
        &self.node
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
    symbols.insert(["seq", "iota"], eval_seq, r#"
A stream of consecutive numbers. If `from` or `step` are not given, they default to 1.
= ?
= ?(from)
= ?(from, step)
> ? => [1, 2, 3, 4, 5, ...]
> ?(0) => [0, 1, 2, 3, 4, ...]
> ?(0, 2) => [0, 2, 4, 6, 8, ...]
> ?(3, -1) => [3, 2, 1, 0, -1, ...]
: range
"#);
}
