use crate::base::*;

fn eval_unweave(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_stream()?;
    let count = node.only_arg_checked()?.as_num()?
        .try_cast_within(UNumber::one()..)?;
    Ok(Item::new_stream(Unweave{
        head: node.head.clone(),
        stream: Rc::clone(stm),
        count,
        env: env.clone(),
    }))
}

struct Unweave {
    head: Head,
    stream: Rc<dyn Stream>,
    count: UNumber,
    env: Env,
}

impl Describe for Unweave {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.stream)
            .push_arg(&self.count)
            .finish(prec)
    }
}

impl Stream for Unweave {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        UnweaveIter{index: UNumber::zero(), node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::Exact(self.count.clone())
    }
}

struct UnweaveIter {
    node: Rc<Unweave>,
    index: UNumber,
}

impl PreIterator for UnweaveIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.index < self.node.count {
            let node = Node {
                head: "stride".into(),
                source: Some(Item::from(&self.node.stream).into()),
                args: vec![
                    Item::new_number(&self.node.count).into(),
                    Item::new_number(&self.index).into()
                ]
            };
            self.index += 1;
            Some(node.eval(&self.node.env)).transpose()
        } else {
            Ok(None)
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.index += n;
        if self.index >= self.node.count {
            Ok(Some(&self.index - &self.node.count))
        } else {
            Ok(None)
        }
    }

    fn origin(&self) -> &Rc<Unweave> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unweave() {
        test_eval!("(1..5).unweave(2)" : 10 => "[[1, 3, 5], [2, 4]]");
        test_eval!("seq.unweave(2)" => "[[1, 3, 5, 7, ...], ...]");
        test_eval!("seq.unweave(2)[2]" => "[2, 4, 6, 8, 10, ...]");
        test_len!("(1..10^5).unweave(3)" => 3);
        test_advance("(1..10^10).unweave(10^6)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("unweave", eval_unweave, r#"
Separates `stream` into `count` substreams, items are distributed cyclically one by one.
= stream.?(step)
> (1..6).?(2) : 10 => [[1, 3, 5], [2, 4, 6]]
: stride
: weave
: transpose
: partition
"#);
}
