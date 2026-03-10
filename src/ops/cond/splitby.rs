use crate::base::*;

fn eval_splitby(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let [Expr::Eval(cond)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    match node.source_checked()?.eval(env)? {
        Item::Stream(stm) => Ok(Item::new_stream(SplitBy{head: node.head.clone(), source: stm, cond: cond.eval_all(env)?, env: env.clone()})),
        Item::String(stm) => Ok(Item::new_stream(SplitBy{head: node.head.clone(), source: stm, cond: cond.eval_all(env)?, env: env.clone()})),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct SplitBy<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    cond: Node<Item>,
    env: Env
}

struct SplitByIter<'node, I: ItemType> {
    source: Box<dyn SIterator<I> + 'node>,
    cond: &'node Node<Item>,
    env: &'node Env,
    done: bool,
}

impl<I: ItemType> Describe for SplitBy<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&self.cond)
            .finish(prec)
    }
}

impl<I: ItemType> Stream for SplitBy<I> {
    fn iter0<'node>(&'node self) -> Result<Box<dyn SIterator<Item> + 'node>, StreamError> {
        Ok(Box::new(SplitByIter{source: self.source.iter(), cond: &self.cond, env: &self.env, done: false}))
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl<I: ItemType> SIterator for SplitByIter<'_, I> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        if self.done {
            return Ok(None);
        }
        let mut cache = vec![];
        for item in self.source.transposed() {
            check_stop!();
            let item = item?;
            let cond = Node::from(self.cond)
                .with_source(Expr::from(item.clone().into()))?
                .eval(self.env)?
                .to_bool()?;
            if cond {
                return Ok(Some(Item::from(cache)));
            }
            cache.push(item);
        }
        self.done = true;
        Ok(Some(Item::from(cache)))
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_split() {
        use super::*;
        test_eval!("\"abc\ndef\".splitby(iswhite)" => "[\"abc\", \"def\"]");
        test_eval!("range(10).splitby(iseven)" => "[[1], [3], [...], ...]");
        test_describe!("range(10).splitby(iseven)" => "range(10).splitby(iseven)");
        test_describe!("range(10).splitby{#<3}" => "range(10).splitby({#<3})");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("splitby", eval_splitby, r#"
Splits `stream` to substreams by evaluating `cond` on its items. Every time the condition is `true`, the item is dropped and a new substream starts.
If the input is a string, evaluates `cond` on characters and returns a stream of strings similarly.
= stream.?{cond}
= string.?{cond}
> [1, 2, -1, 0, 5].?{# < 0} : 6 => [[1, 2], [0, 5]]
> "Hello world".?(?iswhite) => ["Hello", "world"]
> "three   spaces".?(?iswhite) => ["three", "", "", "spaces"] ; does not coalesce the delimiters
: split
"#);
}
