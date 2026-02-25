use crate::base::*;

fn eval_splitby(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_source(env)?;
    match node {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Expr::Eval(cond)) } =>
            Ok(Item::new_stream(SplitBy{head, source: stm.into(), cond: cond.eval_all(env)?, env: env.clone()})),
        RNodeS { head, source: Item::String(stm), args: RArgs::One(Expr::Eval(cond)) } =>
            Ok(Item::new_stream(SplitBy{head, source: stm.into(), cond: cond.eval_all(env)?, env: env.clone()})),
        _ => Err(StreamError::new("expected: stream.splitby{condition}", node))
    }
}

#[derive(Clone)]
struct SplitBy<I: ItemType> {
    head: Head,
    source: BoxedStream<I>,
    cond: ENode,
    env: Env
}

struct SplitByIter<'node, I: ItemType> {
    source: Box<dyn SIterator<I> + 'node>,
    cond: &'node ENode,
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
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SplitByIter{source: self.source.iter(), cond: &self.cond, env: &self.env, done: false})
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl<I: ItemType> Iterator for SplitByIter<'_, I> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let mut cache = vec![];
        for item in &mut self.source {
            check_stop!(iter);
            let item = iter_try_expr!(item);
            let cond_item = iter_try_call!(Node::from(self.cond.clone())
                .with_source(Expr::from(item.clone().into()))?
                .eval(self.env)?);
            let Item::Bool(cond) = cond_item else {
                return Some(Err(StreamError::new(format!("expected bool, found {:?}", cond_item), self.cond.clone())));
            };
            if cond {
                return Some(Ok(Item::from(cache)));
            }
            cache.push(item);
        }
        self.done = true;
        Some(Ok(Item::from(cache)))
    }
}

impl<I: ItemType> SIterator for SplitByIter<'_, I> {
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
A stream of streams: evaluates `cond` on items of `stream`. Every time the condition is `true`, the item is dropped and a new substream starts.
If the input is a string, evaluates `cond` on characters and returns a stream of strings similarly.
! Does not coalesce the delimiters. If multiple items in a row satisfy `item.cond`, the output will contain empty streams / empty strings.
= stream.?{cond}
= string.?{cond}
> [1, 2, -1, 0, 5].?{# < 0} : 6 => [[1, 2], [0, 5]]
> "Hello world".?(?iswhite) => ["Hello", "world"]
> "three   spaces".?(?iswhite) => ["three", "", "", "spaces"]
: split
"#);
}
