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
struct SplitBy<ItemType: ItemTypeT> {
    head: Head,
    source: BoxedStream<ItemType>,
    cond: ENode,
    env: Env
}

struct SplitByIter<'node, ItemType: ItemTypeT> {
    source: Box<dyn SIterator<ItemType> + 'node>,
    cond: &'node ENode,
    env: &'node Env,
    done: bool,
}

impl<ItemType: ItemTypeT> Describe for SplitBy<ItemType> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.cond], prec, env)
    }
}

impl<ItemType: ItemTypeT> Stream for SplitBy<ItemType> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SplitByIter{source: self.source.iter(), cond: &self.cond, env: &self.env, done: false})
    }

    fn length(&self) -> Length {
        Length::at_most(self.source.length())
    }
}

impl<ItemType: ItemTypeT> Iterator for SplitByIter<'_, ItemType> {
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

impl<ItemType: ItemTypeT> SIterator for SplitByIter<'_, ItemType> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_split() {
        use super::*;
        use crate::parser::parse;
        test_eval!("\"abc\ndef\".splitby(iswhite)" => "[\"abc\", \"def\"]");
        test_eval!("range(10).splitby(iseven)" => "[[1], [3], [...], ...]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("splitby", eval_splitby);
}
