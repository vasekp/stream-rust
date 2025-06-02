use crate::base::*;

#[derive(Clone)]
struct SplitBy {
    head: Head,
    source: BoxedStream,
    cond: ENode,
    env: Env,
    is_string: bool
}

struct SplitByIter<'node> {
    source: Box<dyn SIterator + 'node>,
    cond: &'node ENode,
    env: &'node Env,
    is_string: bool,
    done: bool,
}

impl SplitBy {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let node = node.eval_source(env)?;
        let is_string = node.source.is_string();
        match node {
            RNodeS { head, source: Item::Stream(stm) | Item::String(stm), args: RArgs::One(Expr::Eval(cond)) } =>
                Ok(Item::new_stream(SplitBy{head, source: stm.into(),
                    cond: cond.eval_all(env)?, env: env.clone(), is_string})),
            _ => Err(StreamError::new("expected: stream.splitby{condition}", node))
        }
    }
}

impl Describe for SplitBy {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.cond], prec, env)
    }
}

impl Stream for SplitBy {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SplitByIter{source: self.source.iter(), cond: &self.cond, env: &self.env, is_string: self.is_string, done: false})
    }

    fn length(&self) -> Length {
        Length::at_most(self.source.length())
    }
}

impl Iterator for SplitByIter<'_> {
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
                .with_source(item.clone().into())?
                .eval(self.env)?);
            let Item::Bool(cond) = cond_item else {
                return Some(Err(StreamError::new(format!("expected bool, found {:?}", cond_item), self.cond.clone())));
            };
            if cond {
                return Some(Ok(Item::new_stream_or_string(List::from(cache), self.is_string)));
            }
            cache.push(item);
        }
        self.done = true;
        Some(Ok(Item::new_stream_or_string(List::from(cache), self.is_string)))
    }
}

impl SIterator for SplitByIter<'_> {
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
    keywords.insert("splitby", SplitBy::eval);
}
