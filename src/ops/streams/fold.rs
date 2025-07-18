use crate::base::*;
use std::collections::VecDeque;

#[derive(Clone)]
struct Fold {
    head: Head,
    body: ENode,
    source: BoxedStream,
    env: Env
}

struct FoldIter<'node> {
    body: &'node ENode,
    source: Box<dyn SIterator + 'node>,
    prev: VecDeque<Item>,
    env: &'node Env
}

fn eval_fold(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Expr::Eval(body)) }
        if body.source.is_none() && !body.args.is_empty() => {
            Ok(Item::new_stream(Fold{head, body: body.eval_all(env)?, source: stm.into(), env: env.clone()}))
        },
        node => Err(StreamError::new("expected: stream.fold({body}(args))", node))
    }
}

impl Describe for Fold {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.body], prec, env)
    }
}

impl Stream for Fold {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let args = self.body.args.iter().cloned().collect();
        Box::new(FoldIter{body: &self.body, source: self.source.iter(), prev: args, env: &self.env})
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

impl Iterator for FoldIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = iter_try_expr!(self.source.next()?);
        let args = self.prev.iter()
            .map(|item| Expr::Imm(item.to_owned()))
            .collect();
        let node = Node::new(self.body.head.clone(), Some(source.into()), args);
        let item = iter_try_expr!(node.eval(self.env));
        self.prev.pop_front();
        self.prev.push_back(item.clone());
        Some(Ok(item))
    }
}

impl SIterator for FoldIter<'_> {
    fn len_remain(&self) -> Length {
        self.source.len_remain()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fold() {
        use super::*;
        use crate::parser::parse;
        test_eval!("seq.fold{#*#1}(1)" => "[1, 2, 6, 24, 120, ...]");
        test_eval!("(1..20).fold{#*#1}(1).last" => "2432902008176640000");
        test_eval!("seq.fold{#+#1}(0)" => "[1, 3, 6, 10, 15, ...]");
        test_eval!("seq.fold{#+#1+#2}(0,1)" => "[2, 5, 10, 19, 34, ...]");
        test_eval!("seq.fold{#}" => err);
        test_eval!("seq.fold{#+#1+#2}(0)" => "[<!>");
        test_eval!("3.fold{#+#1}(0)" => err);
        test_eval!("3.fold{#+#1}(0)" => err);
        test_eval!("\"abc\".chars.fold{#+#1}('a')" => "['b', 'd', 'g']");
        test_eval!("\"abc\".fold{#+#1}('a')" => err);
        test_describe!("seq.fold{#+#1+#2}(0,1)" => "seq.fold({#+#1+#2}(0, 1))");
        test_eval!("seq.fold{#1~#}([])" => "[[1], [1, 2], ...]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("fold", eval_fold);
}
