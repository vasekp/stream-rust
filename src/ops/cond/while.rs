use crate::base::*;

#[derive(Clone)]
struct While {
    head: Head,
    source: BoxedStream,
    cond: ENode,
    env: Env
}

struct WhileIter<'node> {
    cond: &'node ENode,
    source: Box<dyn SIterator + 'node>,
    env: &'node Env
}

fn eval_while(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Expr::Eval(cond)) } =>
            Ok(Item::new_stream(While{head, cond: cond.eval_all(env)?, source: stm.into(), env: env.clone()})),
        node => Err(StreamError::new("expected: stream.while{cond}", node))
    }
}

impl Describe for While {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.cond], prec, env)
    }
}

impl Stream for While {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(WhileIter{cond: &self.cond, source: self.source.iter(), env: &self.env})
    }

    fn length(&self) -> Length {
        Length::at_most(self.source.length())
    }
}

impl Iterator for WhileIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = iter_try_expr!(self.source.next()?);
        let cond_item = iter_try_call!(Node::from(self.cond.clone())
            .with_source(source.clone().into())?
            .eval(self.env)?);
        let Item::Bool(cond) = cond_item else {
            return Some(Err(StreamError::new(format!("expected bool, found {:?}", cond_item), self.cond.clone())));
        };
        if cond {
            Some(Ok(source))
        } else {
            None
        }
    }
}

impl SIterator for WhileIter<'_> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_while() {
        use super::*;
        use crate::parser::parse;
        test_eval!("range(5).while{true}" => "[1, 2, 3, 4, 5]");
        test_eval!("range(5).while{false}" => "[]");
        test_eval!("range(5).while{#<3}" => "[1, 2]");
        test_eval!("seq.while{#<#1}(5)" => "[1, 2, 3, 4]");
        test_len!("range(5).while{true}" => 5);
        test_len!("range(5).while{false}" => 0);
        test_len!("range(5).while{#<3}" => 2);
        test_len!("seq.while{#<3}" => 2);
        test_eval!("seq.while(isodd)" => "[1]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("while", eval_while);
}
