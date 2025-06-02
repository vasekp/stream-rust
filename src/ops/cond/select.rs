use crate::base::*;

#[derive(Clone)]
struct Select {
    head: Head,
    source: BoxedStream,
    cond: ENode,
    env: Env
}

struct SelectIter<'node> {
    cond: &'node ENode,
    source: Box<dyn SIterator + 'node>,
    env: &'node Env
}

fn eval_select(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Expr::Eval(cond)) } =>
            Ok(Item::new_stream(Select{head, cond: cond.eval_all(env)?, source: stm.into(), env: env.clone()})),
        node => Err(StreamError::new("expected: stream.select{cond}", node))
    }
}

impl Describe for Select {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.cond], prec, env)
    }
}

impl Stream for Select {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SelectIter{cond: &self.cond, source: self.source.iter(), env: &self.env})
    }

    fn length(&self) -> Length {
        Length::at_most(self.source.length())
    }
}

impl Iterator for SelectIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            let source = iter_try_expr!(self.source.next()?);
            let cond_item = iter_try_call!(Node::from(self.cond.clone())
                .with_source(source.clone().into())?
                .eval(self.env)?);
            let Item::Bool(cond) = cond_item else {
                return Some(Err(StreamError::new(format!("expected bool, found {:?}", cond_item), self.cond.clone())));
            };
            if cond {
                return Some(Ok(source));
            }
        }
    }
}

impl SIterator for SelectIter<'_> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_select() {
        use super::*;
        use crate::parser::parse;
        test_eval!("range(5).select{true}" => "[1, 2, 3, 4, 5]");
        test_eval!("range(5).select{false}" => "[]");
        test_eval!("range(5).select{#}" => "[<!>");
        test_eval!("seq.select{#>#1}(5)" => "[6, 7, 8, 9, 10, ...]");
        test_eval!("range(5).select([].len)" => "[<!>");
        test_eval!("[].select([].len)" => "[]");
        test_eval!("[].select{1}" => "[]");
        test_eval!("[].select(1)" => err);
        test_len!("range(5).select{true}" => 5);
        test_len!("range(5).select{false}" => 0);
        test_len!("range(5).select{#<3}" => 2);
        test_eval!("seq.select(isodd)" => "[1, 3, 5, 7, 9, ...]");
        // Short-circuiting
        test_eval!("[1,2,'a','á'].select{#.isnum&#.isodd|#.ischar&#.isalpha}" => "[1, 'a']");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("select", eval_select);
}
