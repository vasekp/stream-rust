use crate::base::*;

fn eval_select(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let [Expr::Eval(cond)] = &node.args[..] else {
        return Err(StreamError::new0("expected: stream.select{cond}"))
    };
    Ok(Item::new_stream(Select{head: node.head.clone(), cond: cond.eval_all(env)?, source: stm, env: env.clone()}))
}

struct Select {
    head: Head,
    source: Rc<dyn Stream>,
    cond: Node<Item>,
    env: Env
}

struct SelectIter<'node> {
    cond: &'node Node<Item>,
    source: Box<dyn SIterator + 'node>,
    env: &'node Env
}

impl Describe for Select {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&self.cond)
            .finish(prec)
    }
}

impl Stream for Select {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SelectIter{cond: &self.cond, source: self.source.iter(), env: &self.env})
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl SIterator for SelectIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        loop {
            check_stop!();
            let source = iter_try!(self.source.next());
            let cond_item = Node::from(self.cond.clone())
                .with_source(source.clone().into())?
                .eval(self.env)?;
            let Item::Bool(cond) = cond_item else {
                return Err(StreamError::new(format!("expected bool, found {:?}", cond_item), self.cond.clone()));
            };
            if cond {
                return Ok(Some(source));
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_select() {
        use super::*;
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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["select", "filter"], eval_select, r#"
Evaluates `cond` on every item of `stream` and returns only thise items for which it gave `true`.
= stream.?{cond}
> [1, 2, -1, 0, 5].?{# > 0} => [1, 2, 5]
> ?range(10).?(?isodd) => [1, 3, 5, 7, 9]
: countif
: while
: if
"#);
}
