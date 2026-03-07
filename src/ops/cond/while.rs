use crate::base::*;

fn eval_while(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let [Expr::Eval(cond)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    Ok(Item::new_stream(While{head: node.head.clone(), cond: cond.eval_all(env)?, source: stm, env: env.clone()}))
}

struct While {
    head: Head,
    source: Rc<dyn Stream>,
    cond: Node<Item>,
    env: Env
}

struct WhileIter<'node> {
    cond: &'node Node<Item>,
    source: Box<dyn SIterator + 'node>,
    env: &'node Env
}

impl Describe for While {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&self.cond)
            .finish(prec)
    }
}

impl Stream for While {
    fn iter0<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(WhileIter{cond: &self.cond, source: self.source.iter(), env: &self.env})
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl SIterator for WhileIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        let source = iter_try!(self.source.next());
        let cond = Node::from(self.cond)
            .with_source(source.clone().into())?
            .eval(self.env)?
            .to_bool()?;
        if cond {
            Ok(Some(source))
        } else {
            Ok(None)
        }
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_while() {
        use super::*;
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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("while", eval_while, r#"
Evaluates `cond` on items of `stream` and returns them as long as the result is `true`.
(The first item for which `item.cond == false` stops the stream.}
= stream.?{cond}
> [1, 2, -1, 0, 5].?{# > 0} => [1, 2]
> ?range(10).?(?isodd) => [1]
: select
"#);
}
