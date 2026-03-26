use crate::base::*;

fn eval_while(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let [Expr::Eval(cond)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    Ok(Item::new_stream(While{
        cond: Rc::clone(cond),
        source: stm,
        head: node.head.clone(),
        env: env.clone(),
    }))
}

struct While {
    head: Head,
    source: Rc<dyn Stream>,
    cond: Rc<Node>,
    env: Env
}

struct WhileIter {
    node: Rc<While>,
    source: Box<dyn SIterator>,
}

impl Describe for While {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.cond)
            .finish(prec)
    }
}

impl Stream for While {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        WhileIter{source: self.source.iter(), node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl PreIterator for WhileIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let source = iter_try!(self.source.next());
        let cond = self.node.cond
            .with_source(Expr::from(&source))?
            .eval(&self.node.env)?
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

    fn origin(&self) -> &Rc<While> {
        &self.node
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
