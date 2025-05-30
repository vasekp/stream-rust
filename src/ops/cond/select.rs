use crate::base::*;

#[derive(Clone)]
struct Select {
    head: Head,
    source: BoxedStream,
    body: ENode,
    env: Env
}

struct SelectIter<'node> {
    body: &'node ENode,
    source: Box<dyn SIterator + 'node>,
    env: &'node Env
}

fn eval_select(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    match rnode {
        RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Expr::Eval(body)) }
        if body.source.is_none() => {
            Ok(Item::new_stream(Select{head, body: body.eval_all(env)?, source: stm.into(), env: env.clone()}))
        },
        node => Err(StreamError::new("expected: stream.select{cond}", node))
    }
}

impl Describe for Select {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.body], prec, env)
    }
}

impl Stream for Select {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SelectIter{body: &self.body, source: self.source.iter(), env: &self.env})
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
            let source = match self.source.next()? {
                Ok(item) => item,
                Err(err) => return Some(Err(err))
            };
            let cond_item = match Node::from(self.body.clone())
                .with_source(source.clone().into())
                .and_then(|node| node.eval(self.env)) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err))
            };
            let Item::Bool(cond) = cond_item else {
                return Some(Err(StreamError::new(format!("expected bool, found {:?}", cond_item), self.body.clone())));
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
        assert_eq!(parse("range(5).select{true}").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3, 4, 5]");
        assert_eq!(parse("range(5).select{false}").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("range(5).select{#}").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert_eq!(parse("seq.select{#>#1}(5)").unwrap().eval_default().unwrap().to_string(), "[6, 7, 8, 9, 10, ...]");
        test_len_exact(&parse("range(5).select{true}").unwrap().eval_default().unwrap(), 5);
        test_len_exact(&parse("range(5).select{false}").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("range(5).select{#<3}").unwrap().eval_default().unwrap(), 2);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("select", eval_select);
}
