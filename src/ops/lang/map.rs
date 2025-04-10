use crate::base::*;

#[derive(Clone)]
struct Map {
    source: BoxedStream,
    body: Node,
    env: Rc<Env>
}

struct MapIter<'node> {
    parent: &'node Map,
    source: Box<dyn SIterator + 'node>
}

impl Map {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_source(env)?;
        let source = try_with!(node, node.source_checked()?.as_item()?.to_stream()?);
        let body = match node.args[..] {
            [ref expr] => try_with!(node, expr.to_node()?),
            _ => return Err(StreamError::new("exactly 1 argument required", node))
        };
        if body.source.is_some() {
            return Err(StreamError::new("body already has source", node));
        }
        Ok(Item::new_stream(Map{source: source.into(), body, env: Rc::clone(env)}))
    }
}

impl Describe for Map {
    fn describe(&self, prec: u32) -> String {
        let base = Node::describe_helper(&Head::Lang(LangItem::Map), Some(&self.source), [&self.body], prec);
        self.env.wrap_describe(base)
    }
}

impl Stream for Map {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(MapIter{parent: self, source: self.source.iter()})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Iterator for MapIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let source = self.source.next()?;
        let Ok(source) = source else {
            return Some(source)
        };
        let expr = Expr::Eval(Node{
            source: Some(Box::new(source.into())),
            head: self.parent.body.head.clone(),
            args: self.parent.body.args.clone()
        });
        Some(expr.eval_env(&self.parent.env))
    }
}

impl SIterator for MapIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.source.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        self.source.len_remain()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map() {
        use crate::parser::parse;
        assert_eq!(parse("[1,2,3]:{#*10}").unwrap().eval().unwrap().to_string(), "[10, 20, 30]");
        assert_eq!(parse("seq:{#^2}").unwrap().eval().unwrap().to_string(), "[1, 4, 9, 16, 25, ...]");
        assert_eq!(parse("seq:{#1}").unwrap().eval().unwrap().to_string(), "[<!>");
        assert_eq!(parse("seq:{range(#)}").unwrap().eval().unwrap().to_string(), "[[1], [1, 2], ...]");
        test_len_exact(&parse("[1,2,3]:{#}").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("[]:{#}").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse("range(10^10):{#}").unwrap().eval().unwrap());
        test_skip_n(&parse("seq:{#}").unwrap().eval().unwrap());
        assert_eq!(parse("[1,2,3]:{#}").unwrap().eval().unwrap().describe(0), "[1, 2, 3]:{#}");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("$map", Map::eval);
}
