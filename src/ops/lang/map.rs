use crate::base::*;

#[derive(Clone)]
struct Map {
    source: BoxedStream,
    body: Node,
    head: Head,
    env: Rc<Env>
}

struct MapIter<'node> {
    parent: &'node Map,
    source: Box<dyn SIterator + 'node>
}

impl Map {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        match node.eval_source(env)? {
            RNodeS { head, source: Item::Stream(source), args: RArgs::One(Expr::Eval(body)) } =>
                Ok(Item::new_stream(Map{head, source: source.into(), body, env: Rc::clone(env)})),
            node => Err(StreamError::new("expected: source:body", node))
        }
    }
}

impl Describe for Map {
    fn describe_prec(&self, prec: u32) -> String {
        self.env.wrap_describe(|prec|
            Node::describe_helper(&self.head, Some(&self.source), [&self.body], prec),
            prec)
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
        Some(self.parent.body.clone()
            .with_source(source.into())
            .and_then(|node| Expr::from(node).eval(&self.parent.env)))
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
        assert_eq!(parse("[1,2,3]:{#*10}").unwrap().eval_default().unwrap().to_string(), "[10, 20, 30]");
        assert_eq!(parse("seq:{#^2}").unwrap().eval_default().unwrap().to_string(), "[1, 4, 9, 16, 25, ...]");
        assert_eq!(parse("seq:{#1}").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert_eq!(parse("seq:{range(#)}").unwrap().eval_default().unwrap().to_string(), "[[1], [1, 2], ...]");
        assert_eq!(parse("seq.map{#+#1}(3)").unwrap().eval_default().unwrap().to_string(), "[4, 5, 6, 7, 8, ...]");
        test_len_exact(&parse("[1,2,3]:{#}").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("[]:{#}").unwrap().eval_default().unwrap(), 0);
        test_skip_n(&parse("range(10^10):{#}").unwrap().eval_default().unwrap());
        test_skip_n(&parse("seq:{#}").unwrap().eval_default().unwrap());
        assert_eq!(parse("[1,2,3]:{#}").unwrap().eval_default().unwrap().describe(), "[1, 2, 3]:{#}");
        assert_eq!(parse("seq:{#}").unwrap().eval_default().unwrap().describe(), "seq:{#}");
        assert_eq!(parse("seq.map{#}").unwrap().eval_default().unwrap().describe(), "seq.map({#})");
        assert_eq!(parse("seq.map{#+#1}(1)").unwrap().eval_default().unwrap().describe(), "seq.map({#+#1}(1))");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("$map", Map::eval);
    keywords.insert("map", Map::eval);
}
