use crate::base::*;

fn eval_map(node: Node, env: &Env) -> Result<Item, StreamError> {
    match node.eval_source(env)? {
        RNodeS { head, source: Item::Stream(source), args: RArgs::One(Expr::Eval(body)) } =>
            Ok(Item::new_stream(Map{head, source: source.into(), body, env: env.clone()})),
        RNodeS { head, source: Item::String(source), args: RArgs::One(Expr::Eval(body)) } =>
            Ok(Item::new_string(CharMap{head, source: source.into(), body, env: env.clone()})),
        node => Err(StreamError::new("expected: source:body", node))
    }
}

#[derive(Clone)]
struct Map {
    source: BoxedStream,
    body: Node,
    head: Head,
    env: Env
}

impl Describe for Map {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.body], prec, env)
    }
}

impl Stream for Map {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SMap::new(&*self.source, |item| {
            self.body.clone()
                .with_source(item.into())
                .and_then(|node| Expr::from(node).eval(&self.env))
                .map_err(BaseError::from)
        }))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

#[derive(Clone)]
struct CharMap {
    source: BoxedStream<Char>,
    body: Node,
    head: Head,
    env: Env
}

impl Describe for CharMap {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.body], prec, env)
    }
}

impl Stream<Char> for CharMap {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        Box::new(SMap::new(&*self.source, |ch| {
            self.body.clone()
                .with_source(Item::Char(ch).into())
                .and_then(|node| Expr::from(node).eval(&self.env))
                .and_then(|item| item.into_char()
                    .map_err(|err| StreamError::new(err, Item::from(self.source.clone()))))
                .map_err(BaseError::from)
        }))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map() {
        test_eval!("[1,2,3]:{#*10}" => "[10, 20, 30]");
        test_eval!("seq:{#^2}" => "[1, 4, 9, 16, 25, ...]");
        test_eval!("seq:{#1}" => "[<!>");
        test_eval!("seq:{range(#)}" => "[[1], [1, 2], ...]");
        test_eval!("seq.map{#+#1}(3)" => "[4, 5, 6, 7, 8, ...]");
        test_eval!("\"abc\":{#}" => "\"abc\"");
        test_eval!("\"a1b2c\":{if(#.isdigit,'-',#)}" => "\"a-b-c\"");
        test_len!("[1,2,3]:{#}" => 3);
        test_len!("[]:{#}" => 0);
        test_advance("range(10^10):{#}");
        test_advance("seq:{#}");
        test_describe!("[1,2,3]:{#}" => "[1, 2, 3]:{#}");
        test_describe!("seq:{#}" => "seq:{#}");
        test_describe!("seq.map{#}" => "seq.map({#})");
        test_describe!("seq.map{#+#1}(1)" => "seq.map({#+#1}(1))");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("*map", eval_map);
    symbols.insert(["foreach", "map"], eval_map);
}
