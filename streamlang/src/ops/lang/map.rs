use crate::base::*;

fn eval_map(node: &Node, env: &Env) -> SResult<Item> {
    let body = node.only_arg_checked()?.as_func()?;
    match node.source_checked()?.eval(env)? {
        Item::Stream(source) => {
            Ok(Item::new_stream(Map { head: node.head.clone(), source, body: Function::new(body, env) }))
        },
        Item::String(source) => {
            Ok(Item::new_string(CharMap { head: node.head.clone(), source, body: Function::new(body, env) }))
        },
        _ => Err("expected string or stream".into()),
    }
}

struct Map {
    source: Rc<dyn Stream>,
    body: Function,
    head: Head,
}

impl Describe for Map {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_func(&self.head, env, Some(&self.body))
            .set_source(&self.source)
            .push_arg(&self.body)
            .finish(prec)
    }
}

impl Stream for Map {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let body = self.body.clone();
        Box::new(SMap::new(&self.source, move |item| body.call_source(item), &self))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

struct CharMap {
    source: Rc<dyn Stream<Char>>,
    body: Function,
    head: Head,
}

impl Describe for CharMap {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_func(&self.head, env, Some(&self.body))
            .set_source(&self.source)
            .push_arg(&self.body)
            .finish(prec)
    }
}

impl Stream<Char> for CharMap {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<Char>> {
        let body = self.body.clone();
        Box::new(SMap::new(&self.source, move |ch| body.call_source(Item::Char(ch)).and_then(Item::into_char), &self))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_raw("[map]", eval_map);
    symbols.insert(["foreach", "map"], eval_map, r#"
Applies `func` on each item in `stream`, or on each character of `string`.
The shorthand for `stream.?(func)` or `stream.?{#.func(args)}` is `stream:func` or `stream:func(args)`.
* For a `string` the result is again a string, so `func` must return a character; use `?chars` for other results.
= stream.?{func}
= string.?{func}
= stream:func(args)
= string:func(args)
> ?seq.?{#*3} => [3, 6, 9, 12, 15, ...]
> ?seq.?{#*#1}(5) => [5, 10, 15, 20, 25, ...]
> "abc".?{#+1} => "bcd"
"#);
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
