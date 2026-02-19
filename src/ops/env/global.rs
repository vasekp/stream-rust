use crate::base::*;

fn eval_global(node: Node, _env: &Env) -> Result<Item, StreamError> {
    let node = node.resolve();
    let RNode::NoSource(RNodeNS { args: RArgs::One(body), .. }) = node else {
        return Err(StreamError::new("expected: global(expr)", node));
    };
    body.eval(&Default::default())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_global() {
        use super::*;

        test_eval!("with(len=1, {global(range(3).len)})" => "3");
        test_eval!("with(a=1, a.{global(#+#1)}(a))" => "2");
        test_eval!("with(a={#+#1}, 1.a(2))" => "3");
        test_eval!("with(a={#+#1}, 1.global{a}(2))" => err);
        test_eval!("1.{global(2)}" => "2");
        test_eval!("1.global(2)" => err);
        test_eval!("with(a=1, {global(a)})" => err);
        test_eval!("with(a=1, {global(with(a=#1+1, a))}(a))" => "2");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_with_docs("global", eval_global, r#"
Evaluates `expr` reverting all local definitions and alphabet.
* All globally defined functions (`$name = {}`) are automatically wrapped in `?` to avoid dependende on local environment.
= ?(expr)
> ?alpha("cba", ['a', 'c', 'b'].sort) => ['c', 'b', 'a'] ; unusual ordering of letters
> ?alpha("cba", ?(['a', 'c', 'b'].sort)) => ['a', 'b', 'c'] ; ? fixes that
> ?with(len={1}, ?range(3).?len) => 1 ; ?with can locally overwrite existing symbols
> ?with(len={1}, ?(?range(3).len)) => 3 ; here we revert to the keyword ?len
"#);
}
