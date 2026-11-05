use crate::base::*;

fn eval_args(node: &Node, env: &Env) -> SResult<Item> {
    debug_assert!(node.args.len() == 2);
    // The parser constructs a bare node here, but a variable substitution (`with`, `$name`) may
    // have replaced it by a value.
    let Expr::Eval(head_node) = &node.args[0] else {
        return Err(StreamError::with_expr("expected function", &node.args[0]));
    };
    debug_assert!(
        head_node.source.is_none() && head_node.args.is_empty(),
        "@ should have a bare node as first argument by construction"
    );
    let head = head_node.head.clone();
    let args = node.args[1].eval(env)?.as_stream()?.listout()?;
    let source = node.source.as_ref().map(|s| s.eval(env)).transpose()?;
    Node::from(Node { head, source, args }).eval(env)
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_raw("[args]", eval_args);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        test_eval!("range@[3]" => "[1, 2, 3]");
        test_eval!("range@range(3)" => "[1]");
        test_eval!("range@range(3)" => "[1]");
        test_eval!("range@[3][2]" => "2");
        test_eval!("range@range(3)[1]" => "1");
        test_eval!("range@3" => err);
        test_eval!("range@seq" => err);
        test_eval!("range@\"ab\"" => err);
        test_eval!("with(a=1, a@[3])" => err);
        test_eval!("with(a=[1], a@[3])" => err);
        test_eval!("with(a={range(#1)}, a@[3])" => "[1, 2, 3]");
        test_describe!("range@[3,4]" => "range(3, 4)");
    }
}
