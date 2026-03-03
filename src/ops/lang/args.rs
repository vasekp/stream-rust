use crate::base::*;

fn eval_args(node: &Node, env: &Env) -> Result<Item, StreamError> {
    debug_assert!(node.args.len() == 2);
    let head = if let Expr::Eval(head_node) = &node.args[0]
        && head_node.source.is_none() && head_node.args.is_empty() {
            head_node.head.clone()
    } else {
        panic!("@ should have a bare node as first argument by construction");
    };
    let args = node.args[1].eval(env)?.as_stream()?.listout()?;
    let source = node.source.as_ref().map(|s| s.eval(env)).transpose()?;
    Node::from(ENode { head, source, args }).eval(env)
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
        test_describe!("range@[3,4]" => "range(3, 4)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_raw("[args]", eval_args);
}
