use crate::base::*;

fn eval_args(node: Node, env: &Env) -> Result<Item, StreamError> {
    debug_assert!(node.args.len() == 2);
    let node = node.eval_nth_arg(1, env)?;
    let args_arg = &node.args[1];
    let Expr::Imm(Item::Stream(stm)) = args_arg else {
        return Err(StreamError::new(format!("expected stream, found {:?}", args_arg), node));
    };
    if stm.length() == Length::Infinite {
        return Err(StreamError::new("stream is infinite", node));
    }
    let args = stm.listout()?;
    let Expr::Eval(Node{head, ..}) = node.args.into_iter().next().unwrap() else {
        panic!("@ should have a bare node as first argument by construction");
    };
    let source = node.source.map(|s| s.eval(env)).transpose()?;
    Node::from(ENode { head, source, args }).eval(env)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        use crate::parser::parse;

        assert_eq!(parse("range@[3]").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("range@range(3)").unwrap().eval_default().unwrap().to_string(), "[1]");
        assert_eq!(parse("range@range(3)").unwrap().eval_default().unwrap().to_string(), "[1]");
        assert_eq!(parse("range@[3][2]").unwrap().eval_default().unwrap().to_string(), "2");
        assert_eq!(parse("range@range(3)[1]").unwrap().eval_default().unwrap().to_string(), "1");
        assert!(parse("range@3").unwrap().eval_default().is_err());
        assert!(parse("range@seq").unwrap().eval_default().is_err());
        assert!(parse("range@\"ab\"").unwrap().eval_default().is_err());
        assert_eq!(parse("range@[3,4]").unwrap().eval_default().unwrap().describe(), "range(3, 4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("*args", eval_args);
}
