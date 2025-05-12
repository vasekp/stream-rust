use crate::base::*;

fn eval_args(mut node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let (head_arg, args_arg) = match std::mem::take(&mut node.args).into() {
        RArgs::Two(head_arg, args_arg) => (head_arg, args_arg),
        rargs => {
            node.args = rargs.into();
            return Err(StreamError::new("expected: body@stream or source.body@stream", node));
        }
    };
    let arg_stream = match args_arg.eval(env)? {
        Item::Stream(stream) if stream.length() != Length::Infinite => stream,
        arg @ Item::Stream(..) => return Err(StreamError::new("stream is infinite",
            Node { head: node.head, source: node.source, args: vec![head_arg, arg.into()] })),
        arg => return Err(StreamError::new(format!("expected stream, found {arg:?}"),
            Node { head: node.head, source: node.source, args: vec![head_arg, arg.into()] }))
    };
    if arg_stream.length() == Length::Infinite {
        return Err(StreamError::new("stream is infinite",
            Node { head: node.head, source: node.source, args: vec![head_arg, Item::Stream(arg_stream).into()] }));
    }
    let head = match head_arg {
        Expr::Eval(Node { head, source: None, args }) if args.is_empty() => head,
        _ => return Err(StreamError::new(format!("expected bare symbol or block, found {:?}", head_arg),
            Node { head: node.head, source: node.source, args: vec![head_arg, Item::Stream(arg_stream).into()] }))
    };
    let expr = Expr::Eval(Node{
        head,
        source: node.source,
        args: arg_stream.iter()
            .map(|res| res.map(Expr::from))
            .collect::<Result<Vec<_>, _>>()?
    });
    expr.eval(env)
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
        assert_eq!(parse("range@[3,4]").unwrap().eval_default().unwrap().describe(), "range(3, 4)");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("*args", eval_args);
}
