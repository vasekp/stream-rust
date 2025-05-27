use crate::base::*;

fn eval_with(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_source()?);
    if node.args.len() < 2 {
        return Err(StreamError::new("at least 2 arguments required", node));
    }
    let mut args = node.args;
    let body = args.pop().unwrap(); // just checked len > 2 > 1
    let mut env = Rc::clone(env);
    for arg in args {
        let mut args = match arg {
            Expr::Eval(Node { head: Head::Oper(op), source: None, args })
                if op == "="
            => args,
            _ => return Err(StreamError::new("expected assignment", arg))
        };
        let rhs = match args.pop().expect("= should have at least 2 args") {
            Expr::Eval(Node { head: Head::Block(block), source: None, args })
                if args.is_empty()
                => Rhs::Function(*block),
            expr => Rhs::Value(expr.eval(&env)?)
        };
        let mut new_vars = (*env.vars).clone();
        let last = args.pop();
        for name in args {
            let name = match name {
                Expr::Eval(Node { head: Head::Symbol(sym), source: None, args })
                    if args.is_empty()
                => sym,
                _ => return Err(StreamError::new("expected variable name", name))
            };
            new_vars.insert(name, rhs.clone());
        }
        if let Some(name) = last {
            let name = match name {
                Expr::Eval(Node { head: Head::Symbol(sym), source: None, args })
                    if args.is_empty()
                => sym,
                _ => return Err(StreamError::new("expected variable name", name))
            };
            new_vars.insert(name, rhs);
        }
        let mut new_env = Rc::unwrap_or_clone(env);
        new_env.vars = Rc::new(new_vars);
        env = Rc::new(new_env);
    };
    body.eval(&env)
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("with", eval_with);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_with() {
        use crate::parser::parse;
        assert_eq!(parse("with(a=1, a)").unwrap().eval_default().unwrap().to_string(), "1");
        assert!(parse("with(1)").unwrap().eval_default().is_err());
        assert!(parse("with(a=1, b)").unwrap().eval_default().is_err());
        assert!(parse("with(a=a, a)").unwrap().eval_default().is_err());
        assert!(parse("with(a, a)").unwrap().eval_default().is_err());
        assert!(parse("with(1=2, 1)").unwrap().eval_default().is_err());
        assert_eq!(parse("with(a=b=1, a+b)").unwrap().eval_default().unwrap().to_string(), "2");
        assert!(parse("with(a=(b=1), 1)").unwrap().eval_default().is_err());
        assert_eq!(parse("with(a=1, with(a=a+1, a))").unwrap().eval_default().unwrap().to_string(), "2");
        assert_eq!(parse("with(a=5, a=a+4, a)").unwrap().eval_default().unwrap().to_string(), "9");
        // Rewrite existing symbols
        assert_eq!(parse("with(seq=2, seq)").unwrap().eval_default().unwrap().to_string(), "2");
        assert!(parse("with(len=2, seq.len)").unwrap().eval_default().is_err());
        assert_eq!(parse("with(a={5*#1}, a(4))").unwrap().eval_default().unwrap().to_string(), "20");
        assert_eq!(parse("with(a={5*#1}, b={a(#+1)}, 3.b)").unwrap().eval_default().unwrap().to_string(), "20");
        // This fails because the second expression has to be stored unevaluated.
        assert!(parse("with(a={5*#1}, a={a(#+1)}, 3.a)").unwrap().eval_default().is_err());
        assert_eq!(parse("with(df={times@range(#1,1,-2)}, df(10))").unwrap().eval_default().unwrap().to_string(), "3840");
    }
}
