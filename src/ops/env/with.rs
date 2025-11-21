use crate::base::*;

fn eval_with(node: Node, env: &Env) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_source()?);
    if node.args.len() < 2 {
        return Err(StreamError::new("at least 2 arguments required", node));
    }
    let mut args = node.args;
    let body = args.pop().unwrap(); // just checked len > 2 > 1
    let mut env = env.clone();
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
                => Rhs::Function(*block, env.clone()),
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
        env.vars = Rc::new(new_vars);
    };
    body.eval(&env)
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("with", eval_with);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_with() {
        use super::*;

        test_eval!("with(a=1, a)" => "1");
        test_eval!("with(1)" => err);
        test_eval!("with(a=1, b)" => err);
        test_eval!("with(a=a, a)" => err);
        test_eval!("with(a, a)" => err);
        test_eval!("with(1=2, 1)" => err);
        test_eval!("with(a=b=1, a+b)" => "2");
        test_eval!("with(a=(b=1), 1)" => err);
        test_eval!("with(a=1, with(a=a+1, a))" => "2");
        test_eval!("with(a=5, a=a+4, a)" => "9");
        // Rewrite existing symbols
        test_eval!("with(seq=2, seq)" => "2");
        test_eval!("with(len=2, seq.len)" => err);
        test_eval!("with(a={5*#1}, a(4))" => "20");
        test_eval!("with(a={5*#1}, b={a(#+1)}, 3.b)" => "20");
        test_eval!("with(a={5*#1}, a={a(#+1)}, 3.a)" => "20");
        test_eval!("with(df={times@range(#1,1,-2)}, df(10))" => "3840");

        test_eval!("with(a={a}, a)" => err);
        test_eval!("with(a=1, a={a}, a)" => "1");
        test_eval!("with(a=1, b={a}, a=2, b)" => "1");

        test_describe!("with(a=1,[].nest{#:{1}})[3]" => "with(a=1, []:{1}:{1}:{1})");
    }
}
