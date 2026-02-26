use crate::base::*;

use std::collections::HashMap;

fn eval_with(mut node: Node, env: &Env) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_source()?);
    if node.args.len() < 2 {
        return Err(StreamError::new("at least 2 arguments required", node));
    }
    let (body, assigns) = (node.args.pop().unwrap(), node.args); // just checked len > 2 > 1
    let mut replace = HashMap::<String, Rc<Rhs>>::new();
    for assign in assigns {
        let (mut body, names) = match assign {
            Expr::Eval(Node { head: Head::Oper(op), source: None, mut args })
                if op == "=" && args.len() >= 2
            => (args.pop().unwrap(), args),
            _ => return Err(StreamError::new("expected assignment", assign))
        };
        let names = names.into_iter().map(|expr| match expr {
            Expr::Eval(Node { head: Head::Symbol(sym), source: None, args }) if args.is_empty() => Ok(sym),
            _ => Err(StreamError::new("expected variable name", expr))
        }).collect::<Result<Vec<_>, _>>()?;
        body = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))?;
        let rhs = Rc::new(match body {
            Expr::Eval(Node { head: Head::Block(block), source: None, args })
                if args.is_empty()
                => Rhs::Function(*block),
            expr => Rhs::Value(expr.eval(env)?)
        });
        for name in names {
            replace.insert(name, Rc::clone(&rhs));
        }
    };
    let body = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))?;
    body.eval(env)
}

fn with_replacer(expr: Expr, replace: &HashMap::<String, Rc<Rhs>>)
    -> Result<std::ops::ControlFlow<Expr, Node>, StreamError>
{
    use std::ops::ControlFlow;
    let Expr::Eval(mut node) = expr else { return Ok(ControlFlow::Break(expr)) };
    match node {
        Node { head: Head::Symbol(ref sym), .. } if sym == "global" =>
            Ok(ControlFlow::Break(node.into())),
        Node { head: Head::Symbol(sym), source: None, mut args } if sym == "with" && args.len() > 1 => {
            let (mut body, assigns) = (args.pop().unwrap(), &mut args); // just checked len > 2 > 1
            let mut replace = replace.clone();
            for assign in assigns {
                let Expr::Eval(Node { head: Head::Oper(op), source: None, args }) = assign
                    else { return Err(StreamError::new("expected assignment", assign.clone())) };
                if op != "=" { return Err(StreamError::new("expected assignment", assign.clone())); }
                let mut body = args.pop().expect("Oper(=) should have at least 2 arguments");
                body = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))?;
                for expr in args.iter() {
                    match expr {
                        Expr::Eval(Node { head: Head::Symbol(sym), source: None, args })
                        if args.is_empty() => { replace.remove(sym); },
                        _ => return Err(StreamError::new("expected variable name", assign.clone()))
                    }
                }
                args.push(body);
            };
            body = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))?;
            args.push(body);
            Ok(ControlFlow::Break(Expr::new_node(sym, args)))
        },
        Node { head: Head::Symbol(ref sym), ref mut source, ref mut args } => {
            match replace.get(sym).map(|rc| (**rc).clone()) {
                Some(Rhs::Value(item)) => {
                    if source.is_some() || !args.is_empty() {
                        Err(StreamError::new("no source or arguments allowed", node))
                    } else {
                        Ok(ControlFlow::Break(item.clone().into()))
                    }
                },
                Some(Rhs::Function(block)) => {
                    Ok(ControlFlow::Continue(Node {
                        head: Expr::new_node("global", vec![block.clone()]).into(),
                        source: source.take(),
                        args: std::mem::take(args)
                    }))
                },
                None => Ok(ControlFlow::Continue(node)),
            }
        },
        _ => Ok(ControlFlow::Continue(node))
    }
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

        test_describe!("1.nest{with(a={#+1},#.a)}[3]" => "4");
        test_describe!("[1].nest{with(a={#+1},#:a)}[2]" => "[1]:{global(#+1)}:{global(#+1)}");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("with", eval_with, r#"
Defines or redefines symbols (local variables) for the evaluation of `expr`.
Both values and functions can be assigned: expressions enclosed in `{}` can refer to `#, #1, ...` and are evaluated only when used.
* The assignments are processed from left to right, so one assignment can already be used in defining the next.
= ?(var1=value1, ..., expr)
> ?(str="input", str~", "~str) => "input, input"
> ?(func={#.isodd & #>=3}, (1..5).?filter(func)) => [3, 5]
> ?(a = [3, 4, 5], b = a + 1, b - a) => [1, 1, 1]
> ?(seq=[1, 2, 3], seq) => [1, 2, 3] ; can redefine existing keywords
: alpha
: global
"#);
}
