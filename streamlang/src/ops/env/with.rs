use crate::base::*;

use std::collections::HashMap;
use std::borrow::Cow;

fn eval_with(node: &Node, env: &Env) -> SResult<Item> {
    node.check_no_source()?;
    let (body, assigns) = if let Some((body, assigns)) = node.args.split_last()
        && !assigns.is_empty() {
            (body, assigns)
    } else {
        return Err(StreamError::usage(&node.head));
    };
    let mut replace = HashMap::<&str, Rc<Rhs>>::new();
    for assign in assigns {
        let (body, names) = if let Expr::Eval(node) = assign
            && &node.head == "=" && node.source.is_none() {
                node.args.split_last()
        } else {
            return Err(StreamError::with_expr("expected assignment", assign));
        }.expect("= should have at least 2 arguments by construction");
        let names = names.iter().map(|expr|
            if let Expr::Eval(node) = expr
            && let Head::Symbol(sym) = &node.head
            && node.source.is_none() && node.args.is_empty() {
                Ok(sym)
            } else {
                Err(StreamError::with_expr("expected variable name", expr))
            }
        ).collect::<SResult<Vec<_>>>()?;
        let body = replace_inner(body, &replace)?;
        let rhs = Rc::new(if let Expr::Eval(node) = &*body
            && let Head::Block{body, ..} = &node.head
            && node.source.is_none() && node.args.is_empty() {
                Rhs::Function(body.clone())
            } else {
                Rhs::Value(body.eval(env)?)
            });
        for name in names {
            replace.insert(name, Rc::clone(&rhs));
        }
    };
    replace_inner(body, &replace)?.eval(env)
}

fn replace_inner<'a>(expr: &'a Expr, replace: &'_ HashMap::<&'_ str, Rc<Rhs>>)
    -> SResult<std::borrow::Cow<'a, Expr>>
{
    let Expr::Eval(node) = expr else {
        return Ok(Cow::Borrowed(expr));
    };
    let mut new_node = Cow::Borrowed(&**node);
    if &node.head == "with" {
        let Some((body, assigns)) = node.args.split_last() else {
            return Err(StreamError::usage(&node.head));
        };
        let mut replace = replace.clone();
        for (ix, assign) in assigns.iter().enumerate() {
            let Expr::Eval(assign) = assign else {
                return Err(StreamError::usage(&node.head));
            };
            let mut new_assign = Cow::Borrowed(&**assign);
            let (body, names) = if &assign.head == "="
                && assign.source.is_none() {
                    assign.args.split_last()
            } else {
                return Err(StreamError::usage(&node.head));
            }.expect("= should have at least 2 arguments by construction");
            if let Cow::Owned(new_body) = replace_inner(body, &replace)? {
                new_assign.to_mut().args[assign.args.len() - 1] = new_body;
            }
            for expr in names {
                if let Expr::Eval(node) = expr
                    && let Head::Symbol(sym) = node.head
                    && node.source.is_none() && node.args.is_empty() {
                        replace.remove(sym);
                } else {
                    return Err(StreamError::with_expr("expected variable name", expr))
                }
            }
            if let Cow::Owned(new_assign) = new_assign {
                new_node.to_mut().args[ix] = new_assign.into();
            }
        };
        if let Cow::Owned(new_body) = replace_inner(body, &replace)? {
            new_node.to_mut().args[node.args.len() - 1] = new_body;
        }
        return match new_node {
            Cow::Borrowed(_) => Ok(Cow::Borrowed(expr)),
            Cow::Owned(new_node) => Ok(Cow::Owned(new_node.into())),
        }
    }
    if let Head::Block{body, reset_env: false} = &node.head
        && let Cow::Owned(new_body) = replace_inner(body, &replace)? {
            new_node.to_mut().head = Head::Block{body: new_body, reset_env: false};
    }
    if let Head::Symbol(sym) = node.head {
        match replace.get(sym).map(|rc| (**rc).clone()) {
            Some(Rhs::Value(item)) => {
                return if node.source.is_none() && node.args.is_empty() {
                    Ok(Cow::Owned(item.into()))
                } else {
                    Err(StreamError::with_expr("no source or arguments allowed", node))
                }
            },
            Some(Rhs::Function(body)) =>
                new_node.to_mut().head = Head::Block{body: body.clone(), reset_env: true},
            None => ()
        }
    }
    if let Some(source) = &node.source
        && let Cow::Owned(new_source) = replace_inner(source, replace)? {
            new_node.to_mut().source = Some(new_source);
    }
    for (ix, arg) in node.args.iter().enumerate() {
        if let Cow::Owned(new_arg) = replace_inner(arg, &replace)? {
            new_node.to_mut().args[ix] = new_arg;
        }
    }
    match new_node {
        Cow::Borrowed(_) => Ok(Cow::Borrowed(expr)),
        Cow::Owned(node) => Ok(Cow::Owned(node.into())),
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
        test_eval!("with(a=-1, a={a}, a)" => "-1");
        test_eval!("with(a=1, b={a}, a=2, b)" => "1");

        test_eval!("with(a=1, A=a+1, a)" => "2");
        test_eval!("with(a=1, a=A+1, A)" => "2");

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
"#);
}
