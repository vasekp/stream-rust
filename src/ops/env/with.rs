use crate::base::*;

use std::collections::HashMap;

fn eval_with(node: &Node, env: &Env) -> Result<Item, StreamError> {
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
        ).collect::<Result<Vec<_>, _>>()?;
        let body = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))?;
        let rhs = Rc::new(if let Expr::Eval(node) = &*body
            && let Head::Block(block) = &node.head
            && node.source.is_none() && node.args.is_empty() {
                Rhs::Function(block.clone())
            } else {
                Rhs::Value(body.eval(env)?)
            });
        for name in names {
            replace.insert(name, Rc::clone(&rhs));
        }
    };
    let body = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))?;
    body.eval(env)
}

fn with_replacer<'a>(expr: &'a Expr, replace: &'_ HashMap::<&'_ str, Rc<Rhs>>)
    -> Result<std::borrow::Cow<'a, Expr>, StreamError>
{
    use std::borrow::Cow;
    let Expr::Eval(node) = expr else {
        return Ok(Cow::Borrowed(expr));
    };
    match &**node {
        Node { head: Head::Symbol("global"), .. } =>
            Ok(Cow::Owned(expr.clone())),
        Node { head: Head::Symbol("with"), source: None, .. } => {
            let mut node = (**node).clone();
            let Some((body, assigns)) = node.args.split_last_mut() else {
                return Err(StreamError::usage(&node.head));
            };
            let mut replace = replace.clone();
            for assign in assigns {
                let Expr::Eval(assign_node) = assign else {
                    return Err(StreamError::usage(&node.head));
                };
                let mut new_assign = (**assign_node).clone();
                let (body, names) = if &new_assign.head == "="
                    && new_assign.source.is_none() {
                        new_assign.args.split_last_mut()
                } else {
                    return Err(StreamError::usage(&node.head));
                }.expect("= should have at least 2 arguments by construction");
                if let Cow::Owned(new_body) = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))? { *body = new_body }
                for expr in names {
                    if let Expr::Eval(node) = expr
                        && let Head::Symbol(sym) = node.head
                        && node.source.is_none() && node.args.is_empty() {
                            replace.remove(sym);
                    } else {
                        return Err(StreamError::with_expr("expected variable name", expr))
                    }
                }
                *assign = new_assign.into();
            };
            if let Cow::Owned(new_body) = body.replace(&|sub_expr| with_replacer(sub_expr, &replace))? { *body = new_body }
            eprintln!("{}", node.describe());
            Ok(Cow::Owned(node.into()))
        },
        Node { head: Head::Symbol(sym), source, args } => {
            match replace.get(sym).map(|rc| (**rc).clone()) {
                Some(Rhs::Value(item)) => {
                    if source.is_some() || !args.is_empty() {
                        Err(StreamError::with_expr("no source or arguments allowed", node))
                    } else {
                        Ok(Cow::Owned(item.clone().into()))
                    }
                },
                Some(Rhs::Function(block)) => {
                    let mut new_node = (**node).clone();
                    new_node.head = Expr::new_node("global", None, vec![block.clone()]).into();
                    Ok(Cow::Owned(new_node.into()))
                },
                None => Ok(Cow::Borrowed(expr)),
            }
        },
        _ => Ok(Cow::Borrowed(expr)),
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
