use crate::base::*;

use std::collections::HashMap;
use std::borrow::Cow;

fn eval_with(node: &Node, env: &Env) -> SResult<Item> {
    node.check_no_source()?;
    let (body, assigns) = if let Some((body, assigns)) = node.args.split_last()
        && !assigns.is_empty()
    {
        (body, assigns)
    } else {
        return Err(StreamError::usage(&node.head));
    };
    let mut replace = HashMap::<&str, Rc<Rhs>>::new();
    for assign in assigns {
        let (body, names) = if let Expr::Eval(node) = assign
            && &node.head == "="
            && node.source.is_none()
        {
            node.args.split_last()
        } else {
            return Err(StreamError::with_expr("expected assignment", assign));
        }
        .expect("= should have at least 2 arguments by construction");
        let names = names
            .iter()
            .map(|expr| {
                if let Expr::Eval(node) = expr
                    && let Head::Symbol(sym) = &node.head
                    && node.source.is_none()
                    && node.args.is_empty()
                {
                    Ok(sym)
                } else {
                    Err(StreamError::with_expr("expected variable name", expr))
                }
            })
            .collect::<SResult<Vec<_>>>()?;
        let body = replace_inner(body, &replace)?;
        let rhs = Rc::new(
            if let Expr::Eval(node) = &*body
                && node.source.is_none()
                && node.args.is_empty()
                && let Some(closure) = match &node.head {
                    Head::Block(body) => Some(Closure::new(body.clone(), env.clone())),
                    Head::Closure(closure) => Some(closure.clone()),
                    _ => None,
                }
            {
                Rhs::Function(closure)
            } else {
                Rhs::Value(body.eval(env)?)
            },
        );
        for name in names {
            replace.insert(name, Rc::clone(&rhs));
        }
    }
    replace_inner(body, &replace)?.eval(env)
}

fn replace_inner<'a>(expr: &'a Expr, replace: &'_ HashMap<&'_ str, Rc<Rhs>>) -> SResult<std::borrow::Cow<'a, Expr>> {
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
            let (body, names) = if &assign.head == "=" && assign.source.is_none() {
                assign.args.split_last()
            } else {
                return Err(StreamError::usage(&node.head));
            }
            .expect("= should have at least 2 arguments by construction");
            if let Cow::Owned(new_body) = replace_inner(body, &replace)? {
                new_assign.to_mut().args[assign.args.len() - 1] = new_body;
            }
            for expr in names {
                if let Expr::Eval(node) = expr
                    && let Head::Symbol(sym) = node.head
                    && node.source.is_none()
                    && node.args.is_empty()
                {
                    replace.remove(sym);
                } else {
                    return Err(StreamError::with_expr("expected variable name", expr));
                }
            }
            if let Cow::Owned(new_assign) = new_assign {
                new_node.to_mut().args[ix] = new_assign.into();
            }
        }
        if let Cow::Owned(new_body) = replace_inner(body, &replace)? {
            new_node.to_mut().args[node.args.len() - 1] = new_body;
        }
        return match new_node {
            Cow::Borrowed(_) => Ok(Cow::Borrowed(expr)),
            Cow::Owned(new_node) => Ok(Cow::Owned(new_node.into())),
        };
    }
    // A closure (`${...}`, or an already substituted function or value) is opaque to locals.
    if let Head::Block(body) = &node.head
        && let Cow::Owned(new_body) = replace_inner(body, replace)?
    {
        new_node.to_mut().head = Head::Block(new_body);
    }
    if let Head::Symbol(sym) = node.head {
        match replace.get(sym).map(|rc| (**rc).clone()) {
            Some(Rhs::Value(item)) => {
                return if node.source.is_none() && node.args.is_empty() {
                    Ok(Cow::Owned(splice_value(item)))
                } else {
                    Err(StreamError::with_expr("no source or arguments allowed", node))
                };
            },
            Some(Rhs::Function(closure)) => new_node.to_mut().head = Head::Closure(closure),
            None => (),
        }
    }
    if let Some(source) = &node.source
        && let Cow::Owned(new_source) = replace_inner(source, replace)?
    {
        new_node.to_mut().source = Some(new_source);
    }
    for (ix, arg) in node.args.iter().enumerate() {
        if let Cow::Owned(new_arg) = replace_inner(arg, replace)? {
            new_node.to_mut().args[ix] = new_arg;
        }
    }
    match new_node {
        Cow::Borrowed(_) => Ok(Cow::Borrowed(expr)),
        Cow::Owned(node) => Ok(Cow::Owned(node.into())),
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("with", eval_with, r#"
Defines or redefines symbols (local variables) for the evaluation of `expr`.
Both values and functions can be assigned: expressions enclosed in `{}` can refer to `#, #1, ...` and are evaluated only when used.
* The assignments are processed from left to right, so one assignment can already be used in defining the next.
* Scoping is lexical: a function body sees the variables and the alphabet in effect where it was defined, not where it is used.
* Blocks `${...}` do not see local variables and use the default alphabet, which makes them the way to reach a keyword hidden by a local.
= ?(var1=value1, ..., expr)
> ?(str="input", str~", "~str) => "input, input"
> ?(func={#.isodd & #>=3}, (1..5).?filter(func)) => [3, 5]
> ?(a = [3, 4, 5], b = a + 1, b - a) => [1, 1, 1]
> ?(seq=[1, 2, 3], seq) => [1, 2, 3] ; can redefine existing keywords
> ?(a=1, b={a+#}, a=10, 2.b) => 3 ; `b` keeps the `a` it was defined with
> ?(len={1}, [1, 2].len) => 1
> ?(len={1}, ${[1, 2].len}) => 2
: alpha
"#);
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

        test_eval!("with(len={1}, range(3).len)" => "1");
        test_eval!("with(len={1}, ${range(3).len})" => "3");
        test_eval!("with(a=1, a.${#+#1}(a))" => "2");
        test_eval!("with(a={#+#1}, 1.a(2))" => "3");
        test_eval!("with(a=${#+#1}, 1.a(2))" => "3");
        test_eval!("with(a={#+#1}, 1.${a}(2))" => err);
        test_eval!("with(a=1, ${a})" => err);
        test_eval!("with(a=1, ${with(a=#1+1, a)}(a))" => "2");

        test_describe!("1.nest{with(a={#+1},#.a)}[3]" => "4");
        test_describe!("[1].nest{with(a={#+1},#:a)}[2]" => "[1]:${#+1}:${#+1}");
    }

    #[test]
    fn test_with_scoping() {
        use super::*;

        // A function sees the alphabet in effect where it was defined, not where it is used.
        test_eval!("alpha(\"cba\", with(f={#+1}, \"abc\".f))" => "\"cab\"");
        test_eval!("with(f={#+1}, alpha(\"cba\", \"abc\".f))" => "\"bcd\"");
        test_eval!("with(f={#+1}, alpha(\"cba\", with(g={#.f}, \"abc\".g)))" => "\"bcd\"");
        // The source and arguments of a call belong to the call site.
        test_eval!("with(f={#}, alpha(\"cba\", (\"a\"+1).f))" => "\"c\"");
        test_eval!("alpha(\"cba\", with(f={#}, (\"a\"+1).f))" => "\"c\"");
        test_eval!("with(f={#+#1}, alpha(\"cba\", 'a'.f('a'+1)))" => "'d'");
        // A `${...}` right-hand side keeps the default environment; `{...}` captures the current one.
        test_eval!("with(a=${#+#1}, 1.a(2))" => "3");
        test_eval!("alpha(\"cba\", with(a=${'a'+#}, 1.a))" => "'b'");
        test_eval!("alpha(\"cba\", with(a={'a'+#}, 1.a))" => "'c'");
        // A `${...}` block is opaque to locals and uses the default alphabet.
        test_eval!("with(len={1}, ${range(3).len})" => "3");
        test_eval!("alpha(\"cba\", with(f={1}, ${['a', 'c', 'b'].sort}))" => "['a', 'b', 'c']");
        test_eval!("alpha(\"cba\", alpha([], ['a', 'c', 'b'].sort))" => "['a', 'b', 'c']");

        // A closure describes relative to the default environment, wherever it appears, and an op
        // whose function is a closure needs no `alpha(...)` wrapper of its own.
        test_describe!("alpha(\"cba\", with(f={#+1}, seq:f))" => "seq:${alpha(['c', 'b', 'a'], #+1)}");
        test_describe!("with(f={#+1}, alpha(\"cba\", seq:f))" => "seq:${#+1}");
        test_describe!("alpha(\"cba\", seq:{'a'+#})" => "alpha(['c', 'b', 'a'], seq:{'a'+#})");
        test_describe!("alpha(\"cba\", seq:ucase)" => "alpha(['c', 'b', 'a'], seq:ucase)");
        test_describe!("alpha(\"cba\", with(f={'a'+#}, [1]:{with(x=#, x.f)}))" => "alpha(['c', 'b', 'a'], [1]:{with(x=#, x.${alpha(['c', 'b', 'a'], 'a'+#)})})");

        // Values whose description is free of symbols are substituted as they are; any other value
        // is substituted as a closure, so that a `with` in the description cannot capture it.
        test_describe!("with(n=3, seq:{#*n})" => "seq:{#*3}");
        test_describe!("with(a=[1, 2], seq:{#~a})" => "seq:{#~[1, 2]}");
        test_describe!("with(s=\"ab\", seq:{#~s})" => "seq:{#~\"ab\"}");
        test_describe!("with(a=[[1], 'a', true], seq:{#~a})" => "seq:{#~[[1], 'a', true]}");
        test_describe!("with(a=[seq], seq:{#~a})" => "seq:{#~${[seq]}}");
        test_describe!("with(a=seq:{#+1}, seq:{#~a})" => "seq:{#~${seq:{#+1}}}");
        test_describe!("with(s=\"ab\".ucase, seq:{#~s})" => "seq:{#~${\"ab\".ucase}}");
        test_describe!("with(a=seq:{#+1}, a~[1])" => "seq:{#+1}~[1]");
        // ... including when the value is in error, which must survive the round trip.
        test_describe!("with(v=seq:{x+#}, [1]:{with(x=5, v)})" => "[1]:{with(x=5, ${seq:{x+#}})}");
        test_eval!("with(v=seq:{x+#}, [1]:{with(x=5, v)})" : 5 => "[[<!>");
        test_eval!("[1]:{with(x=5, seq:{x+#})}" : 5 => "[[6, 7, 8, 9, ...]]");
        test_describe!("with(v=[[1], [2, 3]]:len, [1]:{with(len={1}, v)})" => "[1]:{with(len={1}, ${[[1], [2, 3]]:len})}");
        test_eval!("with(v=[[1], [2, 3]]:len, [1]:{with(len={1}, v)})" => "[[1, 2]]");
        test_eval!("[1]:{with(len={1}, [[1], [2, 3]]:len)}" => "[[1, 1]]");
    }
}
