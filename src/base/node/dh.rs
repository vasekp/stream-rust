use crate::base::*;

pub(crate) fn describe_helper<T, U>(
    head: &Head,
    source: Option<&T>,
    args: impl IntoIterator<Item = U>,
    prec: u32,
    env: &Env)
-> String
    where T: Describe, U: Describe
{
    let mut ret = String::new();
    if let Some(source) = source {
        ret += &source.describe_inner(u32::MAX, env);
        match head {
            Head::Lang(LangItem::Map) => ret.push(':'),
            Head::Lang(LangItem::Part) => (),
            _ => ret.push('.')
        }
    }
    if let Head::Oper(op) = head {
        let nprec = op_prec(op).unwrap_or(0);
        let parens = nprec <= prec;
        if parens {
            ret.push('(');
        }
        let mut it = args.into_iter().map(|arg| arg.describe_inner(nprec, env));
        let first = it.next().expect("Head::Oper should have at least one arg");
        // if len == 1, print {op}{arg}, otherwise {arg}{op}{arg}...
        match it.next() {
            Some(string) => {
                ret += &first;
                ret += op;
                ret += &string;
            },
            None => {
                ret += op;
                ret += &first;
            }
        }
        for string in it {
            ret += op;
            ret += &string;
        }
        if parens {
            ret.push(')');
        }
    } else {
        match head {
            Head::Symbol(s) => ret += s,
            Head::Block(b) => ret += &format!("{{{}}}", b.describe_inner(0, env)),
            _ => ()
        };
        let mut it = args.into_iter().map(|arg| arg.describe_inner(0, env));
        match it.next() {
            Some(first) => {
                match head {
                    Head::Lang(LangItem::Part | LangItem::List) => ret.push('['),
                    Head::Lang(LangItem::Map) => (),
                    _ => ret.push('(')
                }
                ret += &first;
                for s in it {
                    ret += ", ";
                    ret += &s
                }
                match head {
                    Head::Lang(LangItem::Part | LangItem::List) => ret.push(']'),
                    Head::Lang(LangItem::Map) => (),
                    _ => ret.push(')')
                };
            },
            None => if matches!(head, Head::Lang(LangItem::List)) {
                ret += "[]";
            }
        }
    }
    ret
}
