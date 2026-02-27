use crate::base::*;

//Args

pub(crate) fn describe_helper<T, U>(
    head: &Head,
    source: Option<&T>,
    args: impl Iterator<Item = U>,
    prec: u32,
    env: &Env)
-> String
    where T: Describe, U: Describe
{
    let src_str = source.map(|src| src.describe_inner(u32::MAX, env));
    match head {
        Head::Symbol(sym) => desc_basic(sym, src_str, args, env),
        Head::Block(b) => desc_basic(&format!("{{{}}}", b.describe_inner(0, env)), src_str, args, env),
        Head::Oper(op) => desc_oper(op, args, prec, env),
        Head::Lang(LangItem::List) => desc_list(args, env),
        Head::Lang(LangItem::Part) => desc_part(src_str, args, env),
        Head::Lang(LangItem::Map) => desc_map(src_str, args, env),
        Head::Lang(LangItem::Args) => desc_at_args(src_str, args, env),
    }
}

fn desc_args<U>(args: impl Iterator<Item = U>, env: &Env, prec: u32, sep: &str) -> (String, usize)
where U: Describe {
    let mut it = args.map(|arg| arg.describe_inner(prec, env));
    let Some(mut ret) = it.next() else {
        return (String::new(), 0);
    };
    let mut count = 1;
    for string in it {
        ret += sep;
        ret += &string;
        count += 1;
    }
    (ret, count)
}

fn desc_basic<U>(head_str: &str, source: Option<String>, args: impl Iterator<Item = U>, env: &Env) -> String
where U: Describe {
    let mut ret = if let Some(src) = source { format!("{src}.") } else { String::new() };
    ret += head_str;
    match desc_args(args, env, 0, ", ") {
        (_, 0) => (),
        (args_str, _) => {
            ret.push('(');
            ret += &args_str;
            ret.push(')');
        }
    }
    ret
}

fn desc_list<U>(args: impl Iterator<Item = U>, env: &Env) -> String
where U: Describe {
    match desc_args(args, env, 0, ", ") {
        (_, 0) => "[]".into(),
        (args_str, _) => {
            let mut ret = String::from("[");
            ret += &args_str;
            ret.push(']');
            ret
        }
    }
}

fn desc_part<U>(source: Option<String>, args: impl Iterator<Item = U>, env: &Env) -> String
where U: Describe {
    let mut ret = source.expect("*part should have source by construction");
    ret.push('[');
    ret += &desc_args(args, env, 0, ", ").0;
    ret.push(']');
    ret
}

fn desc_map<U>(source: Option<String>, mut args: impl Iterator<Item = U>, env: &Env) -> String
where U: Describe {
    let mut ret = source.expect("*map should have source by construction");
    ret.push(':');
    let first = args.next().expect("*map should have one argument");
    ret += &first.describe_inner(0, env);
    ret
}

fn desc_oper<U>(op: &str, args: impl Iterator<Item = U>, prec: u32, env: &Env) -> String
where U: Describe {
    let nprec = op_prec(op).unwrap_or(0);
    let (lparen, rparen) = if nprec <= prec { ("(", ")") } else { ("", "") };
    let (args_str, count) = desc_args(args, env, nprec, op);
    if count == 1 {
        format!("{lparen}{op}{args_str}{rparen}")
    } else {
        format!("{lparen}{args_str}{rparen}")
    }
}

fn desc_at_args<U>(source: Option<String>, mut args: impl Iterator<Item = U>, env: &Env) -> String
where U: Describe {
    let mut ret = if let Some(src) = source { format!("{src}.") } else { String::new() };
    let head = args.next()
        .expect("*args should have one argument")
        .describe_inner(u32::MAX, env);
    let argsrc = args.next()
        .expect("*args should have one argument")
        .describe_inner(u32::MAX, env);
    ret += &format!("{head}@({argsrc})");
    ret
}
