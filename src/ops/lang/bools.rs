use crate::base::*;

fn eval_and(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    let res = try_with!(node, node.args.iter()
        .try_fold(true, |acc, arg| arg.to_bool().map(|v| acc & v))?);
    Ok(Item::Bool(res))
}

fn eval_or(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    let res = try_with!(node, node.args.iter()
        .try_fold(false, |acc, arg| arg.to_bool().map(|v| acc | v))?);
    Ok(Item::Bool(res))
}

fn eval_not_xor(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    let res = try_with!(node, match (&node.head, &node.args[..]) {
        (Head::Oper(op), [one]) if op == "!" => one.to_bool().map(|b| !b),
        (Head::Symbol(sym), [one]) if sym == "not" => one.to_bool().map(|b| !b),
        (Head::Symbol(sym), _) if sym == "not" => Err("exactly 1 operand required".into()),
        (_, any) => any.iter().try_fold(false, |acc, arg| arg.to_bool().map(|v| acc ^ v))
    }?);
    Ok(Item::Bool(res))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_bools() {
        use crate::parser::parse;

        assert_eq!(parse("false&false").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("false&true").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("true&false").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("true&true").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("and@[]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("and@[1==1]").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("and@[1==1,1==2]").unwrap().eval_default().unwrap().to_string(), "false");
        // Eval error
        assert!(parse("and@[1==1,1==2,1+'a'==1]").unwrap().eval_default().is_err());
        // Eval to non-bool after short-circuit
        assert!(parse("and@[1==1,1==2,1]").unwrap().eval_default().is_err());

        assert_eq!(parse("false|false").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("true|false").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("true|true").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("false|true").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("or@[]").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("or@[1==1]").unwrap().eval_default().unwrap().to_string(), "true");

        assert_eq!(parse("!false").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("true!true").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("true!true!true").unwrap().eval_default().unwrap().to_string(), "true");
        assert!(parse("!true!true!true").is_err());
        assert_eq!(parse("xor()").unwrap().eval_default().unwrap().to_string(), "false");
        assert!(parse("not()").unwrap().eval_default().is_err());
        assert_eq!(parse("xor(false)").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("not(false)").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("xor(true,false)").unwrap().eval_default().unwrap().to_string(), "true");
        assert!(parse("not(true,false)").unwrap().eval_default().is_err());

        assert_eq!(parse("false&true==false").unwrap().eval_default().unwrap().to_string(), "false");
        assert_eq!(parse("(false&true)==false").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("1==2|2==2").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("true&false|false&true|true&true").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("!true|true").unwrap().eval_default().unwrap().to_string(), "true");
        assert_eq!(parse("with(a=1==2,a)").unwrap().eval_default().unwrap().to_string(), "false");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("&", eval_and);
    keywords.insert("and", eval_and);
    keywords.insert("|", eval_or);
    keywords.insert("or", eval_or);
    keywords.insert("!", eval_not_xor);
    keywords.insert("xor", eval_not_xor);
    keywords.insert("not", eval_not_xor);
}
