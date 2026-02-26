use crate::base::*;

fn eval_and(mut node: Node, env: &Env) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_source()?);
    for arg in std::mem::take(&mut node.args) {
        let val = try_with!(node, arg.eval(env)?.to_bool()?);
        if !val { return Ok(Item::Bool(false)); }
    }
    Ok(Item::Bool(true))
}

fn eval_or(mut node: Node, env: &Env) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_source()?);
    for arg in std::mem::take(&mut node.args) {
        let val = try_with!(node, arg.eval(env)?.to_bool()?);
        if val { return Ok(Item::Bool(true)); }
    }
    Ok(Item::Bool(false))
}

fn eval_not(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    match &node.args[..] {
        [Item::Bool(b)] => Ok(Item::Bool(!b)),
        _ => Err(StreamError::new("one bool argument expected", node))
    }
}

fn eval_xor(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    let res = try_with!(node, node.args.iter().try_fold(false, |acc, arg| arg.to_bool().map(|v| acc ^ v))?);
    Ok(Item::Bool(res))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_bools() {
        use super::*;

        test_eval!("false&false" => "false");
        test_eval!("false&true" => "false");
        test_eval!("true&false" => "false");
        test_eval!("true&true" => "true");
        test_eval!("and@[]" => "true");
        test_eval!("and@[1==1]" => "true");
        test_eval!("and@[1==1,1==2]" => "false");
        // Eval error avoided by short circuit
        test_eval!("1==1&1==2&1+'a'==1" => "false");
        // Here it happens because evaluation reaches it
        test_eval!("1==1&1+'a'==1" => err);
        // Here it happens because @ evaulates all
        test_eval!("and@[1==1,1==2,1+'a'==1]" => err);
        // Eval to non-bool after short-circuit
        test_eval!("and@[1==1,1==2,1]" => "false");

        test_eval!("false|false" => "false");
        test_eval!("true|false" => "true");
        test_eval!("true|true" => "true");
        test_eval!("false|true" => "true");
        test_eval!("or@[]" => "false");
        test_eval!("or@[1==1]" => "true");
        // Short-circuit
        test_eval!("1==2|1==1|1+'a'==1" => "true");
        test_eval!("1==2|1+'a'==1" => err);

        test_eval!("!false" => "true");
        test_eval!("xor()" => "false");
        test_eval!("not()" => err);
        test_eval!("xor(false)" => "false");
        test_eval!("not(false)" => "true");
        test_eval!("xor(true,false)" => "true");
        test_eval!("not(true,false)" => err);

        test_eval!("false&true==false" => "false");
        test_eval!("(false&true)==false" => "true");
        test_eval!("1==2|2==2" => "true");
        test_eval!("true&false|false&true|true&true" => "true");
        test_eval!("!true|true" => "true");
        test_eval!("with(a=1==2,a)" => "false");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("and", eval_and, r#"
Logical `AND` of all inputs: evaluates to `true` only if all `inputM` are `true`, `false` otherwise.
The shorthand for `?(input1, input2, ...)` is `input1 & input2 & ...`.
* Allows short-circuting: if any `inputM` is `false`, does not evaluate the rest.
= ?(input1, input2, ...)
> [true, true, false, false].?windows(2, ?and) => [true, false, false]
: &
: or
: not
: xor
"#);
    symbols.insert("&", eval_and, r#"
Logical `AND` of all inputs: evaluates to `true` only if all `inputM` are `true`, `false` otherwise.
* Allows short-circuting: if any `inputM` is `false`, does not evaluate the rest.
= input1 ? input2 ? ...
> 1+1 == 2 ? [] == [] => true
> "a" == "a" ? 1 == "1" => false
: and
: |
: !
"#);
    symbols.insert("or", eval_or, r#"
Logical `OR` of all inputs: evaluates to `true` if at least one `inputM` is `true`, `false` otherwise.
The shorthand for `?(input1, input2, ...)` is `input1 | input2 | ...`.
* Allows short-circuting: if any `inputM` is `true`, does not evaluate the rest.
= ?(input1, input2, ...)
> [true, true, false, false].?windows(2, ?) => [true, true, false]
: |
: and
: not
: xor
"#);
    symbols.insert("|", eval_or, r#"
Logical `OR` of all inputs: evaluates to `true` if at least one `inputM` is `true`, `false` otherwise.
* Allows short-circuting: if any `inputM` is `true`, does not evaluate the rest.
= input1 ? input2 ? ...
> 1+1 == 2 ? [] == [] => true
> "a" == "a" ? 1 == "1" => true
: or
: &
: !
: xor
"#);
    symbols.insert("not", eval_not, r#"
Logical `NOT`: evaluates to `true` if `input` is `false`, `false` if it is `true`.
The shorthand for `?(input)` is `!input1`.
= ?(input)
> ?(1 > 2) => true
> ?("a" == "a") => false
: !
: and
: or
: xor
"#);
    symbols.insert("!", eval_not, r#"
Logical `NOT`: evaluates to `true` if `input` is `false`, `false` if it is `true`.
= !input
> !(1 > 2) => true
> !("a" == "a") => false
: not
: &
: |
"#);
    symbols.insert("xor", eval_xor, r#"
Logical `XOR` ("exclusive or") of all inputs: evaluates to `true` if the number of `true` inputs is odd, `false` if it's even.
For two inputs, `?(input1, input2)` is `true` if one is `true` but not both, equivalent to `input1 <> input2`.
= ?(input1, input2, ...)
> ?(1+1 == 2, [] == []) => false
> ?("a" == "a", 1 == "1") => true
> [true, true, false, false].?windows(2, ?) => [false, true, false]
: and
: or
: not
"#);
}
