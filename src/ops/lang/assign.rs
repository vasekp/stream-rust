use crate::base::*;

fn eval_assign(_node: &Node, _env: &Env) -> Result<Item, StreamError> {
    Err(StreamError::new0("assignment not possible here, use == for comparisons"))
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("=", eval_assign, r#"
Assigns `value` to `name`.
This can only be used for local assignments using `?with` or for global variables. Use `==` for comparison.
= name = variable
> 10 = 11 => !can not assign to 10
> a = 10 => !can only assign to global symbols
> ?with(a = 10, a) => 10
: ==
: with
"#);
}
