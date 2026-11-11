use crate::base::*;

fn eval_assign(_node: &Node, _env: &Env) -> SResult<Item> {
    Err("assignment not possible here, use == for comparisons".into())
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("=", eval_assign, r#"
Assigns `value` to `name`.
This can only be used for local assignments using `?with` or for global variables. Use `==` for comparison.
= name = value
> 10 = 11 => !assignment not possible here, use == for comparisons
> a = 10 => !assignment not possible here, use == for comparisons ; only global variables ($name) can be assigned
> ?with(a = 10, a) => 10
: ==
: with
"#);
}
