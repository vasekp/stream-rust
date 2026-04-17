use crate::base::*;

fn eval_sqrt(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let num = node.source_checked()?.as_num()?.try_unsign()?;
    Ok(Item::new_number(sqrt_impl(&num)))
}

fn sqrt_impl(n: &UNumber) -> UNumber {
    if n.is_zero() { return UNumber::zero(); }
    let mut x = UNumber::one() << (n.bit_len() / 2 + 1);
    loop {
        let next_x = (&x + n / &x) >> 1;
        if next_x >= x { break; }
        x = next_x;
    }
    x
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_sqrt() {
        use super::*;
        test_eval!("seq(0):sqrt" : 10 => "[0, 1, 1, 1, 2, 2, 2, 2, 2, 3, ...]");
        test_eval!("(10^100).sqrt" => "100000000000000000000000000000000000000000000000000");
        test_eval!("(-1).sqrt" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("sqrt", eval_sqrt, r#"
Square root of `number`, rounded below to nearest integer.
= number.?
> 10.? => 3
"#);
}
