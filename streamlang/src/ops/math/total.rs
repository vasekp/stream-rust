use crate::base::*;

fn eval_total(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    node.check_no_args()?;
    let mut iter = stm.iter();
    let Some(first) = iter.next()? else {
        return Ok(Item::new_number(0));
    };
    let mut total = first.into_num()?;
    for res in iter.transposed() {
        check_stop!();
        total += res?.as_num()?;
    }
    Ok(Item::new_number(total))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_total() {
        use super::*;
        test_eval!("(1..5).total" => "15");
        test_eval!("[1].total" => "1");
        test_eval!("['a', 1].total" => err);
        test_eval!("[].total" => "0");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("total", eval_total, r#"
Sums all numbers forming `stream`.
= stream.?
> (1..5).? => 15
: reduce
"#);
}
