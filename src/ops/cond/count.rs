use crate::base::*;

fn eval_count(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    let (stm, cond) = match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(Expr::Eval(cond)), .. } => (stm, cond),
        _ => return Err(StreamError::new("expected: stream.countif{cond}", rnode))
    };
    let mut count = 0;
    try_with!(rnode, {
        for item in stm.iter() {
            check_stop!();
            match cond.clone().with_source(item?.into())?.eval(env)? {
                Item::Bool(value) => if value { count.inc() },
                other => return Err(format!("expected bool, found {:?}", other).into())
            }
        }
        Ok(Item::new_number(count))
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_count() {
        use super::*;
        use crate::parser::parse;

        test_eval!("range(5).countif{true}" => "5");
        test_eval!("range(5).countif{false}" => "0");
        test_eval!("range(-5,5).countif{#<0}" => "5");
        test_eval!("range(5).countif{#}" => err);
        test_eval!("range(5).countif([].len)" => err);
        test_eval!("[].countif{1}" => "0");
        test_eval!("[].countif(1)" => err);
        test_eval!("[].countif([].len)" => "0");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("countif", eval_count);
}
