use crate::base::*;

fn eval_factorial(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve();
    match &node {
        RNode::Source(RNodeS { source: Item::Number(x), args: RArgs::Zero, .. })
        | RNode::NoSource(RNodeNS { args: RArgs::One(Item::Number(x)), .. })
        => {
            if x.is_negative() {
                return Err(StreamError::new("input can't be negative", node));
            }
            let Ok(x) = x.try_into() else {
                return Err(StreamError::new("input too large", node));
            };
            Ok(Item::new_number(crate::utils::factorial(x)))
        },
        _ => Err(StreamError::new("expected: number.factorial or factorial(number)", node))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_factorial() {
        use super::*;
        test_eval!("(0..5):factorial" : 6 => "[1, 1, 2, 6, 24, 120]");
        test_eval!("factorial(10)" => "3628800");
        test_eval!("10.factorial(10)" => err);
        test_eval!("factorial(-1)" => err);
        test_eval!("factorial(10^10)" => err);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("factorial", eval_factorial);
}
