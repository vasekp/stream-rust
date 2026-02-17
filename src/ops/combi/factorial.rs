use crate::base::*;
use crate::utils::factorial;

fn eval_factorial(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve();
    match &node {
        RNode::Source(RNodeS { source: Item::Number(x), args: RArgs::Zero, .. })
        | RNode::NoSource(RNodeNS { args: RArgs::One(Item::Number(x)), .. })
        => {
            if x.is_negative() {
                return Err(StreamError::new("input can't be negative", node));
            }
            let Some(x) = x.to_u32() else {
                return Err(StreamError::new("input too large", node));
            };
            Ok(Item::new_number(factorial(x)))
        },
        _ => Err(StreamError::new("expected: number.factorial or factorial(number)", node))
    }
}

fn eval_binom(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?.resolve();
    match &node {
        RNode::NoSource(RNodeNS { args: RArgs::Two(Item::Number(n), Item::Number(k)), .. })
        => {
            if n.is_negative() | k.is_negative() {
                return Err(StreamError::new("input can't be negative", node));
            }
            let (Some(n), Some(k)) = (n.to_u32(), k.to_u32()) else {
                return Err(StreamError::new("input too large", node));
            };
            if k > n {
                return Err(StreamError::new("out of range", node));
            }
            Ok(Item::new_number(factorial(n) / (factorial(k) * factorial(n - k))))
        },
        _ => Err(StreamError::new("expected: binom(n, k)", node))
    }
}

fn eval_comb(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    let mut total = 0;
    let mut denom = UNumber::one();
    for arg in &node.args {
        let Item::Number(k) = arg else {
            return Err(StreamError::new("expected: comb(number, number, ...)", node));
        };
        if k.is_negative() {
            return Err(StreamError::new("input can't be negative", node));
        }
        let Some(k) = k.to_u32() else {
            return Err(StreamError::new("input too large", node));
        };
        total = try_with!(node, total.checked_add(&k)
            .ok_or(BaseError::from("input too large"))?);
        denom *= factorial(k);
    }
    Ok(Item::new_number(factorial(total) / denom))
}
fn eval_rcomb(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    let mut total = 0;
    let mut denom = UNumber::one();
    let mut vec = Vec::new();
    for arg in &node.args {
        let Item::Number(k) = arg else {
            return Err(StreamError::new("expected: comb(number, number, ...)", node));
        };
        if k.is_negative() {
            return Err(StreamError::new("input can't be negative", node));
        }
        let Some(k) = k.to_u32() else {
            return Err(StreamError::new("input too large", node));
        };
        if k == 0 { continue; }
        if vec.contains(&k) {
            denom *= 1 + vec.iter().filter(|x| *x == &k).count();
        }
        vec.push(k);
        total = try_with!(node, total.checked_add(&k)
            .ok_or(BaseError::from("input too large"))?);
        denom *= factorial(k);
    }
    Ok(Item::new_number(factorial(total) / denom))
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

    #[test]
    fn test_binom() {
        use super::*;
        test_eval!("seq(0):{binom(5,#)}" : 7 => "[1, 5, 10, 10, 5, 1, <!>");
        test_eval!("binom(0,0)" => "1");
        test_eval!("binom(-1,0)" => err);
        test_eval!("binom(0,-1)" => err);
        test_eval!("binom(0,1)" => err);
        test_eval!("seq(0):{binom(2*#,#)}" : 10 => "[1, 2, 6, 20, 70, 252, 924, 3432, 12870, 48620, ...]");
        test_eval!("binom(100,30)" => "29372339821610944823963760");
    }

    #[test]
    fn test_comb() {
        use super::*;
        test_eval!("comb(3,5) == binom(8,3)" => "true");
        test_eval!("comb(3,5,0) == binom(8,3)" => "true");
        test_eval!("comb()" => "1");
        test_eval!("comb(0)" => "1");
        test_eval!("comb(0,0)" => "1");
        test_eval!("comb(1,1,1,1) == factorial(4)" => "true");
        test_eval!("comb(3,3,3) == factorial(9)/factorial(3)^3" => "true");
        test_eval!("comb(3,4,5) == factorial(12)/factorial(3)/factorial(4)/factorial(5)" => "true");
    }

    #[test]
    fn test_rcomb() {
        use super::*;
        test_eval!("rcomb(3,5) == comb(3,5)" => "true");
        test_eval!("rcomb(3,5,0) == comb(3,5,0)" => "true");
        test_eval!("rcomb()" => "1");
        test_eval!("rcomb(0)" => "1");
        test_eval!("rcomb(0,0)" => "1");
        test_eval!("rcomb(1,1,1,1)" => "1");
        test_eval!("rcomb(3,3,3) == comb(3,3,3)/factorial(3)" => "true");
        test_eval!("rcomb(3,4,3) == comb(3,3,4)/factorial(2)" => "true");
        test_eval!("rcomb(3,4,5) == comb(3,4,5)" => "true");
        test_eval!("rcomb(3,4,3,4,3) == comb(3,3,3,4,4)/2/6" => "true");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_with_docs("factorial", eval_factorial, r#"
Factorial of `number`.
= number.?
= ?(number)
> ?seq:? => [1, 2, 6, 24, 120, ...]
: binom
: comb
"#);
    symbols.insert_with_docs("binom", eval_binom, r#"
Binomial coefficient, i.e., `factorial(n) / (factorial(k) * factorial(n-k))` if `0 <= k <= n`.
= ?(n, k)
> [0, 1, 2, 3, 4]:{?(4, #)} => [1, 4, 6, 4, 1]
: factorial
: comb
"#);
    symbols.insert_with_docs("comb", eval_comb, r#"
Multinomial coefficient, i.e., `factorial(k1 + k2 + ...) / (factorial(k1) * factorial(k2) * ...)`.
= ?(k1, ..., kM)
> ?(3, 3, 4) => 4200
> ?(3, 3, 4) == factorial(3+3+4) / (factorial(3)^2 * factorial(4)) => true
: factorial
: rcomb
"#);
    symbols.insert_with_docs("rcomb", eval_rcomb, r#"
The number of partitions of a set of type `(k1, ..., kM)`, i.e., `?comb(k1, ..., kM)` divided by the number of permutations keeping the argument tuple unchanged.
= ?(k1, ..., kM)
> ?(3, 3, 4) => 2100
> ?(3, 3, 4) == comb(3, 3, 4) / (factorial(2) * factorial(1)) => true ; 3 repeats twice, 4 once
: comb
: factorial
"#);
}
