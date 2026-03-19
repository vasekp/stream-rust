use crate::base::*;

fn eval_gcd(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let args = node.args.iter()
        .map(Item::to_num)
        .collect::<SResult<Vec<_>>>()?;
    let mut iter = args.iter();
    let Some(first) = iter.next() else {
        return Err(StreamError::usage(&node.head));
    };
    Ok(Item::Number(args.iter().fold(first.clone(), |a, b| gcd(&a, b))))
}

fn gcd(a: &Number, b: &Number) -> Number {
    let mut a = a.abs();
    let mut b = b.abs();
    if a < b { std::mem::swap(&mut a, &mut b); }
    while !b.is_zero() {
        (b, a) = (a % &b, b);
    }
    a
}

fn eval_egcd(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let (a, b) = match &node.args[..] {
        [Item::Number(a), Item::Number(b)] => (a, b),
        _ => return Err(StreamError::usage(&node.head))
    };
    let (g, (ca, cb)) = egcd(a, b);
    let list = vec![Item::Number(g), Item::Number(ca), Item::Number(cb)];
    Ok(Item::new_stream(List::from(list)))
}

fn egcd(x: &Number, y: &Number) -> (Number, (Number, Number)) {
    let (mut a, mut b, mut ax, mut ay, mut bx, mut by) = if x < y {
        (y.abs(), x.abs(), 0.into(), y.signum(), x.signum(), 0.into())
    } else {
        (x.abs(), y.abs(), x.signum(), 0.into(), 0.into(), y.signum())
    };
    while !b.is_zero() {
        let (div, rem) = a.div_rem(&b);
        (b, a) = (rem, b);
        (bx, ax) = (ax - &div * &bx, bx);
        (by, ay) = (ay - &div * &by, by);
    }
    (a, (ax, ay))
}

fn eval_lcm(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let args = node.args.iter()
        .map(Item::to_num)
        .collect::<SResult<Vec<_>>>()?;
    let mut iter = args.iter();
    let Some(first) = iter.next() else {
        return Err(StreamError::usage(&node.head));
    };
    Ok(Item::Number(args.iter().fold(first.clone(), |a, b| lcm(&a, b))))
}

fn lcm(a: &Number, b: &Number) -> Number {
    let g = gcd(a, b);
    if g.is_zero() {
        g
    } else {
        a / g * b
    }
}

fn eval_chinese(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let args = node.args.iter()
        .map(Item::to_num)
        .collect::<SResult<Vec<_>>>()?;
    if args.len() < 2 {
        return Err(StreamError::usage(&node.head));
    }
    for i in 0..args.len() {
        if args[i].is_zero() {
            return Err("can't be zero".into());
        }
        for j in 0..i {
            if gcd(&args[i], &args[j]) != 1.into() {
                return Err("arguments are not mutually coprime".into());
            }
        }
    }
    let lcm = args.iter().fold(Number::one(), |a, e| a * e);
    let ret = args.iter()
        .map(|arg| {
            let others = &lcm / arg;
            let (_, (_, q)) = egcd(arg, &others);
            Item::Number((q * others).rem_euclid(&lcm))
        })
        .collect::<Vec<_>>();
    Ok(Item::from(ret))
}


#[cfg(test)]
mod tests {
    #[test]
    fn test_gcd() {
        use super::*;
        test_eval!("gcd(100,85)" => "5");
        test_eval!("gcd(10,10)" => "10");
        test_eval!("gcd(0,5)" => "5");
        test_eval!("gcd(0,0)" => "0");
        test_eval!("gcd(10,-3)" => "1");
        test_eval!("gcd(10^60-1,7)" => "7");
        test_eval!("gcd(10^61-1,7)" => "1");
        test_eval!("gcd(20,15,4)" => "1");
        test_eval!("gcd(20,16,8)" => "4");
    }

    #[test]
    fn test_egcd() {
        use super::*;
        test_eval!("egcd(100,85)" => "[5, 6, -7]");
        test_eval!("egcd(144,89)" => "[1, 34, -55]");
        test_eval!("egcd(355,113)" => "[1, -7, 22]");
        test_eval!("egcd(0,0)" => "[0, 0, 0]");
        test_eval!("egcd(48,12)" => "[12, 0, 1]");
        test_eval!("egcd(25,0)" => "[25, 1, 0]");
        test_eval!("egcd(144,-89)" => "[1, 34, 55]");
        test_eval!("egcd(-144,89)" => "[1, -34, -55]");
        test_eval!("egcd(-144,-89)" => "[1, -34, 55]");
    }

    #[test]
    fn test_lcm() {
        use super::*;
        test_eval!("lcm(100,85)" => "1700");
        test_eval!("lcm(10,10)" => "10");
        test_eval!("lcm(0,5)" => "0");
        test_eval!("lcm(0,0)" => "0");
        test_eval!("lcm(10,-3)" => "-30");
        test_eval!("lcm(20,15,4)" => "60");
        test_eval!("lcm(20,16,8)" => "80");
    }

    #[test]
    fn test_chinese() {
        use super::*;
        test_eval!("crt(2,3,5)" => "[15, 10, 6]");
        test_eval!("crt(100,9,7)" => "[1701, 2800, 1800]");
        test_eval!("crt(5,6)" => "[6, 25]");
        test_eval!("crt(2,4)" => err);
        test_eval!("crt(0,1)" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("gcd", eval_gcd, r#"
Greatest common divisor of all inputs.
= ?(number, number, ...)
> ?(100, 85) => 5
> ?(50, 20, 15) => 5
: egcd
: lcm
: crt
"#);
    symbols.insert("egcd", eval_egcd, r#"
Extended greatest common divisor.
`gcd(a, b)` evaluates to a list `[g, x, y]` such that `g == gcd(a, b) == x*a + y*b`.
= ?(number, number)
> ?(100, 85) => [5, 6, -7] ; 6*100 - 7*85 == 1
: gcd
: crt
"#);
    symbols.insert("lcm", eval_lcm, r#"
Least common multiple of all inputs.
= ?(number, number, ...)
> ?(100, 85) => 1700
> ?(50, 20, 15) => 300
: gcd
"#);
    symbols.insert("crt", eval_chinese, r#"
Finds the base multipliers for Chinese Remainder Theorem,
i.e., numbers `[mult1, ..., multN]` within 0 and the product of all `number`s whose remainders are `[1, 0, 0, ...`, `[0, 1, 0, ...` etc.
= ?(number, number, ...)
> ?(3, 7, 10) => [70, 120, 21]
> 70 % [3, 7, 10] => [1, 0, 0] ; check
> 1 * 70 + 2 * 120 + 3 * 21 => 373 ; use this to find `x` for any given tuple of reminders
> 373 % [3, 7, 10] => [1, 2, 3]
: gcd
: egcd
"#);
}
