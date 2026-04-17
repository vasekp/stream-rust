use crate::base::*;
use ibig::modular::ModuloRing;

fn eval_mod(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let (m, base) = match &node.args[..] {
        [Item::Number(m)] => (m, &Number::zero()),
        [Item::Number(m), Item::Number(base)] => (m, base),
        _ => return Err(StreamError::usage(&node.head)),
    };
    let num = node.source_checked()?.as_num()?;
    let ring = ModuloRing::new(&m.try_cast_within(UNumber::one()..)?);
    let rem = Number::from(ring.from(num - base).residue()) + base;
    Ok(Item::Number(rem))
}

fn eval_modinv(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let m = node.only_arg_checked()?.as_num()?;
    let num = node.source_checked()?.as_num()?;
    let ring = ModuloRing::new(&m.try_cast_within(UNumber::one()..)?);
    if let Some(inv) = ring.from(num).inverse() {
        Ok(Item::new_number(inv.residue()))
    } else {
        Err("does not have an inverse".into())
    }
}

fn eval_modpow(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let [Item::Number(m), Item::Number(pow)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    let num = node.source_checked()?.as_num()?;
    let ring = ModuloRing::new(&m.try_cast_within(UNumber::one()..)?);
    let upow = pow.unsigned_abs();
    if pow.is_negative() {
        if let Some(inv) = ring.from(num).inverse() {
            Ok(Item::new_number(inv.pow(&upow).residue()))
        } else {
            Err("does not have an inverse".into())
        }
    } else {
        Ok(Item::new_number(ring.from(num).pow(&upow).residue()))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_mod() {
        use super::*;
        test_eval!("(1..5):mod(3)" => "[1, 2, 0, 1, 2]");
        test_eval!("(1..5):mod(3, 1)" => "[1, 2, 3, 1, 2]");
        test_eval!("(-2..2):mod(3)" => "[1, 2, 0, 1, 2]");
        test_eval!("(-2..2):mod(3, 1)" => "[1, 2, 3, 1, 2]");
        test_eval!("5.mod(1)" => "0");
        test_eval!("5.mod(0)" => err);
    }

    #[test]
    fn test_modinv() {
        use super::*;
        test_eval!("(1..5):modinv(5)" => "[1, 3, 2, 4, <!>");
        test_eval!("3.modinv(10)" => "7");
        test_eval!("1.modinv(7)" => "1");
        test_eval!("0.modinv(7)" => err);
    }

    #[test]
    fn test_modpow() {
        use super::*;
        test_eval!("(0..9):{2.modpow(10,#)}" : 10 => "[1, 2, 4, 8, 6, 2, 4, 8, 6, 2]");
        test_eval!("(-3..5):{3.modpow(10,#)}" : 10 => "[3, 9, 7, 1, 3, 9, 7, 1, 3]");
        test_eval!("2.modpow(10,-3)" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("mod", eval_mod, r#"
Computes the residue of `number` modulo `modulus`.
If `base` is given, the result satisfies `base ≤ result < base + modulus`.
Usual choices are `base = 0` (the default) and `base = 1`.
= number.?(modulus)
= number.?(modulus, base)
> 123.?(10) => 3
> (-123).?(10) => 7 ; Euclidean type of modulo
: %
: modinv
: modpow
"#);
    symbols.insert("modinv", eval_modinv, r#"
Modular inverse, i.e., the unique `x` such that `(number * x).?mod(modulus) == 1`.
= number.?(modulus)
> 3.?(10) => 7
> 5.?(10) => !does not have an inverse
: modpow
: mod
"#);
    symbols.insert("modpow", eval_modpow, r#"
Modular power, i.e., `(number ^ power).?mod(modulus)` without the overhead of calculating a huge number and then reducing again.
Negative powers are also accepted, if `number` has a modular inverse for `modulus`.
= number.?(modulus, power)
> 3.?(10, 100) => 1 ; 3^100 == ...22001
> 7.?(10, -2) => 9
: modinv
: mod
: ^
"#);
}
