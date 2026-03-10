use crate::base::*;

fn eval_pi(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let radix = match &node.args[..] {
        [] => None,
        [Item::Number(radix)] => Some(radix.try_cast_within(2u32..)?),
        _ => return Err(StreamError::usage(&node.head))
    };
    Ok(Item::new_stream(Pi{head: node.head.clone(), radix}))
}

#[derive(Clone)]
pub struct Pi {
    head: Head,
    radix: Option<u32>,
}

impl Stream for Pi {
    fn iter(&self) -> Result<Box<dyn SIterator + '_>, StreamError> {
        Ok(Box::new(PiIter::new(self)))
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Describe for Pi {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .push_args(&self.radix)
            .finish(prec)
    }
}

struct PiIter {
    inner: PiIterInner,
    cached: Vec<u32>
}

impl PiIter {
    fn new(node: &Pi) -> PiIter {
        let radix = node.radix.unwrap_or(10);
        PiIter {
            inner: PiIterInner::new(radix),
            cached: if radix <= 3 { vec![0] } else { vec![] }
        }
    }
}

impl SIterator for PiIter {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        if self.cached.len() >= 2 {
            return Ok(Some(Item::new_number(self.cached.remove(0))));
        }
        loop {
            check_stop!();
            let (n, carry) = self.inner.next()?;
            if carry {
                for x in self.cached.iter_mut().rev() {
                    *x += 1;
                    if *x == self.inner.radix { *x = 0; continue; } else { break; }
                }
            }
            self.cached.push(n);
            if n == self.inner.radix - 1 {
                continue;
            }
            if self.cached.len() >= 2 {
                return Ok(Some(Item::new_number(self.cached.remove(0))));
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        match n.try_into() {
            Ok(n) => {
                self.inner.advance(n)?;
                Ok(None)
            },
            _ => Err("numerical overflow".into())
        }
    }
}

struct PiIterInner {
    radix: u32,
    power: UNumber,
    cdigits: Vec<u32>,
}

impl PiIterInner {
    fn new(radix: u32) -> PiIterInner {
        PiIterInner {
            radix,
            power: 2u32.into(),
            cdigits: vec![2u32]
        }
    }

    fn next(&mut self) -> Result<(u32, bool), StreamError> {
        self.power *= self.radix;
        let bits = self.power.bit_len() - 1;
        let prev_len = self.cdigits.len();
        self.cdigits.resize(bits, 0);
        let mut carry = UNumber::zero();
        for ix in (0..bits).rev() {
            check_stop!();
            carry += if ix < prev_len { UNumber::from(self.cdigits[ix]) * self.radix } else { self.power.clone() };
            let num = if ix == 0 { UNumber::one() } else { UNumber::from(ix) };
            let den = if ix == 0 { UNumber::from(self.radix) } else { &num * 2u32 + 1u32 };
            let (quot, rem) = carry.div_rem_euclid(&den);
            self.cdigits[ix] = rem.try_into().expect("pi (next): remainder of div_euclid should fit into u32");
            carry = quot * num;
        }
        let (div, rem) = carry.div_rem_euclid(&UNumber::from(self.radix));
        Ok((rem.try_into().unwrap(), !div.is_zero()))
    }

    fn advance(&mut self, n: usize) -> Result<(), StreamError> {
        let mul = UNumber::from(self.radix).pow(n);
        let len = self.cdigits.len();
        let mut carry = UNumber::zero();
        for ix in (0..len).rev() {
            check_stop!();
            carry += &mul * self.cdigits[ix];
            let num = if ix == 0 { UNumber::one() } else { UNumber::from(ix) };
            let den = if ix == 0 { UNumber::from(self.radix) } else { &num * 2u32 + 1u32 };
            let (quot, rem) = carry.div_rem_euclid(&den);
            self.cdigits[ix] = rem.try_into().expect("pi (advance): remainder of div_euclid should fit into u32");
            carry = quot * num;
        }
        self.power *= &mul;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_seq() {
        use super::*;
        test_eval!("pi" : 50 => "[3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4, 1, 9, 7, 1, 6, 9, 3, 9, 9, 3, 7, 5, 1, ...]");
        test_eval!("pi[32]" => "5");
        test_eval!("pi[33]" => "0");
        test_eval!("pi.skip(1000)" : 20 => "[9, 3, 8, 0, 9, 5, 2, 5, 7, 2, 0, 1, 0, 6, 5, 4, 8, 5, 8, 6, ...]");
        test_eval!("pi(2)" : 30 => "[1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, ...]");
        test_eval!("pi(3)" : 30 => "[1, 0, 0, 1, 0, 2, 1, 1, 0, 1, 2, 2, 2, 2, 0, 1, 0, 2, 1, 1, 0, 0, 2, 1, 1, 1, 1, 1, 0, 2, ...]");
        test_eval!("pi(4)" : 30 => "[3, 0, 2, 1, 0, 0, 3, 3, 3, 1, 2, 2, 2, 2, 0, 2, 0, 2, 0, 1, 1, 2, 2, 0, 3, 0, 0, 2, 0, 3, ...]");
        test_eval!("pi(16)" : 30 => "[3, 2, 4, 3, 15, 6, 10, 8, 8, 8, 5, 10, 3, 0, 8, 13, 3, 1, 3, 1, 9, 8, 10, 2, 14, 0, 3, 7, 0, 7, ...]");
        test_eval!("pi(10000)" => "[3, 1415, 9265, 3589, 7932, ...]");
        test_eval!("pi(65535)" => "[3, 9279, 17992, 54480, 37699, ...]");
        test_advance("pi.take(1000)");
        test_describe!("pi" => "pi");
        test_describe!("pi(3)" => "pi(3)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("pi", eval_pi, r#"
A stream of the digits of π (pi). If `base` is omitted, it defaults to 10 (decimal).
= ?
= ?(base)
> ? => [3, 1, 4, 1, 5, ...]
> ?(16) => [3, 2, 4, 3, 15, ...]
"#);
}
