use crate::base::*;
use num::traits::Euclid;

fn eval_pi(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_no_source()?;
    match &rnode.args {
        RArgs::Zero => Ok(Item::new_stream(Pi{head: rnode.head, radix: None})),
        RArgs::One(Item::Number(radix)) if *radix >= Number::from(2) => {
            if let Some(radix) = radix.to_u32() {
                Ok(Item::new_stream(Pi{head: rnode.head, radix: Some(radix)}))
            } else {
                Err(StreamError::new("radix much be between 2 and 65535", rnode))
            }
        }
        _ => Err(StreamError::new("expected: pi", rnode))
    }
}

#[derive(Clone)]
pub struct Pi {
    head: Head,
    radix: Option<u32>,
}

impl Stream for Pi {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PiIter::new(self))
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Describe for Pi {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, None::<&Item>, self.radix.iter().copied().map(Item::new_number), prec, env)
    }
}

struct PiIter<'node> {
    parent: &'node Pi,
    inner: PiIterInner,
    cached: Vec<u32>
}

impl PiIter<'_> {
    fn new(node: &Pi) -> PiIter<'_> {
        let radix = node.radix.unwrap_or(10);
        PiIter {
            parent: node,
            inner: PiIterInner::new(radix),
            cached: if radix <= 3 { vec![0] } else { vec![] }
        }
    }
}

impl Iterator for PiIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cached.len() >= 2 {
            return Some(Ok(Item::new_number(self.cached.remove(0))));
        }
        loop {
            check_stop!(iter);
            let (n, carry) = iter_try_expr!(self.inner.next().unwrap());
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
                return Some(Ok(Item::new_number(self.cached.remove(0))));
            }
        }
    }
}

impl SIterator for PiIter<'_> {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        match n.to_u32() {
            Some(n) => {
                self.inner.advance(n)?;
                Ok(None)
            },
            None => Err(StreamError::new("numerical overflow", Item::new_stream(self.parent.clone())))
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
            cdigits: vec![2u32.into()]
        }
    }

    fn advance(&mut self, n: u32) -> Result<(), StreamError> {
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

impl Iterator for PiIterInner {
    type Item = Result<(u32, bool), StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.power *= self.radix;
        let bits = usize::try_from(self.power.bits() - 1)
            .expect("usize should be enough");
        let prev_len = self.cdigits.len();
        self.cdigits.resize(bits, 0);
        let mut carry = UNumber::zero();
        for ix in (0..bits).rev() {
            check_stop!(iter);
            carry += if ix < prev_len { UNumber::from(self.cdigits[ix]) * self.radix } else { self.power.clone() };
            let num = if ix == 0 { UNumber::one() } else { UNumber::from(ix) };
            let den = if ix == 0 { UNumber::from(self.radix) } else { &num * 2u32 + 1u32 };
            let (quot, rem) = carry.div_rem_euclid(&den);
            self.cdigits[ix] = rem.try_into().expect("pi (next): remainder of div_euclid should fit into u32");
            carry = quot * num;
        }
        let (div, rem) = carry.div_rem_euclid(&UNumber::from(self.radix));
        Some(Ok((rem.to_u32().unwrap(), !div.is_zero())))
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
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("pi", eval_pi);
}
