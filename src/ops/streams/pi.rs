use crate::base::*;

fn eval_pi(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_no_source()?;
    match rnode.args {
        RArgs::Zero => Ok(Item::new_stream(Pi{head: rnode.head})),
        _ => return Err(StreamError::new("expected: pi", rnode))
    }
}

#[derive(Clone)]
pub struct Pi {
    head: Head,
}

impl Stream for Pi {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PiIter::new())
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Describe for Pi {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, None::<&Item>, None::<&Item>, prec, env)
    }
}

struct PiIter {
    inner: PiIterInner,
    cached: Vec<u32>
}

impl PiIter {
    fn new() -> PiIter {
        PiIter {
            inner: PiIterInner::new(),
            cached: vec![]
        }
    }
}

impl Iterator for PiIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cached.len() >= 2 {
            return Some(Ok(Item::new_number(self.cached.remove(0))));
        }
        loop {
            check_stop!(iter);
            let (n, carry) = self.inner.next().unwrap();
            if carry {
                for x in self.cached.iter_mut().rev() {
                    *x = *x + 1;
                    if *x == 10 { *x = 0; continue; } else { break; }
                }
            }
            self.cached.push(n);
            if n == 9 {
                continue;
            }
            if self.cached.len() >= 2 {
                return Some(Ok(Item::new_number(self.cached.remove(0))));
            }
        }
    }
}

impl SIterator for PiIter {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        match n.to_u32() {
            Some(n) => {
                self.inner.skip_n(n);
                Ok(None)
            },
            None => Err(StreamError::new("numerical overflow", Expr::new_node("pi", vec![]))) // TODO
        }
    }
}

struct PiIterInner {
    power: UNumber,
    cdigits: Vec<UNumber>,
}

impl PiIterInner {
    fn new() -> PiIterInner {
        PiIterInner {
            power: 2u32.into(),
            cdigits: vec![2u32.into()]
        }
    }

    fn skip_n(&mut self, n: u32) {
        let mul = UNumber::from(10u32).pow(n);
        for x in &mut self.cdigits {
            *x *= &mul;
        }
        self.power *= UNumber::from(10u32).pow(n);
    }
}

impl Iterator for PiIterInner {
    type Item = (u32, bool);

    fn next(&mut self) -> Option<Self::Item> {
        use num::traits::Euclid;
        self.power *= 10u32;
        for x in &mut self.cdigits {
            *x *= 10u32;
        }
        let bits = usize::try_from(self.power.bits() - 1)
            .expect("usize should be enough");
        self.cdigits.resize(bits, self.power.clone());
        let mut carry = UNumber::zero();
        for (ix, x) in self.cdigits.iter_mut().enumerate().rev() {
            let num = if ix == 0 { UNumber::one() } else { UNumber::from(ix) };
            let den = if ix == 0 { UNumber::from(10u32) } else { &num * 2u32 + 1u32 };
            let (quot, rem) = (&*x + &carry).div_rem_euclid(&den);
            *x = rem;
            carry = quot * num;
        }
        let (div, rem) = carry.div_rem_euclid(&UNumber::from(10u32));
        Some((rem.to_u32().unwrap(), !div.is_zero()))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_seq() {
        use super::*;
        use crate::parser::parse;
        test_eval!("pi" : 50 => "[3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4, 1, 9, 7, 1, 6, 9, 3, 9, 9, 3, 7, 5, 1, ...]");
        test_eval!("pi[32]" => "5");
        test_eval!("pi[33]" => "0");
        test_eval!("pi.skip(1000)" : 20 => "[9, 3, 8, 0, 9, 5, 2, 5, 7, 2, 0, 1, 0, 6, 5, 4, 8, 5, 8, 6, ...]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("pi", eval_pi);
}
