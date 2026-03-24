use crate::base::*;
use super::primes::PrimesIter;

fn eval_divisors(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let Some(Item::Number(x)) = &node.source else {
        return Err(StreamError::usage(&node.head));
    };
    let x = x.try_cast_within(UNumber::one()..)?;
    Ok(Item::new_stream(Divisors{x, head: node.head.clone()}))
}

struct Divisors {
    x: UNumber,
    head: Head,
}

impl Stream for Divisors {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        DivisorsIter::new(self).wrap()
    }

    fn len(&self) -> Length {
        Length::UnknownFinite
    }
}

impl Describe for Divisors {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.x)
            .finish(prec)
    }
}

struct DivisorsIter {
    node: Rc<Divisors>,
    x: UNumber,
    iter: PrimesIter,
    prime: UNumber,
    factors: Vec<(UNumber, usize, usize)>,
}

impl DivisorsIter {
    fn new(node: Rc<Divisors>) -> Self {
        Self{
            x: node.x.clone(),
            iter: PrimesIter::new(),
            prime: UNumber::zero(),
            factors: Vec::new(),
            node
        }
    }
}

impl DivisorsIter {
    fn advance(&mut self) -> Option<UNumber> {
        for ix in 0..self.factors.len() {
            let (_, max, cur) = &mut self.factors[ix];
            if cur < max {
                *cur += 1;
                for j in 0..ix {
                    self.factors[j].2 = 0;
                }
                return Some(self.current());
            }
        }
        None
    }

    fn current(&self) -> UNumber {
        let mut x = UNumber::one();
        for (prime, _, power) in &self.factors {
            x *= prime.pow(*power);
        }
        x
    }
}

impl PreIterator for DivisorsIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if let Some(num) = self.advance() {
            return Ok(Some(Item::new_number(num)));
        }
        if self.prime.is_zero() {
            self.prime = self.iter.get_next();
            return Ok(Some(Item::new_number(1)));
        }
        if self.x == UNumber::one() {
            return Ok(None);
        }
        loop {
            check_stop!();
            let (div, rem) = (&self.x).div_rem(&self.prime);
            if rem.is_zero() {
                self.x = div;
                match self.factors.last_mut() {
                    Some((p, max, cur)) if *p == self.prime => *max += 1,
                    _ => self.factors.push((self.prime.clone(), 1, 0)),
                }
                return Ok(self.advance().map(Item::new_number));
            } else {
                self.prime = self.iter.get_next();
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::UnknownFinite
    }

    fn origin(&self) -> &Rc<Divisors> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_divisors() {
        use super::*;
        test_eval!("100.divisors" : 10 => "[1, 2, 4, 5, 10, 20, 25, 50, 100]");
        test_eval!("10.factorial.divisors.len" => "270");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("divisors", eval_divisors, r#"
All integer divisors of `number` in unspecified order.
= number.?
> 20.? : 10 => [1, 2, 4, 5, 10, 20]
: factor
"#);
}
