use crate::base::*;
use std::collections::HashMap;

fn eval_primes(node: &Node, _env: &Env) -> SResult<Item> {
    node.check_no_source()?;
    node.check_no_args()?;
    Ok(Item::new_stream(Primes{head: node.head.clone()}))
}

pub struct Primes {
    head: Head,
}

impl Stream for Primes {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        Box::new(PrimesIter::new())
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Describe for Primes {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env).finish(prec)
    }
}

struct PrimesIter {
    composites: HashMap<UNumber, (UNumber, u32)>,
    current: UNumber,
    pre: <Vec<u32> as IntoIterator>::IntoIter,
    step: u32,
}

impl PrimesIter {
    fn new() -> PrimesIter {
        PrimesIter {
            composites: HashMap::new(),
            current: 2u32.into(),
            pre: vec![2, 3, 5].into_iter(),
            step: 4u32,
        }
    }

    fn get_next(&mut self) -> UNumber {
        loop {
            // no need for check_stop
            if let Some(next) = self.pre.next() {
                self.current = next.into();
            } else {
                self.step = 6 - self.step;
                self.current += self.step;
            }
            let cur = &self.current;
            if let Some((prime, mut step)) = self.composites.remove(cur) {
                step = 6 - step;
                let mut next = &self.current + &prime * step;
                while self.composites.contains_key(&next) {
                    step = 6 - step;
                    next += &prime * step;
                }
                self.composites.insert(next, (prime, step));
            } else {
                self.composites.insert(cur * cur, (cur.clone(), self.step));
                return cur.clone();
            }
        }
    }
}

impl Iterator for PrimesIter {
    type Item = UNumber;

    fn next(&mut self) -> Option<UNumber> {
        Some(self.get_next())
    }
}

impl SIterator for PrimesIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        Ok(Iterator::next(self).map(Item::new_number))
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }
}

fn eval_isprime(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let Some(Item::Number(x)) = &node.source else {
        return Err(StreamError::usage(&node.head));
    };
    let x = x.try_cast_within(UNumber::one()..)?;
    if x == UNumber::one() {
        return Ok(Item::Bool(false));
    }
    for prime in PrimesIter::new() {
        check_stop!();
        if x == prime {
            return Ok(Item::Bool(true));
        } else if (&x % prime).is_zero() {
            return Ok(Item::Bool(false));
        }
    }
    unreachable!()
}

fn eval_factor(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let Some(Item::Number(x)) = &node.source else {
        return Err(StreamError::usage(&node.head));
    };
    let x = x.try_cast_within(UNumber::one()..)?;
    Ok(Item::new_stream(Factor{x, head: node.head.clone()}))
}

struct Factor {
    x: UNumber,
    head: Head,
}

impl Stream for Factor {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        FactorIter::new(self).wrap()
    }

    fn len(&self) -> Length {
        Length::UnknownFinite
    }
}

impl Describe for Factor {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.x)
            .finish(prec)
    }
}

struct FactorIter {
    node: Rc<Factor>,
    x: UNumber,
    iter: PrimesIter,
    prime: UNumber,
}

impl FactorIter {
    fn new(node: Rc<Factor>) -> Self {
        let mut iter = PrimesIter::new();
        let prime = iter.get_next();
        Self{x: node.x.clone(), iter: PrimesIter::new(), prime, node}
    }
}

impl PreIterator for FactorIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.x == UNumber::one() {
            return Ok(None);
        }
        loop {
            check_stop!();
            let (div, rem) = (&self.x).div_rem(&self.prime);
            if rem.is_zero() {
                self.x = div;
                return Ok(Some(Item::new_number(&self.prime)));
            } else {
                self.prime = self.iter.get_next();
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::UnknownFinite
    }

    fn origin(&self) -> &Rc<Factor> {
        &self.node
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn test_seq() {
        use super::*;
        test_eval!("primes" : 20 => "[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, ...]");
        test_eval!("primes[10^5]" => "1299709");
        test_eval!("primes.windows(2).select{#[2]-#[1]==2}[10]" => "[107, 109]");
    }

    #[test]
    fn test_factor() {
        use super::*;
        test_eval!("100.factor" => "[2, 2, 5, 5]");
        test_eval!("10.factorial.factor.counts" : 12 => "[[2, 8], [3, 4], [5, 2], [7, 1]]");
    }

    #[test]
    fn test_isprime() {
        use super::*;
        test_eval!("0.isprime" => err);
        test_eval!("1.isprime" => "false");
        test_eval!("seq.select(isprime)" => "[2, 3, 5, 7, 11, ...]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("primes", eval_primes, r#"
An infinite stream of primes.
= ?
> ? => [2, 3, 5, 7, 11, ...]
> ?[100] => 541
: factor
: isprime
"#);
    symbols.insert("factor", eval_factor, r#"
Prime factors of `number` in increasing order, repeated in respective orders.
= number.?
> 20.? => [2, 2, 5]
: primes
: isprime
"#);
    symbols.insert("isprime", eval_isprime, r#"
Evaluates to `true` if `number` is a prime, `false` otherwise.
= number.?
> [23, 25, 27, 29]:? => [true, false, false, true]
> (100..120).?filter(?) => [101, 103, 107, 109, 113]
> 0.? => !out of range
: primes
: factor
"#);
}
