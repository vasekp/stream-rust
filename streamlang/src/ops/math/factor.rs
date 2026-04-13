use crate::base::*;
use super::primes::PrimesIter;

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
        Self{x: node.x.clone(), iter, prime, node}
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

    fn origin(&self) -> &Rc<Factor> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_factor() {
        use super::*;
        test_eval!("100.factor" => "[2, 2, 5, 5]");
        test_eval!("10.factorial.factor.counts" : 12 => "[[2, 8], [3, 4], [5, 2], [7, 1]]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("factor", eval_factor, r#"
Prime factors of `number` in increasing order, repeated in respective orders.
= number.?
> 20.? => [2, 2, 5]
: primes
: isprime
"#);
}
