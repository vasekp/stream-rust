use crate::base::*;

fn eval_greedy(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let num = node.source_checked()?.as_num()?;
    let stm = node.only_arg_checked()?.as_stream()?;
    let num = num.try_unsign()?;
    Ok(Item::new_stream(Greedy{num, stm: Rc::clone(stm), head: node.head.clone()}))
}

struct Greedy {
    num: UNumber,
    stm: Rc<dyn Stream>,
    head: Head,
}

impl Stream for Greedy {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        GreedyIter{x: self.num.clone(), iter: self.stm.iter(), node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.stm.len()
    }
}

impl Describe for Greedy {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.num)
            .push_arg(&self.stm)
            .finish(prec)
    }
}

struct GreedyIter {
    node: Rc<Greedy>,
    x: UNumber,
    iter: Box<dyn SIterator>,
}

impl PreIterator for GreedyIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let denom = iter_try!(self.iter.next())
            .as_num()?
            .try_cast_within(UNumber::one()..)?;
        let (quot, rem) = std::mem::take(&mut self.x).div_rem(&denom);
        self.x = rem;
        Ok(Some(Item::new_number(quot)))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut n = n.clone();
        while !self.x.is_zero() && !n.is_zero() {
            check_stop!();
            if self.next()?.is_none() {
                return Ok(Some(n));
            }
            n -= 1;
        }
        if n.is_zero() {
            Ok(None)
        } else {
            self.iter.advance(&n)
        }
    }

    fn origin(&self) -> &Rc<Greedy> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_greedy() {
        use super::*;
        test_eval!("96.greedy([50,20,10,5,2,1])" : 6 => "[1, 2, 0, 1, 0, 1]");
        test_eval!("96.greedy([50,20,1,5,2,1])" : 6 => "[1, 2, 6, 0, 0, 0]");
        test_eval!("96.greedy([50,20,10,0,2,1])" : 6 => "[1, 2, 0, <!>");
        test_eval!("5.greedy([3,2])" => "[1, 1]");
        test_eval!("5.greedy([2,3])" => "[2, 0]");
        test_eval!("5.greedy(1.repeat)" => "[5, 0, 0, 0, 0, ...]");
        test_advance("5.greedy(1.repeat(10^10))");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("greedy", eval_greedy, r#"
Decomposes a nonnegative `number` into multiples of items of `stream` using the greedy algorithm.
The stream must contain of a sequence of positive numbers, usually in decreasing order.
* The last element should be 1 such that it can consume any remainder.
= number.?(stream)
> 39.?([50, 20, 10, 5, 2, 1]) : 6 => [0, 1, 1, 1, 2, 0] ; coin denominations
> 532.?([100, 10, 1]) => [5, 3, 2] ; ?numdig may be a better fit here
> 10.?([3, 2]) => [3, 0] ; the remainder of 10 - 3*3 = 1 can't be covered by multiples of 2: silently 
dropped
: numdig
: dot
"#);
}
