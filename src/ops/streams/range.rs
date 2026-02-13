use crate::base::*;

#[derive(Clone)]
pub struct Range {
    head: Head,
    from: Option<Number>,
    to: Number,
    step: Option<Number>,
    rtype: RangeType,
    env: Env,
}

#[derive(Clone, Copy)]
enum RangeType {
    Numeric,
    Character(CharCase)
}

impl Range {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let mut rnode = node.eval_all(env)?.resolve_no_source()?;
        let (from, to, step, rtype) = match rnode {
            RNodeNS { args: RArgs::One(Item::Number(to)), .. }
                => (None, to, None, RangeType::Numeric),
            RNodeNS { args: RArgs::Two(Item::Number(from), Item::Number(to)), .. }
                => (Some(from), to, None, RangeType::Numeric),
            RNodeNS { args: RArgs::Three(Item::Number(from), Item::Number(to), Item::Number(step)), .. }
                => (Some(from), to, Some(step), RangeType::Numeric),
            RNodeNS { args: RArgs::Two(Item::Char(ref from), Item::Char(ref to)), .. }
                => {
                    let abc = env.alphabet();
                    let case = from.case();
                    let from_ix = try_with!(rnode, abc.ord(from)?);
                    let to_ix = try_with!(rnode, abc.ord(to)?);
                    (Some(from_ix.into()), to_ix.into(), None, RangeType::Character(case))
                },
            RNodeNS { args: RArgs::Three(Item::Char(ref from), Item::Char(ref to), Item::Number(ref mut step)), .. }
                => {
                    let abc = env.alphabet();
                    let case = from.case();
                    let from_ix = try_with!(rnode, abc.ord(from)?);
                    let to_ix = try_with!(rnode, abc.ord(to)?);
                    (Some(from_ix.into()), to_ix.into(), Some(std::mem::take(step)), RangeType::Character(case))
                },
            _ => return Err(StreamError::new("expected one of: range(num), range(num, num), range(num, num, num), range(char, char), range(char, char, num)", rnode))
        };
        if Range::empty_helper(from.as_ref(), &to, step.as_ref()) {
            Ok(Item::empty_stream())
        } else {
            Ok(Item::new_stream(Range{head: rnode.head, from, to, step, rtype, env: env.clone()}))
        }
    }

    fn empty_helper(from: Option<&Number>, to: &Number, step: Option<&Number>) -> bool {
        (step.is_some_and(Number::is_negative) && to > from.unwrap_or(&Number::one()))
            || (step.is_none_or(Number::is_positive) && to < from.unwrap_or(&Number::one()))
    }

    fn len_helper(from: Option<&Number>, to: &Number, step: Option<&Number>) -> Option<UNumber> {
        if Self::empty_helper(from, to, step) {
            return Some(UNumber::zero());
        }
        match step.map(|step| (step, step.to_i32())) {
            None | Some((_, Some(1))) => Some(UNumber::try_from(match from {
                    Some(from) => to - from + 1,
                    None => to.to_owned() })
                .expect("expected to > from when step > 0")),
            Some((_, Some(-1))) => Some(UNumber::try_from(from.expect("step should not be Some if from is not") - to + 1)
                .expect("expected to < from when step < 0")),
            Some((_, Some(0))) => None,
            Some((step, _)) => Some(UNumber::try_from((to - from.expect("step should not be Some if from is not")) / step + 1)
                .expect("to-from / step sign mismatch")),
        }
    }
}

impl Stream for Range {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(RangeIter{
            parent: self,
            value: match &self.from {
                Some(from) => from.clone(),
                None => Number::one()
            }
        })
    }

    fn len(&self) -> Length {
        match Range::len_helper(self.from.as_ref(), &self.to, self.step.as_ref()) {
            Some(num) => Length::Exact(num),
            None => Length::Infinite
        }
    }
}

impl Describe for Range {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        match self.rtype {
            RangeType::Numeric =>
                DescribeBuilder::new(&self.head, env)
                    .push_args(&self.from)
                    .push_arg(&self.to)
                    .push_args(&self.step)
                    .finish(prec),
            RangeType::Character(case) => {
                let abc = &self.env.alpha;
                DescribeBuilder::new_with_env(&self.head, env, &self.env)
                    .push_arg(&abc.chr(self.from.as_ref().expect("char range should have from"), case))
                    .push_arg(&abc.chr(&self.to, case))
                    .push_args(&self.step)
                    .finish(prec)
            }
        }
    }
}

struct RangeIter<'node> {
    parent: &'node Range,
    value: Number
}

impl Iterator for RangeIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.parent.step.as_ref().is_some_and(Number::is_zero)
            || (self.parent.step.as_ref().is_none_or(Number::is_positive) && self.value <= self.parent.to)
            || (self.parent.step.as_ref().is_some_and(Number::is_negative) && self.value >= self.parent.to) {
                let ret = match self.parent.rtype {
                    RangeType::Numeric => Item::new_number(self.value.clone()),
                    RangeType::Character(case) => Item::new_char(self.parent.env.alpha.chr(&self.value, case))
                };
                match &self.parent.step {
                    Some(step) => self.value += step,
                    None => self.value.inc()
                }
                Some(Ok(ret))
        } else {
            None
        }
    }
}

impl SIterator for RangeIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if Range::empty_helper(Some(&self.value), &self.parent.to, self.parent.step.as_ref()) {
            return Ok(Some(n))
        };
        let Some(max) = Range::len_helper(Some(&self.value), &self.parent.to, self.parent.step.as_ref())
            else { return Ok(None); };
        if n <= max {
            self.value += match &self.parent.step {
                Some(step) => step * Number::from(n),
                None => Number::from(n)
            };
            Ok(None)
        } else {
            Ok(Some(n - &max))
        }
    }

    fn len_remain(&self) -> Length {
        match Range::len_helper(Some(&self.value), &self.parent.to, self.parent.step.as_ref()) {
            Some(num) => Length::Exact(num),
            None => Length::Infinite
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range() {
        test_eval!("range(3)" => "[1, 2, 3]");
        test_eval!("range(0)" => "[]");
        test_eval!("range(3, 3)" => "[3]");
        test_eval!("range(3, 5)" => "[3, 4, 5]");
        test_eval!("range(5, 3)" => "[]");
        test_eval!("range(1, 10, 4)" => "[1, 5, 9]");
        test_eval!("range(1, 10, 10)" => "[1]");
        test_eval!("range(1, 10, 0)" => "[1, 1, 1, 1, 1, ...]");
        test_eval!("range(1, 1, 0)" => "[1, 1, 1, 1, 1, ...]");
        test_eval!("range(1, 10, -1)" => "[]");
        test_eval!("range(1, -10, -3)" => "[1, -2, -5, -8]");

        test_eval!("range('a', 'C')" => "['a', 'b', 'c']");
        test_eval!("range('D', 'f')" => "['D', 'E', 'F']");
        test_eval!("range('a', 'h', 3)" => "['a', 'd', 'g']");
        test_eval!("range('a', 'z', -1)" => "[]");
        test_eval!("range('a', 'z', 0)" => "['a', 'a', 'a', 'a', 'a', ...]");
        test_eval!("range('a')" => err);
        test_eval!("range('a', 1)" => err);
        test_eval!("range(1, 'a')" => err);
        test_eval!("range('a', 'h', 'c')" => err);

        test_eval!("1..3" => "[1, 2, 3]");
        test_eval!("3..3" => "[3]");
        test_eval!("3..1" => "[]");
        test_eval!("-1..3" => "[-1, -2, -3]");
        test_eval!("1..2..3" => err);
        test_eval!("'a'..'z'" => "['a', 'b', 'c', 'd', 'e', ...]");
        test_eval!("'A'..'z'" => "['A', 'B', 'C', 'D', 'E', ...]");
        test_eval!("'a'..'á'" => err);

        test_eval!("range(10^9, 10^10, 2).len" => "4500000001");
        test_len!("range(0)" => 0);
        test_len!("range(-1)" => 0);
        test_len!("range(3)" => 3);
        test_len!("range(0,3)" => 4);
        test_len!("range(3,0)" => 0);
        test_len!("range(3,3)" => 1);
        test_len!("range(0,3,2)" => 2);
        test_len!("range(0,3,3)" => 2);
        test_len!("range(0,3,4)" => 1);
        test_len!("range(0,4,2)" => 3);
        test_len!("range(0,3,-2)" => 0);
        test_len!("range(0,-3,-2)" => 2);
        test_len!("range(0,-3,-3)" => 2);
        test_len!("range(0,-3,-4)" => 1);
        test_len!("'a'..'z'" => 26);
        test_advance("range(0)");
        test_advance("range(10^10)");
        test_advance("range(-10^10)");
        test_advance("range(0,3)");
        test_advance("range(3,0)");
        test_advance("range(0,3,2)");
        test_advance("range(0,3,3)");
        test_advance("range(0,3,4)");
        test_advance("range(0,4,2)");
        test_advance("range(0,3,0)");
        test_advance("range(0,3,-2)");
        test_advance("range(0,-3,-2)");

        test_describe!("range(5)" => "range(5)");
        test_describe!("range(0)" => "[]");
        test_describe!("range(1,5)" => "range(1, 5)");
        test_describe!("range(1,5,2)" => "range(1, 5, 2)");
        test_describe!("range('a','Z')" => "range('a', 'z')");
        test_describe!("range('A','z',2)" => "range('A', 'Z', 2)");
        test_describe!("alpha(\"abz\", range('a','Z'))" => "alpha(['a', 'b', 'z'], range('a', 'z'))");
        test_describe!("1..5" => "1..5");
        test_describe!("(-1)..5" => "(-1)..5");
        test_describe!("-(1..5)" => "-1..5");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("range", Range::eval);
    keywords.insert("..", Range::eval);
}
