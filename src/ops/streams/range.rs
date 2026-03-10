use crate::base::*;

fn eval_range(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let (from, to, step, rtype) = match &node.args[..] {
        [Item::Number(to)] => (None, to.clone(), None, RangeType::Numeric),
        [Item::Number(from), Item::Number(to)] => (Some(from.clone()), to.clone(), None, RangeType::Numeric),
        [Item::Number(from), Item::Number(to), Item::Number(step)] => (Some(from.clone()), to.clone(), Some(step.clone()), RangeType::Numeric),
        [Item::Char(from), Item::Char(to)] => {
            let case = from.case();
            let from_ix = env.alpha.ord(from)?;
            let to_ix = env.alpha.ord(to)?;
            (Some(from_ix.into()), to_ix.into(), None, RangeType::Character(case))
        },
        [Item::Char(from), Item::Char(to), Item::Number(step)] => {
            let case = from.case();
            let from_ix = env.alpha.ord(from)?;
            let to_ix = env.alpha.ord(to)?;
            (Some(from_ix.into()), to_ix.into(), Some(step.clone()), RangeType::Character(case))
        },
        _ => return Err(StreamError::usage(&node.head))
    };
    if empty_helper(from.as_ref(), &to, step.as_ref()) {
        Ok(Item::empty_stream())
    } else {
        Ok(Item::new_stream(Range{head: node.head.clone(), from, to, step, rtype, env: env.clone()}))
    }
}

fn empty_helper(from: Option<&Number>, to: &Number, step: Option<&Number>) -> bool {
    (step.is_some_and(Number::is_negative) && to > from.unwrap_or(&Number::one()))
        || (step.is_none_or(Number::is_positive) && to < from.unwrap_or(&Number::one()))
}

fn len_helper(from: Option<&Number>, to: &Number, step: Option<&Number>) -> Option<UNumber> {
    if empty_helper(from, to, step) {
        return Some(UNumber::zero());
    }
    match step.map(|step| (step, i32::try_from(step))) {
        None | Some((_, Ok(1))) => Some(UNumber::try_from(match from {
                Some(from) => to - from + 1,
                None => to.to_owned() })
            .expect("expected to > from when step > 0")),
        Some((_, Ok(-1))) => Some(UNumber::try_from(from.expect("step should not be Some if from is not") - to + 1)
            .expect("expected to < from when step < 0")),
        Some((_, Ok(0))) => None,
        Some((step, _)) => Some(UNumber::try_from((to - from.expect("step should not be Some if from is not")) / step + 1)
            .expect("to-from / step sign mismatch")),
    }
}

struct Range {
    head: Head,
    from: Option<Number>,
    to: Number,
    step: Option<Number>,
    rtype: RangeType,
    env: Env,
}

enum RangeType {
    Numeric,
    Character(CharCase)
}

impl Stream for Range {
    fn iter(&self) -> Result<Box<dyn SIterator + '_>, StreamError> {
        Ok(Box::new(RangeIter{
            parent: self,
            value: match &self.from {
                Some(from) => from.clone(),
                None => Number::one()
            }
        }))
    }

    fn len(&self) -> Length {
        match len_helper(self.from.as_ref(), &self.to, self.step.as_ref()) {
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

impl SIterator for RangeIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        if self.parent.step.as_ref().is_some_and(Number::is_zero)
            || (self.parent.step.as_ref().is_none_or(Number::is_positive) && self.value <= self.parent.to)
            || (self.parent.step.as_ref().is_some_and(Number::is_negative) && self.value >= self.parent.to) {
                let ret = match self.parent.rtype {
                    RangeType::Numeric => Item::new_number(self.value.clone()),
                    RangeType::Character(case) => Item::new_char(self.parent.env.alpha.chr(&self.value, case))
                };
                match &self.parent.step {
                    Some(step) => self.value += step,
                    None => self.value += 1
                }
                Ok(Some(ret))
        } else {
            Ok(None)
        }
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        if empty_helper(Some(&self.value), &self.parent.to, self.parent.step.as_ref()) {
            return Ok(Some(n))
        };
        let Some(max) = len_helper(Some(&self.value), &self.parent.to, self.parent.step.as_ref())
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
        match len_helper(Some(&self.value), &self.parent.to, self.parent.step.as_ref()) {
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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("..", eval_range, r#"
A range of values (numbers or characters).
= from..to
> 1..3 => [1, 2, 3]
> 'a'..'c' => ['a', 'b', 'c']
: range
"#);
    symbols.insert("range", eval_range, r#"
A range of values. If `from` or `step` are not given, they default to 1.
Also works for characters, in this case `from` must be given. `step` remains numeric.
The shorthand for `?range(from, to)` is `from..to`.
= ?(to)
= ?(from, to)
= ?(from, to, step)
> ?(3) => [1, 2, 3]
> ?(3, 5) => [3, 4, 5]
> ?(0, 10, 3) => [0, 3, 6, 9]
> ?(1, 2, 0) => [1, 1, 1, 1, 1, ...]
> ?(10, 0, -3) => [10, 7, 4, 1]
: seq
: ..
"#);
}
