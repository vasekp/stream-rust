use crate::base::*;

#[derive(Clone)]
pub struct Range {
    head: Head,
    from: Option<Number>,
    to: Number,
    step: Option<Number>,
    rtype: RangeType,
    env: Rc<Env>
}

#[derive(Clone, Copy)]
enum RangeType {
    Numeric,
    Character(CharCase)
}

impl Range {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
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
                    let (from_ix, case) = try_with!(rnode, abc.ord_case(from)?);
                    let (to_ix, _) = try_with!(rnode, abc.ord_case(to)?);
                    (Some(from_ix.into()), to_ix.into(), None, RangeType::Character(case))
                },
            RNodeNS { args: RArgs::Three(Item::Char(ref from), Item::Char(ref to), Item::Number(ref mut step)), .. }
                => {
                    let abc = env.alphabet();
                    let (from_ix, case) = try_with!(rnode, abc.ord_case(from)?);
                    let (to_ix, _) = try_with!(rnode, abc.ord_case(to)?);
                    (Some(from_ix.into()), to_ix.into(), Some(std::mem::take(step)), RangeType::Character(case))
                },
            _ => return Err(StreamError::new("expected one of: range(num), range(num, num), range(num, num, num), range(char, char), range(char, char, num)", rnode))
        };
        if Range::empty_helper(from.as_ref(), &to, step.as_ref()) {
            Ok(Item::new_stream(EmptyStream::List))
        } else {
            Ok(Item::new_stream(Range{head: rnode.head, from, to, step, rtype, env: Rc::clone(env)}))
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

    fn length(&self) -> Length {
        match Range::len_helper(self.from.as_ref(), &self.to, self.step.as_ref()) {
            Some(num) => Length::Exact(num),
            None => Length::Infinite
        }
    }
}

impl Describe for Range {
    fn describe_prec(&self, prec: u32) -> String {
        match self.rtype {
            RangeType::Numeric => Node::describe_helper(&self.head, None::<&Item>,
                [self.from.as_ref(), Some(&self.to), self.step.as_ref()].into_iter().flatten(), 
                prec),
            RangeType::Character(case) => {
                let abc = self.env.alphabet();
                self.env.wrap_describe(|prec|
                    Node::describe_helper(&self.head, None::<&Item>, [
                        Some(&ProxyItem::Char(&abc.chr_case(self.from.as_ref().expect("char range should have from"), case))),
                        Some(&ProxyItem::Char(&abc.chr_case(&self.to, case))),
                        self.step.as_ref().map(ProxyItem::Number).as_ref()
                    ].into_iter().flatten(), prec),
                    prec)
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
                    RangeType::Character(case) => Item::new_char(self.parent.env.alphabet().chr_case(&self.value, case))
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
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
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
        use crate::parser::parse;

        assert_eq!(parse("range(3)").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("range(0)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("range(3, 3)").unwrap().eval_default().unwrap().to_string(), "[3]");
        assert_eq!(parse("range(3, 5)").unwrap().eval_default().unwrap().to_string(), "[3, 4, 5]");
        assert_eq!(parse("range(5, 3)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("range(1, 10, 4)").unwrap().eval_default().unwrap().to_string(), "[1, 5, 9]");
        assert_eq!(parse("range(1, 10, 10)").unwrap().eval_default().unwrap().to_string(), "[1]");
        assert_eq!(parse("range(1, 10, 0)").unwrap().eval_default().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
        assert_eq!(parse("range(1, 1, 0)").unwrap().eval_default().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
        assert_eq!(parse("range(1, 10, -1)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("range(1, -10, -3)").unwrap().eval_default().unwrap().to_string(), "[1, -2, -5, -8]");

        assert_eq!(parse("range('a', 'C')").unwrap().eval_default().unwrap().to_string(), "['a', 'b', 'c']");
        assert_eq!(parse("range('D', 'f')").unwrap().eval_default().unwrap().to_string(), "['D', 'E', 'F']");
        assert_eq!(parse("range('a', 'h', 3)").unwrap().eval_default().unwrap().to_string(), "['a', 'd', 'g']");
        assert_eq!(parse("range('a', 'z', -1)").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("range('a', 'z', 0)").unwrap().eval_default().unwrap().to_string(), "['a', 'a', 'a', 'a', 'a', ...]");
        assert!(parse("range('a')").unwrap().eval_default().is_err());
        assert!(parse("range('a', 1)").unwrap().eval_default().is_err());
        assert!(parse("range(1, 'a')").unwrap().eval_default().is_err());
        assert!(parse("range('a', 'h', 'c')").unwrap().eval_default().is_err());

        assert_eq!(parse("1..3").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        assert_eq!(parse("3..3").unwrap().eval_default().unwrap().to_string(), "[3]");
        assert_eq!(parse("3..1").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("-1..3").unwrap().eval_default().unwrap().to_string(), "[-1, -2, -3]");
        assert!(parse("1..2..3").unwrap().eval_default().is_err());
        assert_eq!(parse("'a'..'z'").unwrap().eval_default().unwrap().to_string(), "['a', 'b', 'c', 'd', 'e', ...]");
        assert_eq!(parse("'A'..'z'").unwrap().eval_default().unwrap().to_string(), "['A', 'B', 'C', 'D', 'E', ...]");
        assert!(parse("'a'..'á'").unwrap().eval_default().is_err());

        assert_eq!(parse("range(10^9, 10^10, 2).len").unwrap().eval_default().unwrap().to_string(), "4500000001");
        test_len_exact(&parse("range(0)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("range(-1)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("range(3)").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("range(0,3)").unwrap().eval_default().unwrap(), 4);
        test_len_exact(&parse("range(3,0)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("range(3,3)").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("range(0,3,2)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("range(0,3,3)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("range(0,3,4)").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("range(0,4,2)").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("range(0,3,-2)").unwrap().eval_default().unwrap(), 0);
        test_len_exact(&parse("range(0,-3,-2)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("range(0,-3,-3)").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("range(0,-3,-4)").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("'a'..'z'").unwrap().eval_default().unwrap(), 26);
        test_skip_n(&parse("range(0)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(-10^10)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,3)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(3,0)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,3,2)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,3,3)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,3,4)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,4,2)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,3,0)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,3,-2)").unwrap().eval_default().unwrap());
        test_skip_n(&parse("range(0,-3,-2)").unwrap().eval_default().unwrap());

        assert_eq!(parse("range(5)").unwrap().eval_default().unwrap().describe(), "range(5)");
        assert_eq!(parse("range(0)").unwrap().eval_default().unwrap().describe(), "[]");
        assert_eq!(parse("range(1,5)").unwrap().eval_default().unwrap().describe(), "range(1, 5)");
        assert_eq!(parse("range(1,5,2)").unwrap().eval_default().unwrap().describe(), "range(1, 5, 2)");
        assert_eq!(parse("range('a','Z')").unwrap().eval_default().unwrap().describe(), "range('a', 'z')");
        assert_eq!(parse("range('A','z',2)").unwrap().eval_default().unwrap().describe(), "range('A', 'Z', 2)");
        assert_eq!(parse("1..5").unwrap().eval_default().unwrap().describe(), "1..5");
        assert_eq!(parse("(-1)..5").unwrap().eval_default().unwrap().describe(), "(-1)..5");
        assert_eq!(parse("-(1..5)").unwrap().eval_default().unwrap().describe(), "-1..5");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("range", Range::eval);
    keywords.insert("..", Range::eval);
}
