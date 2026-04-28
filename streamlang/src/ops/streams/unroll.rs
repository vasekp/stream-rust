use crate::base::*;

fn eval_unroll(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.as_stream()?;
    Ok(Item::new_stream(Unroll{source: Rc::clone(stm), head: node.head.clone()}))
}

struct Unroll {
    source: Rc<dyn Stream>,
    head: Head
}

impl Describe for Unroll {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for Unroll {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        UnrollIter {
            source: self.source.iter(),
            cached: None,
            node: self,
        }.wrap()
    }

    fn len(&self) -> Length {
        match self.source.len() {
            Length::Infinite | Length::Unknown => Length::Unknown,
            _ => Length::UnknownFinite
        }
    }
}

struct UnrollIter {
    node: Rc<Unroll>,
    source: Box<dyn SIterator>,
    cached: Option<(Item, UNumber)>,
}

impl PreIterator for UnrollIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        loop {
            check_stop!();
            if let Some((item, count)) = &mut self.cached && !count.is_zero() {
                *count -= 1;
                return Ok(Some(item.clone()));
            }
            let next = iter_try!(self.source.next());
            let list = next.as_stream()?.listout()?;
            let [item, count] = &list[..] else {
                return Err(StreamError::with_expr("expected [item, count]", &next));
            };
            let count = count.as_num()?.try_unsign()?;
            self.cached = Some((item.clone(), count));
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut n = n.clone();
        loop {
            check_stop!();
            if let Some((_, count)) = &mut self.cached && !count.is_zero() {
                if *count >= n {
                    *count -= n;
                    return Ok(None);
                } else {
                    n -= &*count;
                }
            }
            let Some(next) = self.source.next()? else {
                return Ok(Some(n));
            };
            let list = next.as_stream()?.listout()?;
            let [item, count] = &list[..] else {
                return Err(StreamError::with_expr("expected [item, count]", &next));
            };
            let count = count.as_num()?.try_unsign()?;
            self.cached = Some((item.clone(), count));
        }
    }

    fn origin(&self) -> &Rc<Unroll> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_unroll() {
        use super::*;
        test_eval!("(1..3):{[#,1]}.unroll" => "[1, 2, 3]");
        test_eval!("(1..3):{[#,2]}.unroll" : 10 => "[1, 1, 2, 2, 3, 3]");
        test_eval!("(1..3):{[#,0]}.unroll" => "[]");
        test_eval!("(1..10):{[#,if(#<3,#+1,0)]}.unroll" => "[1, 1, 2, 2, 2]");
        test_eval!("seq:{[#,#]}.unroll" : 10 => "[1, 2, 2, 3, 3, 3, 4, 4, 4, 4, ...]");
        test_advance("(1..1000):{[#,10^6*#]}.unroll");
        test_advance("seq:{[#,10^6*#]}.unroll");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("unroll", eval_unroll, r#"
Takes a stream of pairs `[item, count]` and returns a stream where each `item` repeats `count`-times.
= stream.?
> [['A', 1], ['B', 2], ['A', 1]].?.?string => "ABBA"
: repeat
: reps
"#);
}
