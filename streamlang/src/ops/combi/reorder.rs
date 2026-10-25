use crate::base::*;

fn eval_reorder(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.to_stream()?;
    let mut indices = Vec::with_capacity(node.args.len());
    for arg in &node.args {
        let index = arg.to_num()?.try_cast_within(UNumber::one()..)?;
        if indices.contains(&index) {
            return Err(format!("index {} repeats", index).into());
        }
        indices.push(index);
    }
    let max_index = indices.iter().max().cloned().unwrap_or_default();
    if let Length::Exact(len) | Length::AtMost(len) = stm.len()
        && max_index > len
    {
        return Err("requested index exceeds length of source".into());
    }
    Ok(Item::new_stream(ReorderStream { source: stm, head: node.head, indices, max_index }))
}

struct ReorderStream {
    source: Rc<dyn Stream>,
    head: Head,
    indices: Vec<UNumber>,
    max_index: UNumber,
}

impl Describe for ReorderStream {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.indices)
            .finish(prec)
    }
}

impl Stream for ReorderStream {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        ReorderIter {
            iter: RandomAccess::new(Rc::clone(&self.source)),
            state: ReorderState::Args { iter: 0..self.indices.len() },
            pos: UNumber::zero(),
            node: self,
        }
        .wrap()
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

struct ReorderIter {
    node: Rc<ReorderStream>,
    iter: RandomAccess,
    state: ReorderState,
    pos: UNumber,
}

enum ReorderState {
    Args { iter: std::ops::Range<usize> },
    Missed { index: UNumber },
    Rest,
}

impl PreIterator for ReorderIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        loop {
            check_stop!();
            match &mut self.state {
                ReorderState::Args { iter } => {
                    if let Some(next) = iter.next() {
                        match self.iter.nth_from_start(&self.node.indices[next] - 1u32)? {
                            Some(item) => {
                                self.pos += 1;
                                return Ok(Some(item));
                            },
                            None => return Err("index past end".into()),
                        }
                    }
                    if usize::try_from(&self.node.max_index).is_ok_and(|max| max == self.node.indices.len()) {
                        self.iter.move_to(self.node.max_index.to_owned())?;
                        self.state = ReorderState::Rest;
                    } else {
                        self.state = ReorderState::Missed { index: UNumber::one() };
                    }
                },
                ReorderState::Missed { index } => {
                    if *index > self.node.max_index {
                        self.iter.move_to(&*index - 1u32)?;
                        self.state = ReorderState::Rest;
                        continue;
                    }
                    if self.node.indices.contains(index) {
                        *index += 1;
                        continue;
                    } else {
                        let ret = self.iter.nth_from_start(&*index - 1u32);
                        *index += 1;
                        self.pos += 1;
                        return ret;
                    }
                },
                ReorderState::Rest => {
                    self.pos += 1;
                    return self.iter.next();
                },
            }
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut n = n.clone();
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            match &mut self.state {
                ReorderState::Args { iter } => {
                    while !n.is_zero() && iter.next().is_some() {
                        n -= 1;
                        self.pos += 1;
                    }
                    if n.is_zero() {
                        return Ok(None);
                    }
                    self.state = ReorderState::Missed { index: UNumber::one() };
                },
                ReorderState::Missed { index } => {
                    // The source may be positioned anywhere after reading the listed items.
                    if self.iter.move_to(&*index - 1u32)?.is_some() {
                        return Ok(Some(n));
                    }
                    let res = self.iter.advance(&n)?;
                    let traversed = match &res {
                        None => n.clone(),
                        Some(remain) => &n - remain,
                    };
                    let new_index = &*index + &traversed;
                    let skipped = self
                        .node
                        .indices
                        .iter()
                        .filter(|ix| (&*index..&new_index).contains(ix))
                        .count();
                    self.pos += &traversed - skipped;
                    match res {
                        None => {
                            *index = new_index;
                            n = skipped.into()
                        },
                        Some(remain) => return Ok(Some(remain + skipped)),
                    }
                    if *index > self.node.max_index {
                        self.state = ReorderState::Rest;
                    }
                },
                ReorderState::Rest => {
                    self.pos += &n;
                    return self.iter.advance(&n);
                },
            }
        }
    }

    fn origin(&self) -> &Rc<ReorderStream> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("reorder", eval_reorder, r#"
Moves the items at positions `i1`, ..., `iK` of `stream` to the front, in that order, followed by all remaining items in their original order.
= stream.?(i1, ..., iK)
> ('a'..'e').?(3, 2, 1) => ['c', 'b', 'a', 'd', 'e']
> (1..5).?(3) => [3, 1, 2, 4, 5]
> ?seq.?(10, 10) => !index 10 repeats
: perm
"#);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_reorder() {
        use super::*;
        test_eval!("('a'..'e').reorder(1)" => "['a', 'b', 'c', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3)" => "['c', 'a', 'b', 'd', 'e']");
        test_eval!("('a'..'e').reorder(5)" => "['e', 'a', 'b', 'c', 'd']");
        test_eval!("('a'..'e').reorder(0)" => err);
        test_eval!("('a'..'e').reorder(6)" => err);
        test_eval!("('a'..'e').reorder()" => "['a', 'b', 'c', 'd', 'e']");
        test_eval!("('a'..'e').$lenAM.reorder(6)" => err);
        test_eval!("('a'..'e').$lenUF.reorder(6)" => "[<!>");
        test_eval!("('a'..'e').reorder(3,2)" => "['c', 'b', 'a', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3,4)" => "['c', 'd', 'a', 'b', 'e']");
        test_eval!("('a'..'e').reorder(2,4)" => "['b', 'd', 'a', 'c', 'e']");
        test_eval!("('a'..'e').reorder(3,1)" => "['c', 'a', 'b', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3,2,1)" => "['c', 'b', 'a', 'd', 'e']");
        test_eval!("('a'..'e').reorder(3,3)" => err);
        test_eval!("('a'..'e').reorder(3,3)" => err);
        test_eval!("range(10^100).reorder(10^10)" => "[10000000000, 1, 2, 3, 4, ...]");
        test_eval!("range(10).reorder(5).stride(9)" => "[5, 10]");
        test_eval!("range(10).reorder(5).stride(6)" => "[5, 7]");
        test_eval!("range(10).reorder(5).stride(5)" => "[5, 6]");
        test_eval!("range(10).reorder(5,2).stride(4)" => "[5, 4, 9]");
        test_eval!("range(10).reorder(5,2)[3..10]" : 10 => "[1, 3, 4, 6, 7, 8, 9, 10]");
        test_eval!("range(10).reorder(2,5).skip(3).first" => "3");
        test_eval!("range(10).$lenUF.reorder(5).stride(9)" => "[5, 10]");
        test_eval!("range(10).$lenUU.reorder(5,2).stride(4)" => "[5, 4, 9]");
        test_advance("range(10).reorder(5)");
        test_advance("range(10).$lenUF.reorder(5)");
        test_advance("range(10).$lenUU.reorder(5,2)");
        test_advance("range(10).reorder(5,2)");
        test_advance("range(10).reorder(5,6)");
        test_advance("range(10).reorder(5,7)");
        test_advance("range(10).reorder");
        test_advance("range(10).reorder(3,1,2,4,5)");
        test_advance("range(10).reorder(1,2,3,4,5,6,7,8,9,10)");
        test_advance("range(10).reorder(10,9,8,7,6,5,4,3,2,1)");
        test_describe!("range(10).reorder(5,2)" => "range(10).reorder(5, 2)");
    }
}
