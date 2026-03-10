use crate::base::*;

fn eval_reorder(node: &Node, env: &Env) -> Result<Item, StreamError> {
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
        && max_index > len {
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
    fn iter(&self) -> Result<Box<dyn SIterator + '_>, StreamError> {
        Ok(Box::new(ReorderIter {
            parent: self,
            iter: RandomAccess::new(&self.source),
            state: ReorderState::Args { vec_iter: self.indices.iter() },
            pos: UNumber::zero()
        }))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

struct ReorderIter<'node> {
    parent: &'node ReorderStream,
    iter: RandomAccess<'node, Item>,
    state: ReorderState<'node>,
    pos: UNumber,
}

enum ReorderState<'node> {
    Args { vec_iter: std::slice::Iter<'node, UNumber> },
    Missed { index: UNumber },
    Rest
}

impl SIterator for ReorderIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        loop {
            check_stop!();
            match &mut self.state {
                ReorderState::Args{vec_iter} => {
                    if let Some(next) = vec_iter.next() {
                        match self.iter.nth_from_start(next - 1u32)? {
                            Some(item) => {
                                self.pos += 1;
                                return Ok(Some(item))
                            },
                            None => return Err("index past end".into())
                        }
                    }
                    if usize::try_from(&self.parent.max_index)
                            .is_ok_and(|max| max == self.parent.indices.len()) {
                        self.iter.move_to(self.parent.max_index.to_owned())?;
                        self.state = ReorderState::Rest;
                    } else {
                        self.state = ReorderState::Missed{index: UNumber::one()};
                    }
                },
                ReorderState::Missed{index} => {
                    if *index > self.parent.max_index {
                        self.iter.move_to(&*index - 1u32)?;
                        self.state = ReorderState::Rest;
                        continue;
                    }
                    if self.parent.indices.contains(index) {
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
                }
            }
        }
    }

    fn len_remain(&self) -> Length {
        self.parent.source.len().map(|len| if len >= &self.pos { len - &self.pos }
            else { UNumber::zero() })
    }

    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            match &mut self.state {
                ReorderState::Args{vec_iter} => {
                    while !n.is_zero() && vec_iter.next().is_some() {
                        n -= 1;
                        self.pos += 1;
                    }
                    if n.is_zero() {
                        return Ok(None);
                    }
                    self.state = ReorderState::Missed{index: UNumber::one()};
                },
                ReorderState::Missed{index} => {
                    let new_index = &*index + &n;
                    let skipped = self.parent.indices.iter()
                        .filter(|ix| (&*index..&new_index).contains(ix))
                        .count();
                    self.pos += &n - skipped;
                    match self.iter.advance(n) {
                        Ok(None) => {
                            *index = new_index;
                            n = skipped.into()
                        },
                        Ok(Some(remain)) => return Ok(Some(remain + skipped)),
                        Err(err) => return Err(err)
                    }
                    if self.pos > self.parent.max_index {
                        self.state = ReorderState::Rest;
                    }
                },
                ReorderState::Rest => {
                    self.pos += &n;
                    return self.iter.advance(n)
                }
            }
        }
    }
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
        test_advance("range(10).reorder(5)");
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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("reorder", eval_reorder, r#"
Applies a given permutation on the input `stream`: returning `stream[i1]`, `stream[i2]` etc., followed by all the remaining items.
= stream.?(i1, ..., iK)
> ('a'..'e').?(3, 2, 1) => ['c', 'b', 'a', 'd', 'e']
> (1..5).?(3) => [3, 1, 2, 4, 5]
> ?seq.?(10, 10) => !indices can not repeat
: perm
"#);
}
