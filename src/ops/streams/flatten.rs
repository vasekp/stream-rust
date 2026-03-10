use crate::base::*;

fn eval_flatten(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.to_stream()?;
    let depth = match &node.args[..] {
        [] => None,
        [Item::Number(depth)] => Some(depth.try_unsign()?),
        _ => return Err(StreamError::usage(&node.head))
    };
    Ok(Item::new_stream(Flatten { source: stm, head: node.head.clone(), depth }))
}

struct Flatten {
    source: Rc<dyn Stream>,
    depth: Option<UNumber>,
    head: Head
}

impl Describe for Flatten {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.depth)
            .finish(prec)
    }
}

impl Stream for Flatten {
    fn iter0<'node>(&'node self) -> Result<Box<dyn SIterator + 'node>, StreamError> {
        Ok(Box::new(FlattenIter {
            outer: self.source.iter(),
            iters: vec![],
            depth: self.depth.as_ref().and_then(|d| d.try_into().ok())
        }))
    }

    fn len(&self) -> Length {
        Length::Unknown
    }
}

struct FlattenIter<'node> {
    outer: Box<dyn SIterator + 'node>,
    iters: Vec<OwnedStreamIter>,
    depth: Option<usize>,
}

impl SIterator for FlattenIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        loop {
            check_stop!();
            let next = match self.iters.last_mut() {
                Some(iter) => iter.next(),
                None => self.outer.next()
            }?;
            match next {
                Some(Item::Stream(stm)) => {
                    if self.depth.is_some_and(|d| self.iters.len() == d) {
                        return Ok(Some(Item::Stream(stm)));
                    } else {
                        self.iters.push(stm.into_iter());
                    }
                },
                Some(item) => return Ok(Some(item)),
                None => {
                    if self.iters.is_empty() {
                        return Ok(None);
                    } else {
                        self.iters.pop();
                    }
                }
            }
        }
    }

    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            if self.depth.is_some_and(|d| self.iters.len() == d) {
                let Some(iter) = self.iters.last_mut() else { unreachable!() };
                let Some(m) = iter.advance(n)? else { return Ok(None); };
                self.iters.pop();
                n = m;
            } else {
                let res = match self.iters.last_mut() {
                    Some(iter) => iter.next(),
                    None => self.outer.next()
                };
                match res? {
                    Some(Item::Stream(stm)) => {
                        self.iters.push(stm.into_iter());
                    },
                    Some(_) => n -= 1,
                    None => {
                        if self.iters.is_empty() {
                            return Ok(Some(n));
                        } else {
                            self.iters.pop();
                        }
                    }
                }
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_flatten() {
        test_eval!("[1, [2, [3]]].flatten" => "[1, 2, 3]");
        test_eval!("[1, [2, [3]]].flatten(1)" => "[1, 2, [3]]");
        test_eval!("[0].nest{[#]}.flatten" => "[0, 0, 0, 0, 0, ...]");
        test_eval!("[0].nest{[#]}.flatten(3)" => "[0, 0, [0], [...], ...]");
        test_eval!("[0].nest{[#]}.flatten(10^10)" => "[0, 0, 0, 0, 0, ...]");
        test_eval!("[\"ab\",\"cd\"].flatten" => "[\"ab\", \"cd\"]");
        test_len!("[].flatten" => 0);
        test_advance("[1,range(3)].repeat(10).flatten");
        test_advance("[1,[2,[3]]].repeat(10).flatten");
        test_advance("[1,[2,[3]]].repeat(10).flatten(1)");
        test_advance("[1,[2,[3]]].repeat(10).flatten(2)");
        test_advance("[1,[2,[3]]].repeat(10).flatten(10)");
        test_describe!("[1, [2, [3]]].flatten" => "[1, [2, [3]]].flatten");
        test_describe!("[1, [2, [3]]].flatten(1)" => "[1, [2, [3]]].flatten(1)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("flatten", eval_flatten, r#"
Flattens `stream` up to `depth` levels. If `depth` is omitted, `stream` is flattened to all levels.
= stream.?
= stream.?(depth)
> [1, [2, [3]]].? => [1, 2, 3]
> [1, [2, [3, [4]]]].?(1) : 10 => [1, 2, [3, [4]]]
> [1, [], [2], [], 3].? => [1, 2, 3]
"#);
}
