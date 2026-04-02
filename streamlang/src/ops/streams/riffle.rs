use crate::base::*;

fn eval_riffle(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.to_stream()?;
    if stm.is_empty()? { return Ok(Item::empty_stream()); }
    let filler = node.only_arg_checked()?;
    Ok(Item::new_stream(Riffle{head: node.head.clone(), source: stm, filler: filler.clone()}))
}

struct Riffle {
    head: Head,
    source: Rc<dyn Stream>,
    filler: Item
}

impl Describe for Riffle {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.filler)
            .finish(prec)
    }
}

impl Stream for Riffle {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let mut source_iter = self.source.iter();
        let filler_iter = match &self.filler {
            Item::Stream(stm) => stm.iter(),
            item => Box::new(std::iter::repeat(Ok(item.clone()))),
        };
        let source_next = source_iter.next().transpose();
        RiffleIter {
            source: source_iter,
            filler: filler_iter,
            source_next,
            which: RiffleState::Source,
            node: self,
        }.wrap()
    }

    fn len(&self) -> Length {
        let len1 = self.source.len();
        let len2 = match &self.filler {
            Item::Stream(stm) => stm.len(),
            _ => Length::Infinite
        };
        Length::intersection(len1.map(|u| 2u32 * u - 1u32), len2.map(|v| 2u32 * v + 1u32))
    }
}

struct RiffleIter {
    node: Rc<Riffle>,
    source: Box<dyn SIterator>,
    filler: Box<dyn SIterator>,
    source_next: Option<SResult<Item>>,
    which: RiffleState
}

#[derive(PartialEq)]
enum RiffleState {
    Source,
    Filler
}

impl PreIterator for RiffleIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        use RiffleState::*;
        match self.which {
            Source => {
                self.which = Filler;
                match self.source_next.take() {
                    Some(res) => Ok(Some(res?)),
                    None => Ok(None)
                }
            },
            Filler => {
                self.source_next = self.source.next().transpose();
                self.which = Source;
                if self.source_next.is_none() { Ok(None) } else { self.filler.next() }
            }
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let common = Length::intersection(self.source.len_remain(), self.filler.len_remain());
        let skip = match Length::intersection(common, Length::Exact(n / 2u32)) {
            Length::Exact(len) => len,
            _ => UNumber::zero()
        };
        let mut remain = if !skip.is_zero() {
            self.filler.advance(&skip)?;
            let n_new = n - 2u32 * &skip;
            match self.which {
                RiffleState::Source => {
                    self.source.advance(&(skip - 1u32))?;
                    self.source_next = self.source.next().transpose();
                },
                RiffleState::Filler => {
                    self.source.advance(&skip)?;
                }
            };
            n_new
        } else {
            n.clone()
        };
        while !remain.is_zero() {
            if self.next()?.is_none() {
                return Ok(Some(remain));
            }
            remain -= 1;
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        let len1 = self.source.len_remain();
        let len2 = self.filler.len_remain();
        let common = Length::intersection(len1, len2);
        match self.which {
            RiffleState::Source => {
                if self.source_next.is_none() {
                    Length::Exact(UNumber::zero())
                } else {
                    common.map(|x| 2u32 * x + 1u32)
                }
            },
            RiffleState::Filler => {
                common.map(|x| 2u32 * x)
            }
        }
    }

    fn origin(&self) -> &Rc<Riffle> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_riffle() {
        test_eval!("seq.riffle(seq + 3)" => "[1, 4, 2, 5, 3, ...]");
        test_eval!("0.repeat.riffle(1)" => "[0, 1, 0, 1, 0, ...]");
        test_eval!("[1,2,3].riffle('a')" => "[1, 'a', 2, 'a', 3]");
        test_eval!("seq.riffle(['a'])" => "[1, 'a', 2]");
        test_eval!("seq.riffle([])" => "[1]");
        test_eval!("[].riffle(1)" => "[]");
        test_eval!("[1,2].riffle(['a', 'b'])" => "[1, 'a', 2]");
        test_eval!("['a','b'].riffle(seq)" => "['a', 1, 'b']");
        test_eval!("\"abc\".riffle(',')" => err);
        test_eval!("\"abc\".chars.riffle(',').string" => "\"a,b,c\"");
        test_eval!("1.riffle(2)" => err);
        test_len!("[1,2,3].riffle('a')" => 5);
        test_len!("[1,2,3].riffle(['a'])" => 3);
        test_len!("[1,2,3].riffle([])" => 1);
        test_len!("seq.riffle(['a'])" => 3);
        test_len!("seq.riffle([])" => 1);
        test_len!("[].riffle(0)" => 0);
        test_advance("seq.riffle(seq)");
        test_advance("seq.riffle(range(100))");
        test_advance("seq.riffle([])");
        test_advance("seq.riffle(0)");
        test_advance("range(100).riffle(seq)");
        test_advance("range(100).riffle(0)");
        test_advance("range(100).riffle(range(50))");
        test_advance("range(100).riffle(range(99))");
        test_advance("range(100).riffle(range(100))");
        test_advance("range(100).riffle(range(101))");
        test_advance("range(100).riffle(range(200))");
        test_advance("range(100).riffle([])");
        test_advance("[].riffle(range(3))");
        test_advance("[].riffle(range(1))");
        test_advance("[].riffle([])");
        test_describe!("seq.riffle(['a'])" => "seq.riffle(['a'])");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("riffle", eval_riffle, r#"
Interleaves `stream` with copies of `filler`.
If `filler` is also a stream, interleaves the two streams, until one of them ends.
= stream.?(filler)
> ?seq.?(0) => [1, 0, 2, 0, 3, ...]
> ?range(1, 2).?(0) => [1, 0, 2]
> ?seq.?(['a', 'b']) => [1, 'a', 2, 'b', 3]
: alternate
: zip
: cat
"#);
}
