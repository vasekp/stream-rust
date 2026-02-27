use crate::base::*;

struct Riffle {
    head: Head,
    source: Rc<dyn Stream>,
    filler: Item
}

impl Riffle {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        let rnode = node.eval_all(env)?.resolve_source()?;
        let (source, filler) = match rnode {
            RNodeS { source: Item::Stream(ref src), args: RArgs::One(_), .. }
            if src.is_empty()
                => return Ok(Item::empty_stream()),
            RNodeS { source: Item::Stream(src), args: RArgs::One(filler), .. }
                => (Rc::clone(&src), filler),
            _ => return Err(StreamError::new("expected: stream.riffle(item or stream)", rnode))
        };
        Ok(Item::new_stream(Riffle{head: rnode.head, source, filler}))
    }
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
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let mut source_iter = self.source.iter();
        let filler_iter = match &self.filler {
            Item::Stream(stm) => stm.iter(),
            item => Box::new(std::iter::repeat(Ok(item.clone())))
        };
        let source_next = source_iter.next();
        Box::new(RiffleIter {
            source: source_iter,
            filler: filler_iter,
            source_next,
            which: RiffleState::Source
        })
    }

    fn len(&self) -> Length {
        use Length::*;
        let len1 = self.source.len();
        let len2 = match &self.filler {
            Item::Stream(stm) => stm.len(),
            _ => Infinite
        };
        Length::intersection(len1.map(|u| 2u32 * u - 1u32), len2.map(|v| 2u32 * v + 1u32))
    }

    fn is_empty(&self) -> bool {
        false
    }
}

struct RiffleIter<'node> {
    source: Box<dyn SIterator + 'node>,
    filler: Box<dyn SIterator + 'node>,
    source_next: Option<Result<Item, StreamError>>,
    which: RiffleState
}

#[derive(PartialEq)]
enum RiffleState {
    Source,
    Filler
}

impl Iterator for RiffleIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        use RiffleState::*;
        match self.which {
            Source => {
                let next = self.source_next.take()?;
                self.which = Filler;
                Some(next)
            },
            Filler => {
                self.source_next = self.source.next();
                self.which = Source;
                if self.source_next.is_none() { None } else { self.filler.next() }
            }
        }
    }
}

impl SIterator for RiffleIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let common = Length::intersection(self.source.len_remain(), self.filler.len_remain());
        let skip = match Length::intersection(common, Length::Exact(&n / 2u32)) {
            Length::Exact(len) => len,
            _ => UNumber::zero()
        };
        let mut remain = if !skip.is_zero() {
            self.filler.advance(skip.clone())?;
            let n_new = n - 2u32 * &skip;
            match self.which {
                RiffleState::Source => {
                    self.source.advance(skip - 1u32)?;
                    self.source_next = self.source.next();
                },
                RiffleState::Filler => {
                    self.source.advance(skip)?;
                }
            };
            n_new
        } else {
            n
        };
        while !remain.is_zero() {
            if self.next().transpose()?.is_none() {
                return Ok(Some(remain));
            }
            remain.dec();
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
    symbols.insert("riffle", Riffle::eval, r#"
Interleaves `stream` with copies of `filler`.
If `filler` is also a stream, interleaves the two streams, until one of them ends.
= stream.?(filler)
> ?seq.?(0) => [1, 0, 2, 0, 3, ...]
> ?range(1, 2).?(0) => [1, 0, 2]
> ?seq.?(['a', 'b']) => [1, 'a', 2, 'b', 3]
: zip
: cat
"#);
}
