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
        let item = match source_iter.next() {
            Ok(Some(item)) => item,
            Ok(None) => return Box::new(std::iter::empty()),
            Err(err) => return iter_error(err, &self),
        };
        RiffleIter {
            source: source_iter,
            filler: filler_iter,
            which: RiffleState::Source{next: item},
            node: self,
        }.wrap()
    }

    fn len(&self) -> Length {
        let len1 = self.source.len();
        let len2 = match &self.filler {
            Item::Stream(stm) => stm.len(),
            _ => Length::Infinite
        };
        // len1 == 0 handled in eval()
        Length::intersection(len1.map(|u| 2u32 * u - 1u32), len2.map(|v| 2u32 * v + 1u32))
    }
}

struct RiffleIter {
    node: Rc<Riffle>,
    source: Box<dyn SIterator>,
    filler: Box<dyn SIterator>,
    which: RiffleState
}

enum RiffleState {
    Source{next: Item},
    Filler
}

impl PreIterator for RiffleIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        use RiffleState::*;
        match std::mem::replace(&mut self.which, RiffleState::Filler) {
            Source{next} => {
                //self.which = Filler; happens in mem::replace
                Ok(Some(next))
            },
            Filler => {
                self.which = Source{next: iter_try!(self.source.next())};
                self.filler.next()
            }
        }
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        if n.is_zero() {
            return Ok(None);
        }
        let mut n = n.clone();
        if matches!(&self.which, RiffleState::Source{..}) {
            self.next()?; // can't fail
            n -= 1;
        }
        debug_assert!(matches!(&self.which, RiffleState::Filler));
        let (half, odd) = n.div_rem(2u32);
        let rem = match (self.source.advance(&half)?, self.filler.advance(&half)?) {
            (None, None) => None,
            (None, Some(r2)) => Some(r2),
            (Some(r1), None) => Some(r1),
            (Some(r1), Some(r2)) => Some(std::cmp::max(r1, r2)),
        };
        if let Some(rem) = rem {
            Ok(Some(2 * rem + odd))
        } else {
            if odd == 1 && self.next()?.is_none() {
                Ok(Some(1u32.into()))
            } else {
                Ok(None)
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
