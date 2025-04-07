use crate::base::*;
use crate::utils::{TriState, EmptyString, EmptyStream};

#[derive(Clone)]
struct Riffle {
    head: Head,
    source: BoxedStream,
    filler: Item
}

impl Riffle {
    fn eval(node: Node, env: &std::rc::Rc<Env>) -> Result<Item, StreamError> {
        let mut node = node.eval_all(env)?;
        let filler = match &mut node.args[..] {
            [f] => f,
            _ => return Err(StreamError::new("exactly 1 argument required", node))
        };
        let source: BoxedStream = match node.source {
            Some(Item::Stream(s)) => s.into(),
            Some(ref item) => return Err(StreamError::new(format!("expected stream, found {:?}", item), node)),
            _ => return Err(StreamError::new("source required", node))
        };
        let filler = std::mem::take(filler);
        if source.is_empty() {
            if source.is_string().is_true() { Ok(Item::new_stream(EmptyString())) }
            else { Ok(Item::new_stream(EmptyStream())) }
        } else {
            Ok(Item::new_stream(Riffle{head: node.head, source, filler}))
        }
    }
}

impl Describe for Riffle {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, Some(&self.source), [&self.filler])
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

    fn is_string(&self) -> TriState {
        self.source.is_string()
    }

    fn length(&self) -> Length {
        use Length::*;
        let len1 = self.source.length();
        let len2 = match &self.filler {
            Item::Stream(stm) => stm.length(),
            _ => Infinite
        };
        Length::intersection(&len1.map(|u| 2u32 * u - 1u32), &len2.map(|v| 2u32 * v + 1u32))
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
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        let common = Length::intersection(&self.source.len_remain(), &self.filler.len_remain());
        let skip = match Length::intersection(&common, &Length::Exact(&n / 2u32)) {
            Length::Exact(len) => len,
            _ => UNumber::zero()
        };
        let mut remain = if !skip.is_zero() {
            self.filler.skip_n(skip.clone())?;
            let n_new = n - 2u32 * &skip;
            match self.which {
                RiffleState::Source => {
                    self.source.skip_n(skip - 1u32)?;
                    self.source_next = self.source.next();
                },
                RiffleState::Filler => {
                    self.source.skip_n(skip)?;
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
        let common = Length::intersection(&len1, &len2);
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
        use crate::parser::parse;
        assert_eq!(parse("seq.riffle(seq + 3)").unwrap().eval().unwrap().to_string(), "[1, 4, 2, 5, 3, ...]");
        assert_eq!(parse("0.repeat.riffle(1)").unwrap().eval().unwrap().to_string(), "[0, 1, 0, 1, 0, ...]");
        assert_eq!(parse("[1,2,3].riffle('a')").unwrap().eval().unwrap().to_string(), "[1, 'a', 2, 'a', 3]");
        assert_eq!(parse("seq.riffle(['a'])").unwrap().eval().unwrap().to_string(), "[1, 'a', 2]");
        assert_eq!(parse("seq.riffle([])").unwrap().eval().unwrap().to_string(), "[1]");
        assert_eq!(parse("[1,2].riffle(['a', 'b'])").unwrap().eval().unwrap().to_string(), "[1, 'a', 2]");
        assert_eq!(parse("['a','b'].riffle(seq)").unwrap().eval().unwrap().to_string(), "['a', 1, 'b']");
        assert_eq!(parse("\"abc\".riffle(',')").unwrap().eval().unwrap().to_string(), "\"a,b,c\"");
        assert_eq!(parse("\"abc\".riffle(0)").unwrap().eval().unwrap().to_string(), "\"a<!>");
        assert!(parse("1.riffle(2)").unwrap().eval().is_err());
        test_len_exact(&parse("[1,2,3].riffle('a')").unwrap().eval().unwrap(), 5);
        test_len_exact(&parse("[1,2,3].riffle(['a'])").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("[1,2,3].riffle([])").unwrap().eval().unwrap(), 1);
        test_len_exact(&parse("seq.riffle(['a'])").unwrap().eval().unwrap(), 3);
        test_len_exact(&parse("seq.riffle([])").unwrap().eval().unwrap(), 1);
        test_len_exact(&parse("[].riffle(0)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("\"\".riffle(0)").unwrap().eval().unwrap(), 0);
        test_skip_n(&parse("seq.riffle(seq)").unwrap().eval().unwrap());
        test_skip_n(&parse("seq.riffle(range(100))").unwrap().eval().unwrap());
        test_skip_n(&parse("seq.riffle([])").unwrap().eval().unwrap());
        test_skip_n(&parse("seq.riffle(0)").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(seq)").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(0)").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(range(50))").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(range(99))").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(range(100))").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(range(101))").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle(range(200))").unwrap().eval().unwrap());
        test_skip_n(&parse("range(100).riffle([])").unwrap().eval().unwrap());
        test_skip_n(&parse("[].riffle(range(3))").unwrap().eval().unwrap());
        test_skip_n(&parse("[].riffle(range(1))").unwrap().eval().unwrap());
        test_skip_n(&parse("[].riffle([])").unwrap().eval().unwrap());
        assert_eq!(parse("seq.riffle(['a'])").unwrap().eval().unwrap().describe(), "seq.riffle(['a'])");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("riffle", Riffle::eval);
}
