use crate::base::*;

#[derive(Clone)]
struct Cat {
    source: BoxedStream,
    head: Head,
    filler: Option<LiteralString>,
}

impl Cat {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        match node.eval_all(env)?.resolve_source()? {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero } =>
                Ok(Item::new_string(Cat { source: stm.into(), head, filler: None })),
            RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Item::String(fill)) } => {
                let filler = LiteralString::from(fill.listout()?);
                Ok(Item::new_string(Cat { source: stm.into(), head, filler: Some(filler) }))
            },
            RNodeS { head, source: Item::Stream(stm), args: RArgs::One(Item::Char(fill)) } => {
                let filler = LiteralString::from(vec![fill]);
                Ok(Item::new_string(Cat { source: stm.into(), head, filler: Some(filler) }))
            },
            node => Err(StreamError::new("expected: stream.cat", node))
        }
    }
}

impl Describe for Cat {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.filler)
            .finish(prec)
    }
}

impl Stream<Char> for Cat {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        match &self.filler {
            None => Box::new(CatIter::new(self)),
            Some(fill) => RiffleCatIter::new_boxed(self, fill)
        }
    }

    fn len(&self) -> Length {
        if self.source.is_empty() {
            Length::Exact(UNumber::zero())
        } else {
            Length::Unknown
        }
    }
}

struct CatIter<'node> {
    outer: Box<dyn SIterator + 'node>,
    inner: Option<OwnedStreamIter<Char>>,
}

impl<'node> CatIter<'node> {
    fn new(parent: &'node Cat) -> Self {
        CatIter {
            outer: parent.source.iter(),
            inner: None,
        }
    }
}

impl Iterator for CatIter<'_> {
    type Item = Result<Char, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            if let Some(ref mut iter) = &mut self.inner {
                match iter_try_expr!(iter.next().transpose()) {
                    Some(ch) => return Some(Ok(ch)),
                    None => self.inner = None
                }
            }
            match iter_try_expr!(self.outer.next()?) {
                Item::Char(ch) => return Some(Ok(ch)),
                Item::String(s) => self.inner = Some(s.into_iter()),
                item => return Some(Err(StreamError::new(format!("expected string or character, found {:?}", item), Expr::new_node("cat", vec![]))))
            }
        }
    }
}

impl SIterator<Char> for CatIter<'_> {
    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            if let Some(ref mut iter) = &mut self.inner {
                let Some(m) = iter.advance(n)? else { return Ok(None); };
                self.inner = None;
                n = m;
            } else {
                let res = self.outer.next();
                match res {
                    Some(Ok(Item::String(s))) => self.inner = Some(s.into_iter()),
                    Some(Ok(_)) => n.dec(),
                    Some(Err(err)) => return Err(err),
                    None => return Ok(Some(n))
                }
            }
        }
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

struct RiffleCatIter<'node> {
    parent: &'node Cat,
    outer: Box<dyn SIterator + 'node>,
    inner: Box<dyn SIterator<Char> + 'node>,
    filler: &'node LiteralString,
    state: RiffleCatState<'node>,
}

enum RiffleCatState<'node> {
    Source,
    Filler { next: Box<dyn SIterator<Char> + 'node> }
}

impl<'node> RiffleCatIter<'node> {
    fn new_boxed(parent: &'node Cat, filler: &'node LiteralString) -> Box<dyn SIterator<Char> + 'node> {
        let mut outer = parent.source.iter();
        let inner = match Self::next_cs(&mut *outer) {
            Some(Ok(cs)) => cs,
            None => return Box::new(std::iter::empty()),
            Some(Err(err)) => return Box::new(std::iter::once(Err(StreamError::new(err, 
                            Item::new_string(parent.clone()))))),
        };
        Box::new(RiffleCatIter{parent, outer, inner, filler, state: RiffleCatState::Source})
    }

    fn next_cs(outer: &mut (dyn SIterator + 'node)) -> Option<Result<Box<dyn SIterator<Char> + 'node>, BaseError>> {
        Some(match outer.next()? {
            Err(err) => Err(err.into()),
            Ok(Item::Char(ch)) => Ok(Box::new(std::iter::once(Ok(ch)))),
            Ok(Item::String(s)) => Ok(Box::new(s.into_iter())),
            Ok(item) => Err(format!("expected character or string, found {:?}", item).into())
        })
    }
}

impl Iterator for RiffleCatIter<'_> {
    type Item = Result<Char, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            check_stop!(iter);
            if let Some(ch) = iter_try_expr!(self.inner.next().transpose()) {
                return Some(Ok(ch));
            }
            match std::mem::replace(&mut self.state, RiffleCatState::Source) {
                RiffleCatState::Source => {
                    let next = iter_try_expr!(Self::next_cs(&mut *self.outer)?.map_err(|err|
                            StreamError::new(err, Item::new_string(self.parent.clone()))));
                    self.state = RiffleCatState::Filler{next};
                    self.inner = self.filler.iter();
                },
                RiffleCatState::Filler{next} => {
                    self.state = RiffleCatState::Source;
                    self.inner = next;
                }
            }
        }
    }
}

impl SIterator<Char> for RiffleCatIter<'_> {
    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            n = match self.inner.advance(n) {
                Ok(Some(n)) => n,
                ret => return ret
            };
            if n.is_zero() {
                return Ok(None);
            }
            match std::mem::replace(&mut self.state, RiffleCatState::Source) {
                RiffleCatState::Source => {
                    let next = match Self::next_cs(&mut *self.outer) {
                        None => return Ok(Some(n)),
                        Some(Ok(cs)) => cs,
                        Some(Err(err)) => return Err(StreamError::new(err, 
                                Item::new_string(self.parent.clone())))
                    };
                    self.state = RiffleCatState::Filler{next};
                    self.inner = self.filler.iter();
                },
                RiffleCatState::Filler{next} => {
                    self.state = RiffleCatState::Source;
                    self.inner = next;
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
    fn test_cat() {
        test_eval!("['a', \"b\", \"cde\"].cat" => "\"abcde\"");
        test_eval!("[\"x\", 'y', 1].cat" => "\"xy<!>");
        test_eval!("[\"ab\",\"\",\"\",\"cd\"].cat" => "\"abcd\"");
        test_eval!("\"abc\".chars.cat(\", \")" => "\"a, b, c\"");
        test_eval!("\"abc\".chars.cat(' ')" => "\"a b c\"");
        test_eval!("['a'].repeat.cat(\", \")" => "\"a, a, a, a, a, a, a,...");
        test_eval!("\"abc\".chars.cat(' '.repeat)" => err);
        test_advance("['a', 'b'].cat(' ')");
        test_advance("[\"abcde\"].repeat(10).cat");
        test_advance("[\"abcde\"].repeat.cat(\", \")");
        test_advance("\"abcde\".repeat.chars.cat(' ')");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("cat", Cat::eval, r#"
Concatenates a stream of strings or characters into a single string.
If a `filler` (character or string) is given, it's inserted between each pair of strings.
= stream.?
= stream.?(filler)
> ["Hello", ' ', "world"].? => "Hello world"
> ['a', 'b', 'c'].?(", ") => "a, b, c"
: join
"#);
}
