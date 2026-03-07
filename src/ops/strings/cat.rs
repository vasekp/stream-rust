use crate::base::*;

fn eval_cat(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.to_stream()?;
    let filler = match &node.args[..] {
        [] => None,
        [Item::String(fill)] => Some(fill.listout()?),
        [Item::Char(fill)] => Some(vec![*fill]),
        _ => return Err(StreamError::usage(&node.head))
    };
    Ok(Item::new_string(Cat {
        source: stm,
        head: node.head.clone(),
        filler: filler.map(LiteralString::from),
    }))
}

struct Cat {
    source: Rc<dyn Stream>,
    head: Head,
    filler: Option<LiteralString>,
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
    fn iter0<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
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

impl SIterator<Char> for CatIter<'_> {
    fn next(&mut self) -> Result<Option<Char>, StreamError> {
        loop {
            check_stop!();
            if let Some(iter) = &mut self.inner {
                match iter.next()? {
                    Some(ch) => return Ok(Some(ch)),
                    None => self.inner = None
                }
            }
            match iter_try!(self.outer.next()) {
                Item::Char(ch) => return Ok(Some(ch)),
                Item::String(s) => self.inner = Some(s.into_iter()),
                _item => return Err(StreamError::new0("expected string or character")) // TODO decorate
            }
        }
    }

    fn advance(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            if let Some(iter) = &mut self.inner {
                let Some(m) = iter.advance(n)? else { return Ok(None); };
                self.inner = None;
                n = m;
            } else {
                match self.outer.next()? {
                    Some(Item::String(s)) => self.inner = Some(s.into_iter()),
                    Some(_) => n -= 1,
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
    _parent: &'node Cat,
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
            Ok(Some(cs)) => cs,
            Ok(None) => return Box::new(std::iter::empty()),
            Err(err) => return Box::new(std::iter::once(Err(err))),
        };
        Box::new(RiffleCatIter{_parent: parent, outer, inner, filler, state: RiffleCatState::Source})
    }

    fn next_cs(outer: &mut (dyn SIterator + 'node)) -> Result<Option<Box<dyn SIterator<Char> + 'node>>, StreamError> {
        match outer.next()? {
            Some(Item::Char(ch)) => Ok(Some(Box::new(std::iter::once(Ok(ch))))),
            Some(Item::String(s)) => Ok(Some(Box::new(s.into_iter()))),
            None => Ok(None),
            Some(_) => Err(StreamError::new0("expected character or string")),
        }
    }
}

impl SIterator<Char> for RiffleCatIter<'_> {
    fn next(&mut self) -> Result<Option<Char>, StreamError> {
        loop {
            check_stop!();
            if let Some(ch) = self.inner.next()? {
                return Ok(Some(ch));
            }
            match std::mem::replace(&mut self.state, RiffleCatState::Source) {
                RiffleCatState::Source => {
                    let next = iter_try!(Self::next_cs(&mut *self.outer));
                    self.state = RiffleCatState::Filler{next};
                    self.inner = self.filler.iter0();
                },
                RiffleCatState::Filler{next} => {
                    self.state = RiffleCatState::Source;
                    self.inner = next;
                }
            }
        }
    }

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
                    let next = match Self::next_cs(&mut *self.outer)? {
                        None => return Ok(Some(n)),
                        Some(cs) => cs,
                    };
                    self.state = RiffleCatState::Filler{next};
                    self.inner = self.filler.iter0();
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
    symbols.insert("cat", eval_cat, r#"
Concatenates a stream of strings or characters into a single string.
If a `filler` (character or string) is given, it's inserted between each pair of strings.
= stream.?
= stream.?(filler)
> ["Hello", ' ', "world"].? => "Hello world"
> ['a', 'b', 'c'].?(", ") => "a, b, c"
: join
: riffle
"#);
}
