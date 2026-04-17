use crate::base::*;

fn eval_cat(node: &Node, env: &Env) -> SResult<Item> {
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
        filler: filler.map(LiteralString::from).map(Rc::new),
    }))
}

struct Cat {
    source: Rc<dyn Stream>,
    head: Head,
    filler: Option<Rc<LiteralString>>,
}

impl Describe for Cat {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(self.filler.as_deref())
            .finish(prec)
    }
}

impl Stream<Char> for Cat {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<Char>> {
        match &self.filler {
            None => CatIter::new(self).wrap(),
            Some(fill) => RiffleCatIter::new_boxed(Rc::clone(fill), self)
        }
    }

    fn len(&self) -> Length {
        Length::Unknown
    }
}

struct CatIter {
    node: Rc<Cat>,
    outer: Box<dyn SIterator>,
    inner: Option<Box<dyn SIterator<Char>>>,
}

impl CatIter {
    fn new(node: Rc<Cat>) -> Self {
        CatIter {
            outer: node.source.iter(),
            inner: None,
            node
        }
    }
}

impl PreIterator<Char> for CatIter {
    fn next(&mut self) -> SResult<Option<Char>> {
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
                Item::String(s) => self.inner = Some(s.to_iter()),
                item => return Err(StreamError::with_expr("expected string or character", &item))
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
            if let Some(iter) = &mut self.inner {
                let Some(m) = iter.advance(&n)? else { return Ok(None); };
                self.inner = None;
                n = m;
            } else {
                match self.outer.next()? {
                    Some(Item::String(s)) => self.inner = Some(s.to_iter()),
                    Some(_) => n -= 1,
                    None => return Ok(Some(n))
                }
            }
        }
    }

    fn origin(&self) -> &Rc<Cat> {
        &self.node
    }
}

struct RiffleCatIter {
    node: Rc<Cat>,
    outer: Box<dyn SIterator>,
    inner: Box<dyn SIterator<Char>>,
    filler: Rc<LiteralString>,
    state: RiffleCatState,
}

enum RiffleCatState {
    Source,
    Filler { next: Box<dyn SIterator<Char>> }
}

impl RiffleCatIter {
    fn new_boxed(filler: Rc<LiteralString>, node: Rc<Cat>) -> Box<dyn SIterator<Char>> {
        let mut outer = node.source.iter();
        let inner = match Self::next_cs(&mut *outer) {
            Ok(Some(cs)) => cs,
            Ok(None) => EmptyString::iter(),
            Err(err) => iter_error(err, &node),
        };
        RiffleCatIter{outer, inner, filler, state: RiffleCatState::Source, node}.wrap()
    }

    fn next_cs(outer: &mut dyn SIterator) -> SResult<Option<Box<dyn SIterator<Char>>>> {
        Some(iter_try!(outer.next()).to_char_iter()).transpose()
    }
}

impl PreIterator<Char> for RiffleCatIter {
    fn next(&mut self) -> SResult<Option<Char>> {
        loop {
            check_stop!();
            if let Some(ch) = self.inner.next()? {
                return Ok(Some(ch));
            }
            match std::mem::replace(&mut self.state, RiffleCatState::Source) {
                RiffleCatState::Source => {
                    let next = iter_try!(Self::next_cs(&mut *self.outer));
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

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        let mut n = n.clone();
        loop {
            check_stop!();
            n = match self.inner.advance(&n) {
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
                    self.inner = self.filler.iter();
                },
                RiffleCatState::Filler{next} => {
                    self.state = RiffleCatState::Source;
                    self.inner = next;
                }
            }
        }
    }

    fn origin(&self) -> &Rc<Cat> {
        &self.node
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
        test_eval!("[].cat(' ')" => "\"\"");
        test_len!("[].cat(' ')" => 0);
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
