use crate::base::*;

#[derive(Clone)]
struct Cat {
    source: BoxedStream,
    head: Head
}

impl Cat {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        match node.eval_all(env)?.resolve_source()? {
            RNodeS { head, source: Item::Stream(stm), args: RArgs::Zero } => {
                Ok(Item::new_string_stream(Cat { source: stm.into(), head }))
            },
            node => Err(StreamError::new("expected: stream.cat", node))
        }
    }
}

impl Describe for Cat {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec, env)
    }
}

impl Stream<Char> for Cat {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        Box::new(CatIter {
            outer: self.source.iter(),
            inner: None
        })
    }

    fn length(&self) -> Length {
        if self.is_empty() {
            Length::Exact(UNumber::zero())
        } else {
            Length::Unknown
        }
    }

    fn is_empty(&self) -> bool {
        self.source.is_empty()
    }
}

struct CatIter<'node> {
    outer: Box<dyn SIterator + 'node>,
    inner: Option<OwnedStreamIter<Char>>,
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
    fn skip_n(&mut self, mut n: UNumber) -> Result<Option<UNumber>, StreamError> {
        loop {
            check_stop!();
            if n.is_zero() {
                return Ok(None);
            }
            if let Some(ref mut iter) = &mut self.inner {
                let Some(m) = iter.skip_n(n)? else { return Ok(None); };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cat() {
        use crate::parser::parse;

        test_eval!("['a', \"b\", \"cde\"].cat" => "\"abcde\"");
        test_eval!("[\"x\", 'y', 1].cat" => "\"xy<!>");
        test_eval!("[\"ab\",\"\",\"\",\"cd\"].cat" => "\"abcd\"");
        test_eval!("\"abc\".chars.riffle(\", \").cat" => "\"a, b, c\"");
        test_eval!("['a', \", \"].repeat.cat" => "\"a, a, a, a, a, a, a,...");
        test_skip_n("[\"abcde\"].repeat(10).cat");
        test_skip_n("[\"a\", \", \"].repeat(10).cat");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("cat", Cat::eval);
}
