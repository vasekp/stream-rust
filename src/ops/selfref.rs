use crate::base::*;

use std::cell::RefCell;
use std::pin::Pin;

#[derive(Clone)]
struct SelfRef {
    head: Head,
    body: Expr,
    env: Rc<Env>
}

impl SelfRef {
    fn eval(mut node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        try_with!(node, node.check_no_source()?);
        let body = match node.args[..] {
            [ref mut body] => std::mem::take(body),
            _ => return Err(StreamError::new("exactly 1 argument expected", node))
        };
        Ok(Item::Stream(Box::new(SelfRef{head: node.head, body: Self::replace_ref(body), env: Rc::clone(env)})))
    }

    fn eval_real(&self) -> Result<(Box<dyn Stream>, Rc<CacheHistory>), StreamError> {
        let mut env = (*self.env).clone();
        let hist = Rc::new(RefCell::new(Vec::new()));
        env.cache = Rc::downgrade(&hist);
        let item = self.body.clone().eval_env(&Rc::new(env))?;
        let stm = try_with!(self.aux_node(), item.to_stream()?);
        Ok((stm, hist))
    }

    fn aux_node(&self) -> Node {
        Node {
            head: self.head.clone(),
            source: None,
            args: vec![self.body.clone()]
        }
    }

    fn replace_ref(expr: Expr) -> Expr {
        match expr {
            Expr::Imm(_) => expr,
            Expr::Eval(node) => match node.head {
                Head::Repl('%', None) => Expr::new_node(LangItem::BackRef, vec![]),
                _ => Expr::Eval(Node {
                    head: node.head,
                    source: node.source.map(|expr| Box::new(Self::replace_ref(*expr))),
                    args: node.args.into_iter()
                        .map(Self::replace_ref)
                        .collect()
                })
            }
        }
    }
}

impl Describe for SelfRef {
    fn describe(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, None::<&Item>, [&self.body], prec)
    }
}

impl Stream for SelfRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let (stm, hist) = match self.eval_real() {
            Ok((stm, hist)) => (Box::into_pin(stm), hist),
            Err(err) => return Box::new(std::iter::once(Err(err)))
        };
        let iter = unsafe { std::mem::transmute::<&dyn Stream, &dyn Stream>(&*stm) }.iter();
        Box::new(SelfRefIter {
            inner: iter,
            _stm: stm,
            hist
        })
    }

    fn is_string(&self) -> TriState {
        match self.eval_real() {
            Ok((stm, _)) => stm.is_string(),
            Err(_) => TriState::False
        }
    }
}

pub(crate) type CacheHistory = RefCell<Vec<Item>>;

struct SelfRefIter<'node> {
    inner: Box<dyn SIterator + 'node>,
    _stm: Pin<Box<dyn Stream>>,
    hist: Rc<CacheHistory>
}

impl Iterator for SelfRefIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next()? {
            Ok(item) => {
                self.hist.borrow_mut().push(item.clone());
                Some(Ok(item))
            },
            Err(err) => Some(Err(err))
        }
    }
}

impl SIterator for SelfRefIter<'_> { }

#[derive(Clone)]
struct BackRef {
    parent: Weak<CacheHistory>
}

struct BackRefIter {
    vec: Rc<CacheHistory>,
    pos: usize
}

impl BackRef {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        if node.source.is_some() {
            return Err(StreamError::new("no source accepted", node));
        }
        if !node.args.is_empty() {
            return Err(StreamError::new("no arguments accepted", node));
        }
        Ok(Item::Stream(Box::new(BackRef{parent: Weak::clone(&env.cache)})))
    }
}

impl Describe for BackRef {
    fn describe(&self, _: u32) -> String {
        "%".to_owned()
    }
}

impl Stream for BackRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        match Weak::upgrade(&self.parent) {
            Some(rc) => Box::new(BackRefIter{vec: rc, pos: 0}),
            None => Box::new(std::iter::once(Err(StreamError::new("back-reference detached from cache", 
                        Node::new("%", None, vec![])))))
        }
    }

    fn is_string(&self) -> TriState {
        TriState::Either
    }

    fn length(&self) -> Length {
        Length::Unknown
    }
}

impl Iterator for BackRefIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let opos = self.pos;
        self.pos += 1;
        self.vec.borrow().get(opos).cloned().map(Result::Ok)
    }
}

impl SIterator for BackRefIter {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_selfref() {
        use crate::parser::parse;
        assert_eq!(parse("self(%)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("self(%+1)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("self(%.repeat)").unwrap().eval().unwrap().to_string(), "[]");
        assert_eq!(parse("self(1~(%+1))").unwrap().eval().unwrap().to_string(), "[1, 2, 3, 4, 5, ...]");
        assert_eq!(parse("self(0~(1-%))").unwrap().eval().unwrap().to_string(), "[0, 1, 0, 1, 0, ...]");
        assert_eq!(parse("self(1~[%+1])").unwrap().eval().unwrap().to_string(), "[1, [2, [3, ...]]]");
        assert_eq!(parse("self([%])").unwrap().eval().unwrap().to_string(), "[[[[[[...]]]]]]");
        assert_eq!(parse("self([%]~1)[2]").unwrap().eval().unwrap().to_string(), "1");
        assert_eq!(parse("self(seq+(5~%))").unwrap().eval().unwrap().to_string(), "[6, 8, 11, 15, 20, ...]");
        assert_eq!(parse("self(\"pokus\".shift(\"ab\"~%))").unwrap().eval().unwrap().to_string(), "\"qqblu\"");
        assert_eq!(parse("self(%[1])").unwrap().eval().unwrap().to_string(), "[<!>");
        assert_eq!(parse("self(%.len)").unwrap().eval().unwrap().to_string(), "[<!>");
        test_len_exact(&parse("self(%)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("self(%~%)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("self(%:{#})").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("self(%.riffle(%))").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("self(%.repeat)").unwrap().eval().unwrap(), 0);
        test_len_exact(&parse("self(\"pokus\".shift(\"ab\"~%))").unwrap().eval().unwrap(), 5);
        test_skip_n(&parse("self(1~(%+1))").unwrap().eval().unwrap());
        assert_eq!(parse("self(%)").unwrap().eval().unwrap().describe(0), "self(%)");
        assert_eq!(parse("self([%]~1)").unwrap().eval().unwrap().describe(0), "self(([%]~1))");
        assert_eq!(parse("self([%]~1)[2]").unwrap().eval().unwrap().describe(0), "1");

        // Hamming weights
        assert_eq!(parse("'a'.repeat.shift(self(([0,1]~%.skip(2)).riffle(1+%)))").unwrap().eval().unwrap().to_string(), "\"abbcbccdbccdcddebccd...");
        // Thue-Morse
        assert_eq!(parse("'a'.repeat.shift(self(([0,1]~%.skip(2)).riffle(1-%)))").unwrap().eval().unwrap().to_string(), "\"abbabaabbaababbabaab...");
        // Paperfolding sequence
        assert_eq!(parse("'a'.repeat.shift(self([0,1].repeat.riffle(%)))").unwrap().eval().unwrap().to_string(), "\"aabaabbaaabbabbaaaba...");
        // Trailing zeroes
        assert_eq!(parse("'a'.repeat.shift(self(0.repeat.riffle(%+1)))").unwrap().eval().unwrap().to_string(), "\"abacabadabacabaeabac...");
        // Binary length
        assert_eq!(parse("'a'.repeat.shift(self((0~(%+1)).riffle(%+1)))").unwrap().eval().unwrap().to_string(), "\"abbccccddddddddeeeee...");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("self", SelfRef::eval);
    keywords.insert("$backref", BackRef::eval);
}
