use crate::base::*;

use std::cell::RefCell;

struct SelfRef {
    head: Head,
    body: Node,
    env: Env,
    pre: Option<Rc<dyn Stream>>,
}

impl SelfRef {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        match node.resolve() {
            RNode::NoSource(RNodeNS { head, args: RArgs::One(Expr::Eval(body)) }) =>
                Ok(Item::new_stream(SelfRef{
                    pre: None, head, body, env: env.clone()})),
            RNode::Source(RNodeS { source, head, args: RArgs::One(Expr::Eval(body)) }) => {
                let Item::Stream(stm) = source.eval(env)? else { todo!() };
                Ok(Item::new_stream(SelfRef{
                    pre: Some(stm), head, body, env: env.clone()}))
            },
            node => Err(StreamError::new("expected: self({body})", node))
        }
    }

    fn eval_real(&self) -> Result<(Rc<dyn Stream>, Rc<CacheHistory>), StreamError> {
        let hist = Rc::new(RefCell::new(Vec::new()));
        let item = self.body.clone()
            .with_source(Expr::new_stream(BackRef {
                parent: Rc::downgrade(&hist)
            }))?
            .eval(&self.env)?;
        let stm = match item {
            Item::Stream(stm) => stm,
            _ => return Err(StreamError::new(format!("expected stream, found {:?}", item), 
                self.body.clone()))
        };
        Ok((stm, hist))
    }
}

impl Describe for SelfRef {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .push_arg(&self.body)
            .finish(prec)
    }
}

impl Stream for SelfRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let (stm, hist) = match self.eval_real() {
            Ok((stm, hist)) => (stm, hist),
            Err(err) => return Box::new(std::iter::once(Err(err)))
        };
        let iter = if let Some(vec) = &self.pre { vec.iter() } else { EmptyStream.iter() };
        Box::new(SelfRefIter {
            pre: iter,
            inner: stm.into_iter(),
            hist,
        })
    }
}

type CacheHistory = RefCell<Vec<Item>>;

struct SelfRefIter<'node> {
    pre: Box<dyn SIterator + 'node>,
    inner: OwnedStreamIter,
    hist: Rc<CacheHistory>,
}

impl Iterator for SelfRefIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = if let Some(item) = iter_try_expr!(self.pre.next().transpose()) {
            item.clone()
        } else {
            iter_try_expr!(self.inner.next()?)
        };
        self.hist.borrow_mut().push(item.clone());
        Some(Ok(item))
    }
}

impl SIterator for SelfRefIter<'_> { }

struct BackRef {
    parent: Weak<CacheHistory>
}

struct BackRefIter {
    vec: Rc<CacheHistory>,
    pos: usize
}

impl Describe for BackRef {
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        "[self]".to_owned()
    }
}

impl Stream for BackRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        match Weak::upgrade(&self.parent) {
            Some(rc) => Box::new(BackRefIter{vec: rc, pos: 0}),
            None => Box::new(std::iter::once(Err(StreamError::new("back-reference detached from cache", 
                        Node::new("#", None, vec![])))))
        }
    }

    fn len(&self) -> Length {
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
        test_eval!("self{#}" => "[]");
        test_eval!("self{#+1}" => "[]");
        test_eval!("self{#.repeat}" => "[]");
        test_eval!("self{1~(#+1)}" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("[1].self{#+1}" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("self{0~(1-#)}" => "[0, 1, 0, 1, 0, ...]");
        test_eval!("self{1~[#+1]}" => "[1, [2, [3, ...]]]");
        test_eval!("[1].self{[#+1]}" => "[1, [2, [3, ...]]]");
        test_eval!("self{[#]}" => "[[[[[[...]]]]]]");
        test_eval!("self{[#]~1}[2]" => "1");
        test_eval!("self{seq+(5~#)}" => "[6, 8, 11, 15, 20, ...]");
        test_eval!("self{\"pokus\".chars+(\"ab\".chars~#)}.string" => "\"qqblu\"");
        test_eval!("self{#[1]}" => "[<!>");
        test_eval!("self{#.len}" => "[<!>");
        test_eval!("1.{#~self{0~#}}" => "[1, 0, 0, 0, 0, ...]");
        test_len!("self{#}" => 0);
        test_len!("self{#~#}" => 0);
        test_len!("self{#:{#}}" => 0);
        test_len!("self{#.riffle(#)}" => 0);
        test_len!("self{#.repeat}" => 0);
        test_len!("self{\"pokus\".chars+(\"ab\".chars~#)}" => 5);
        test_advance("self{1~(#+1)}");
        test_describe!("self{#}" => "self({#})");
        test_describe!("self{[#]~1}" => "self({[#]~1})");
        test_describe!("self{[#]~1}[2]" => "1");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("self", SelfRef::eval, r#"
A stream evaluating `func` on its own output, which is put in place of `#`.
If `stream` is present, uses its items first to populate the history.
= ?{func}
= stream.?{func}
> self{[#]} => [[[[[[...]]]]]]
> [1].self{[#+1]} => [1, [2, [3, ...]]]
: nest
"#);
}
