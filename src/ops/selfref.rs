use crate::base::*;

use std::cell::RefCell;

#[derive(Clone)]
struct SelfRef {
    head: Head,
    body: Node,
    env: Env
}

impl SelfRef {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        match node.resolve() {
            RNode::NoSource(RNodeNS { head, args: RArgs::One(Expr::Eval(body)) }) =>
                Ok(Item::new_stream(SelfRef{head, body, env: env.clone()})),
            node => Err(StreamError::new("expected: self({body})", node))
        }
    }

    fn eval_real(&self) -> Result<(Box<dyn Stream>, Rc<CacheHistory>), StreamError> {
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
        Node::describe_helper(&self.head, None::<&Item>, [&self.body], prec, env)
    }
}

impl Stream for SelfRef {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        let (stm, hist) = match self.eval_real() {
            Ok((stm, hist)) => (stm, hist),
            Err(err) => return Box::new(std::iter::once(Err(err)))
        };
        Box::new(SelfRefIter {
            inner: stm.into_iter(),
            hist
        })
    }
}

type CacheHistory = RefCell<Vec<Item>>;

struct SelfRefIter {
    inner: OwnedStreamIter,
    hist: Rc<CacheHistory>
}

impl Iterator for SelfRefIter {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = iter_try_expr!(self.inner.next()?);
        self.hist.borrow_mut().push(item.clone());
        Some(Ok(item))
    }
}

impl SIterator for SelfRefIter { }

#[derive(Clone)]
struct BackRef {
    parent: Weak<CacheHistory>
}

struct BackRefIter {
    vec: Rc<CacheHistory>,
    pos: usize
}

impl Describe for BackRef {
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        "#".to_owned()
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
        test_eval!("self{#}" => "[]");
        test_eval!("self{#+1}" => "[]");
        test_eval!("self{#.repeat}" => "[]");
        test_eval!("self{1~(#+1)}" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("self{0~(1-#)}" => "[0, 1, 0, 1, 0, ...]");
        test_eval!("self{1~[#+1]}" => "[1, [2, [3, ...]]]");
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
        test_skip_n("self{1~(#+1)}");
        test_describe!("self{#}" => "self({#})");
        test_describe!("self{[#]~1}" => "self({[#]~1})");
        test_describe!("self{[#]~1}[2]" => "1");

        // Hamming weights
        test_eval!("'a'.repeat+self{([0,1]~#.skip(2)).riffle(1+#)}" => "\"abbcbccdbccdcddebccd...");
        // Thue-Morse
        test_eval!("'a'.repeat+self{([0,1]~#.skip(2)).riffle(1-#)}" => "\"abbabaabbaababbabaab...");
        // Paperfolding sequence
        test_eval!("'a'.repeat+self{[0,1].repeat.riffle(#)}" => "\"aabaabbaaabbabbaaaba...");
        // Trailing zeroes
        test_eval!("'a'.repeat+self{0.repeat.riffle(#+1)}" => "\"abacabadabacabaeabac...");
        // Binary length
        test_eval!("'a'.repeat+self{(0~(#+1)).riffle(#+1)}" => "\"abbccccddddddddeeeee...");
        // Hanoi towers
        test_eval!("self{[12,23,31].repeat.riffle([13,32,21].repeat.riffle(#))}" => "[12, 13, 23, 12, 31, ...]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("self", SelfRef::eval);
}
