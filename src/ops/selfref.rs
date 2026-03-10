use crate::base::*;

use std::cell::RefCell;

fn eval_self(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let [Expr::Eval(body)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    let pre = match &node.source {
        None => None,
        Some(source) => Some(source.eval(env)?.to_stream()?),
    };
    Ok(Item::new_stream(SelfRef{
        pre,
        head: node.head.clone(),
        body: Rc::clone(body),
        env: env.clone()
    }))
}


struct SelfRef {
    head: Head,
    body: Rc<Node>,
    env: Env,
    pre: Option<Rc<dyn Stream>>,
}

impl SelfRef {
    fn eval_real(&self) -> Result<(Rc<dyn Stream>, Rc<CacheHistory>), StreamError> {
        let hist = Rc::new(RefCell::new(Vec::new()));
        let stm = self.body.clone()
            .with_source(Expr::new_stream(BackRef {
                parent: Rc::downgrade(&hist)
            }))?
            .eval(&self.env)?
            .to_stream()?;
        Ok((stm, hist))
    }
}

impl Describe for SelfRef {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .push_arg(&*self.body)
            .finish(prec)
    }
}

impl Stream for SelfRef {
    fn iter<'node>(&'node self) -> Result<Box<dyn SIterator + 'node>, StreamError> {
        let (stm, hist) = self.eval_real()?;
        let iter = if let Some(vec) = &self.pre {
            vec.iter()
        } else {
            Box::new(std::iter::empty())
        };
        Ok(Box::new(SelfRefIter {
            pre: iter,
            inner: stm.into_iter(),
            hist,
        }))
    }

    fn len(&self) -> Length {
        Length::Unknown
    }
}

type CacheHistory = RefCell<Vec<Item>>;

struct SelfRefIter<'node> {
    pre: Box<dyn SIterator + 'node>,
    inner: OwnedStreamIter,
    hist: Rc<CacheHistory>,
}

impl SIterator for SelfRefIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        let item = if let Some(item) = self.pre.next()? {
            item.clone()
        } else {
            iter_try!(self.inner.next())
        };
        self.hist.borrow_mut().push(item.clone());
        Ok(Some(item))
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

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
    fn iter<'node>(&'node self) -> Result<Box<dyn SIterator + 'node>, StreamError> {
        match Weak::upgrade(&self.parent) {
            Some(rc) => Ok(Box::new(BackRefIter{vec: rc, pos: 0})),
            None => Err("back-reference detached from cache".into())
        }
    }

    fn len(&self) -> Length {
        Length::Unknown
    }
}

impl SIterator for BackRefIter {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        let opos = self.pos;
        self.pos += 1;
        Ok(self.vec.borrow().get(opos).cloned())
    }

    fn len_remain(&self) -> Length {
        Length::Unknown
    }
}

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
    symbols.insert("self", eval_self, r#"
A stream evaluating `func` on its own output, which is put in place of `#`.
If `stream` is present, uses its items first to populate the history.
= ?{func}
= stream.?{func}
> self{[#]} => [[[[[[...]]]]]]
> [1].self{[#+1]} => [1, [2, [3, ...]]]
: nest
"#);
}
