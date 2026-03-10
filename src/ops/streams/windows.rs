use crate::base::*;

use std::collections::VecDeque;

fn eval_windows(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let (size, body) = match &node.args[..] {
        [size] => (size, None),
        [size, Expr::Eval(node)] if node.args.is_empty() => (size, Some(Rc::clone(node))),
        _ => return Err(StreamError::usage(&node.head))
    };
    let size = size.eval(env)?.as_num()?.try_cast_within(2usize..)?;
    Ok(Item::new_stream(Windows{head: node.head.clone(), source: stm, size, body, env: env.clone()}))
}

struct Windows {
    head: Head,
    source: Rc<dyn Stream>,
    size: usize,
    body: Option<Rc<Node>>,
    env: Env,
}


impl Describe for Windows {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        if let Some(body) = &self.body {
            DescribeBuilder::new_with_env(&self.head, env, &self.env)
                .set_source(&self.source)
                .push_arg(&self.size)
                .push_arg(&**body)
                .finish(prec)
        } else {
            DescribeBuilder::new(&self.head, env)
                .set_source(&self.source)
                .push_arg(&self.size)
                .finish(prec)
        }
    }
}

impl Stream for Windows {
    fn iter<'node>(&'node self) -> Result<Box<dyn SIterator + 'node>, StreamError> {
        let mut iter = self.source.iter();
        let mut deque = VecDeque::with_capacity(self.size - 1);
        for _ in 0..(self.size - 1) {
            match iter.next()? {
                Some(item) => deque.push_back(item),
                None => return Ok(Box::new(std::iter::empty())),
            }
        }
        Ok(Box::new(WindowsIter{iter, size: self.size, deque, body: self.body.as_ref(), env: &self.env}))
    }

    fn len(&self) -> Length {
        self.source.len().map(|len|
            if len < &UNumber::from(self.size) {
                UNumber::zero()
            } else {
                len - (self.size - 1)
            })
    }
}

struct WindowsIter<'node> {
    iter: Box<dyn SIterator + 'node>,
    size: usize,
    deque: VecDeque<Item>,
    body: Option<&'node Rc<Node>>,
    env: &'node Env,
}

impl SIterator for WindowsIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        let next = iter_try!(self.iter.next());
        let first = self.deque.pop_front().unwrap(); // for size ≥ 2, deque has ≥ 1 element
        self.deque.push_back(next);
        let iter = std::iter::once(first)
            .chain(self.deque.iter().cloned());
        if let Some(body) = self.body {
            body.with_args(iter.map(Expr::from).collect())
                .and_then(|expr| expr.eval(self.env))
                .map(Option::Some)
        } else {
            Ok(Some(Item::from(iter.collect::<Vec<_>>())))
        }
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        match (&n).try_into() {
            Ok(num) if num < self.size => { self.deque.drain(0..num); },
            _ => {
                self.deque.clear();
                if let Some(rem) = self.iter.advance(n - (self.size - 1))? {
                    return Ok(Some(rem + (self.size - 1)));
                }
            }
        }
        for _ in self.deque.len()..(self.size - 1) {
            let Some(next) = self.iter.next()? else {
                return Ok(Some((self.size - 1 - self.deque.len()).into()));
            };
            self.deque.push_back(next);
        }
        Ok(None)
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_windows() {
        use super::*;
        test_eval!("seq.windows(3)" : 10 => "[[1, 2, 3], [2, 3, 4], [3, ...], ...]");
        test_eval!("seq.windows(2)" : 10 => "[[1, 2], [2, 3], [3, 4], [...], ...]");
        test_eval!("seq.windows(1)" => err);
        test_eval!("seq.windows(0)" => err);
        test_eval!("seq.windows(-1)" => err);
        test_len!("(1..10).windows(5)" => 6);
        test_len!("(1..10).windows(9)" => 2);
        test_len!("(1..10).windows(10)" => 1);
        test_eval!("(1..10).windows(11)" => "[]");
        test_eval!("(1..10).$lenUU.windows(11)" => "[]");
        test_eval!("(1..5).windows(4)[1]" => "[1, 2, 3, 4]");
        test_eval!("(1..5).windows(4)[2]" => "[2, 3, 4, 5]");
        test_eval!("(1..5).windows(4)[3]" => err);
        test_advance("(1..(10^10)).windows(5)");
        test_eval!("seq.windows(3, plus)" => "[6, 9, 12, 15, 18, ...]");
        test_eval!("(seq^2).windows(3, {#2-#1})" => "[3, 5, 7, 9, 11, ...]");
        test_describe!("seq.windows(3)" => "seq.windows(3)");
        test_describe!("alpha(\"abc\",seq.windows(2, {#1+#2}))" => "alpha(['a', 'b', 'c'], seq.windows(2, {#1+#2}))");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("windows", eval_windows, r#"
Single-argument form: a stream of `size`-sized arrays (overlapping) from the input `stream`.
Two-argument form: evaluations of the function `func` on the above, entering as arguments (`#1, #2, ...`).
= stream.?(size)
= stream.?(size, {func})
> ?seq.?(3) : 10 => [[1, 2, 3], [2, 3, 4], [3, ...], ...]
> ?seq.?(3, ?plus) => [6, 9, 12, 15, 18, ...]
> [1, 3, 7, 5].?(2, {#2-#1}) => [2, 4, -2] ; differences
"#);
}
