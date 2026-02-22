use crate::base::*;

use std::collections::VecDeque;

fn eval_windows(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_nth_arg(0, env)?.eval_source(env)?;
    match node {
        RNodeS { source: Item::Stream(_), args: RArgs::One(Expr::Imm(Item::Number(ref size))), .. } => {
            let size = try_with!(node, check_win_size(size)?);
            let Item::Stream(stm) = node.source else { unreachable!() };
            Ok(Item::new_stream(Windows{head: node.head, source: stm.into(), size, body: None, env: env.clone()}))
        },
        RNodeS { source: Item::Stream(_), args: RArgs::Two(Expr::Imm(Item::Number(ref size)), Expr::Eval(_)), .. } => {
            let size = try_with!(node, check_win_size(size)?);
            let RArgs::Two(_, Expr::Eval(body)) = node.args else { unreachable!() };
            let Item::Stream(stm) = node.source else { unreachable!() };
            Ok(Item::new_stream(Windows{head: node.head, source: stm.into(), size, body: Some(body), env: env.clone()}))
        },
        _ => Err(StreamError::new("expected: source.windows(size) or source.windows(size, func)", node))
    }
}

fn check_win_size(size: &Number) -> Result<usize, BaseError> {
    if size.is_negative() {
        return Err("size must be positive".into());
    }
    match size.to_usize() {
        Some(size @ 2..) => Ok(size),
        Some(0..=1) => Err("size must be at least 2".into()),
        _ => Err("size too large".into())
    }
}

#[derive(Clone)]
struct Windows {
    head: Head,
    source: BoxedStream,
    size: usize,
    body: Option<Node>,
    env: Env,
}


impl Describe for Windows {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        if let Some(body) = &self.body {
            DescribeBuilder::new_with_env(&self.head, env, &self.env)
                .set_source(&self.source)
                .push_arg(&self.size)
                .push_arg(body)
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
    fn iter(&self) -> Box<dyn SIterator + '_> {
        let mut iter = self.source.iter();
        let res = (&mut iter).take(self.size - 1)
            .collect::<Result<VecDeque<_>, _>>();
        let deque = match res {
            Ok(deque) => deque,
            Err(err) => return Box::new(std::iter::once(Err(err)))
        };
        Box::new(WindowsIter{iter, size: self.size, deque, body: self.body.as_ref(), env: &self.env})
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
    body: Option<&'node Node>,
    env: &'node Env,
}

impl Iterator for WindowsIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = iter_try_expr!(self.iter.next()?);
        let first = self.deque.pop_front().unwrap(); // for size ≥ 2, deque has ≥ 1 element
        self.deque.push_back(next);
        let iter = std::iter::once(first)
            .chain(self.deque.iter().cloned());
        if let Some(body) = self.body {
            Some(body.clone()
                .with_args(iter.map(Expr::from).collect())
                .and_then(|expr| expr.eval(self.env)))
        } else {
            Some(Ok(Item::from(iter.collect::<Vec<_>>())))
        }
    }
}

impl SIterator for WindowsIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        match n.to_usize() {
            Some(num) if num < self.size => { self.deque.drain(0..num); },
            _ => {
                self.deque.clear();
                if let Some(rem) = self.iter.advance(n - (self.size - 1))? {
                    return Ok(Some(rem + (self.size - 1)));
                }
            }
        }
        for _ in self.deque.len()..(self.size - 1) {
            let Some(next) = self.iter.next().transpose()? else {
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
    symbols.insert_with_docs("windows", eval_windows, r#"
Single-argument form: a stream of `size`-sized arrays (overlapping) from the input `stream`.
Two-argument form: evaluations of the function `func` on the above.
= stream.?(size)
= stream.?(size, {func})
> ?seq.?(3) : 10 => [[1, 2, 3], [2, 3, 4], [3, ...], ...]
> ?seq.?(3, ?plus) => [6, 9, 12, 15, 18, ...]
> [1, 3, 7, 5].?(2, {#2-#1}) => [2, 4, -2] ; differences
"#);
}
