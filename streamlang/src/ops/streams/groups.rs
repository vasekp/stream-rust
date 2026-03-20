use crate::base::*;

fn eval_groups(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let (size, body) = match &node.args[..] {
        [size] => (size, None),
        [size, Expr::Eval(node)] if node.args.is_empty() => (size, Some(Rc::clone(node))),
        _ => return Err(StreamError::usage(&node.head))
    };
    let size = size.eval(env)?.as_num()?.try_cast_within(1usize..)?;
    Ok(Item::new_stream(Groups{head: node.head.clone(), source: stm, size, body, env: env.clone()}))
}

struct Groups {
    head: Head,
    source: Rc<dyn Stream>,
    size: usize,
    body: Option<Rc<Node>>,
    env: Env,
}

impl Describe for Groups {
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

impl Stream for Groups {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iter = self.source.iter();
        GroupsIter{size: self.size, iter, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.source.len().map(|len| len / self.size)
    }
}

struct GroupsIter {
    node: Rc<Groups>,
    iter: Box<dyn SIterator>,
    size: usize,
}

impl PreIterator for GroupsIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let mut vec = Vec::with_capacity(self.size);
        for _ in 0..self.size {
            vec.push(iter_try!(self.iter.next()));
        }
        if let Some(body) = &self.node.body {
            body.with_args(vec.into_iter().map(Expr::from).collect())
                .and_then(|expr| expr.eval(&self.node.env))
                .map(Option::Some)
        } else {
            Ok(Some(Item::from(vec)))
        }
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        let mul = &n * UNumber::from(self.size);
        match self.iter.advance(mul.clone())? {
            Some(rem) => Ok(Some(n - (mul - rem) / self.size)),
            None => Ok(None)
        }
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain().map(|len| len / self.size)
    }

    fn origin(&self) -> &Rc<Groups> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_groups() {
        use super::*;
        test_eval!("(1..7).groups(3)" : 10 => "[[1, 2, 3], [4, 5, 6]]");
        test_eval!("seq.groups(3)" : 10 => "[[1, 2, 3], [4, 5, 6], [7, ...], ...]");
        test_eval!("seq.groups(2)" : 10 => "[[1, 2], [3, 4], [5, 6], [...], ...]");
        test_eval!("seq.groups(1)" : 10 => "[[1], [2], [3], [4], [5], ...]");
        test_eval!("seq.groups(0)" => err);
        test_len!("(1..10).groups(3)" => 3);
        test_len!("(1..10).groups(5)" => 2);
        test_len!("(1..10).groups(9)" => 1);
        test_len!("(1..10).groups(10)" => 1);
        test_eval!("(1..10).groups(11)" => "[]");
        test_eval!("(1..10).$lenUU.groups(11)" => "[]");
        test_eval!("(1..5).groups(4)[1]" => "[1, 2, 3, 4]");
        test_eval!("(1..5).groups(4)[2]" => err);
        test_advance("(1..(10^5)).groups(3)");
        test_advance("(1..(10^5)).groups(5)");
        test_eval!("seq.groups(3, plus)" => "[6, 15, 24, 33, 42, ...]");
        test_describe!("seq.groups(3)" => "seq.groups(3)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["groups", "group"], eval_groups, r#"
Single-argument form: a stream of `size`-sized non-overlapping arrays from the input `stream`.
Two-argument form: evaluations of the function `func` on the above, entering as arguments (`#1, #2, ...`).
= stream.?(size)
= stream.?(size, {func})
> ?seq.?(3) : 10 => [[1, 2, 3], [4, 5, 6], [7, ...], ...]
> ?seq.?(3, ?plus) => [6, 15, 24, 33, 42, ...]
> (1..5).?(2) : 10 => [[1, 2], [3, 4]] ; an incomplete trailing group is discarded
: windows
"#);
}
