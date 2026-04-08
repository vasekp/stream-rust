use crate::base::*;

fn eval_fold(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = node.only_arg_checked()?.as_func()?;
    Ok(Item::new_stream(Fold{
        head: node.head.clone(),
        source: stm,
        func: Rc::clone(func),
        env: env.clone(),
    }))
}

struct Fold {
    head: Head,
    source: Rc<dyn Stream>,
    func: Rc<Node>,
    env: Env,
}

struct FoldIter {
    node: Rc<Fold>,
    source: Box<dyn SIterator>,
    prev: Option<Item>,
}

impl Describe for Fold {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.func)
            .finish(prec)
    }
}

impl Stream for Fold {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        FoldIter{source: self.source.iter(), prev: None, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

impl PreIterator for FoldIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let next = iter_try!(self.source.next());
        let item = match &self.prev {
            Some(prev) => self.node.func.with_args(vec![prev.into(), next.into()])?
                .eval(&self.node.env)?,
            None => next,
        };
        self.prev = Some(item.clone());
        Ok(Some(item))
    }

    fn len_remain(&self) -> Length {
        self.source.len_remain()
    }

    fn origin(&self) -> &Rc<Fold> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_fold() {
        use super::*;
        test_eval!("seq.fold(plus)" => "[1, 3, 6, 10, 15, ...]");
        test_eval!("seq.fold(times)" => "[1, 2, 6, 24, 120, ...]");
        test_eval!("\"abc\".chars.fold(plus)" => "['a', 'c', 'f']");
        test_eval!("seq.fold{#1~#2}" : 8 => "[1, [1, 2], [1, 2, 3], ...]");
        test_eval!("[].fold{#1~#2}" => "[]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("fold", eval_fold, r#"
A stream `s` where `s[n]` is the result of `func(s[n-1], stream[n])`.
The first element is returned unchanged.
= stream.?{func}
= stream.?(func)
> ?seq.?(plus) => [1, 3, 6, 10, 15, ...] ; partial sums
> ?seq.?(times) => [1, 2, 6, 24, 120, ...] ; factorials
> [1, 2, 1, 3, 2].?(?max) => [1, 2, 2, 3, 3] ; partial maxima
: nest
: reduce
"#);
}
