use crate::base::*;

fn eval_rising(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = match &node.args[..] {
        [] => None,
        [expr] => Some(Rc::clone(expr.as_func()?)),
        _ => return Err(StreamError::usage(&node.head)),
    };
    Ok(Item::new_stream(Rising{
        head: node.head.clone(),
        source: stm,
        func,
        env: env.clone()
    }))
}

struct Rising {
    head: Head,
    source: Rc<dyn Stream>,
    func: Option<Rc<Node>>,
    env: Env,
}


impl Describe for Rising {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env) // TODO všude, kde je func
            .set_source(&self.source)
            .push_args(self.func.as_deref())
            .finish(prec)
    }
}

impl Stream for Rising {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        RisingIter{iter: self.source.iter(), last: None, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct RisingIter {
    node: Rc<Rising>,
    iter: Box<dyn SIterator>,
    last: Option<Item>
}

impl PreIterator for RisingIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            let key = match &self.node.func {
                None => item.clone(),
                Some(func) => func.with_source(Expr::from(&item))?.eval(&self.node.env)?,
            };
            if let Some(last) = &self.last && key.lex_cmp(last, &self.node.env.alpha)? != std::cmp::Ordering::Greater {
                continue;
            }
            self.last = Some(key);
            return Ok(Some(item));
        }
    }

    fn origin(&self) -> &Rc<Rising> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rising() {
        use super::*;
        test_eval!("seq.rising" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("[].rising" => "[]");
        test_eval!("[1,2,1,2,3,1,2,3,4].rising" => "[1, 2, 3, 4]");
        test_eval!("(2..10).rising{#%5}" => "[2, 3, 4]");
        test_eval!("[\"ahoj\", \"auto\", \"abc\"].rising" => "[\"ahoj\", \"auto\"]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("rising", eval_rising, r#"
Keeps only a subsequence that's strictly increasing, starting from its first element.
If `func` is provided, compares `item.func` instead of `item` itself.
Comparison is done lexicographically, same way as in `?sort`.
= stream.?
= stream.?(func)
> [1, 1, 2, 1, 3].? => [1, 2, 3]
> [[2, 1], [3, 1], [4, 2]].?(?last) : 6 => [[2, 1], [4, 2]]
> ?primes.?windows(2).?{#[2]-#[1]} : 12 => [[2, 3], [3, 5], [7, 11], [23, 29], ...]
: sort
"#);
}
