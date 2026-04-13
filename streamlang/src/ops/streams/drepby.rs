use crate::base::*;

fn eval_drepby(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = node.only_arg_checked()?.as_func()?;
    Ok(Item::new_stream(DRepBy{
        head: node.head.clone(),
        source: stm,
        func: Rc::clone(func),
        env: env.clone(),
    }))
}

struct DRepBy {
    head: Head,
    source: Rc<dyn Stream>,
    func: Rc<Node>,
    env: Env,
}


impl Describe for DRepBy {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.func)
            .finish(prec)
    }
}

impl Stream for DRepBy {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        DRepByIter{iter: self.source.iter(), last: None, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DRepByIter {
    node: Rc<DRepBy>,
    iter: Box<dyn SIterator>,
    last: Option<Item>
}

impl PreIterator for DRepByIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            let key = self.node.func
                .with_source(Expr::from(&item))?
                .eval(&self.node.env)?;
            if let Some(last) = &self.last && last.try_eq(&key)? {
                continue;
            }
            self.last = Some(key);
            return Ok(Some(item));
        }
    }

    fn origin(&self) -> &Rc<DRepBy> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_drepby() {
        use super::*;
        test_eval!("seq.drepby{#/5}" => "[1, 5, 10, 15, 20, ...]");
        test_eval!("[1, 2, 4, 6, 9].drepby{#%3}" => "[1, 2, 4, 6]");
        test_eval!("[1, 2, 5, 6, 9].drepby{#%3}" => "[1, 2, 6]");
        test_len!("[].drepby{#+'a'}" => 0);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("drepby", eval_drepby, r#"
Keeps only the first of every chain of items whose value of `item.func` agrees.
= stream.?(func)
> ["one", "two", "three", "four", "five", "ten"].?(?first) => ["one", "two", "four", "ten"]
: drep
: ddupby
: reps
"#);
}
