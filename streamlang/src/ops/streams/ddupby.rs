use crate::base::*;

fn eval_ddupby(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = node.only_arg_checked()?.as_func()?;
    Ok(Item::new_stream(DDupBy{
        head: node.head.clone(),
        source: stm,
        func: Rc::clone(func),
        env: env.clone(),
    }))
}

struct DDupBy {
    head: Head,
    source: Rc<dyn Stream>,
    func: Rc<Node>,
    env: Env,
}


impl Describe for DDupBy {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.func)
            .finish(prec)
    }
}

impl Stream for DDupBy {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        DDupByIter{iter: self.source.iter(), seen: vec![], node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DDupByIter {
    node: Rc<DDupBy>,
    iter: Box<dyn SIterator>,
    seen: Vec<Item>,
}

impl PreIterator for DDupByIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        'a: loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            let key = self.node.func
                .with_source(Expr::from(&item))?
                .eval(&self.node.env)?;
            for seen in &self.seen {
                if key.try_eq(seen)? {
                    continue 'a;
                }
            }
            self.seen.push(key);
            return Ok(Some(item));
        }
    }

    fn origin(&self) -> &Rc<DDupBy> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ddupby() {
        use super::*;
        test_eval!("seq.ddupby{#/5}" => "[1, 5, 10, 15, 20, ...]");
        test_eval!("[1, 2, 4, 5, 6].ddupby{#%3}" => "[1, 2, 6]");
        test_eval!("(1..3).repeat(5).ddupby{#}" => "[1, 2, 3]");
        test_len!("[].ddupby{#+'a'}" => 0);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("ddupby", eval_ddupby, r#"
Keeps only those items of `stream` whose value of `item.func` has not appeared before.
= stream.?(func)
> [1, 2, 4, 5, 6].?{#%3} => [1, 2, 6]
> "Abracadabra".?chars.?(?lcase) => ['A', 'b', 'r', 'c', 'd']
: ddup
: drepby
"#);
}
