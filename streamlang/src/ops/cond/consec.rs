use crate::base::*;

fn eval_consec(node: &Node, env: &Env) -> SResult<Item> {
    let func = node.only_arg_checked()?.as_func()?;
    match node.source_checked()?.eval(env)? {
        Item::Stream(stm) => Ok(Item::new_stream(Consec{
            head: node.head.clone(),
            source: stm,
            func: Rc::clone(func),
            env: env.clone(),
        })),
        Item::String(stm) => Ok(Item::new_stream(Consec{
            head: node.head.clone(),
            source: stm,
            func: Rc::clone(func),
            env: env.clone(),
        })),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct Consec<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    func: Rc<Node>,
    env: Env,
}

impl<I: ItemType> Describe for Consec<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.func)
            .finish(prec)
    }
}

impl<I: ItemType> Stream for Consec<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iter = self.source.iter();
        ConsecIter{iter, done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct ConsecIter<I: ItemType> {
    node: Rc<Consec<I>>,
    iter: Box<dyn SIterator<I>>,
    done: bool,
}

impl<I: ItemType> PreIterator for ConsecIter<I> {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.done {
            return Ok(None);
        }
        let mut vals = Vec::new();
        while let Some(val) = self.iter.next()? {
            check_stop!();
            let cond = self.node.func
                .with_source(val.to_item().into())?
                .eval(&self.node.env)?
                .to_bool()?;
            if cond {
                vals.push(val);
            } else if !vals.is_empty() {
                return Ok(Some(Item::from(vals)));
            }
        }
        self.done = true;
        if !vals.is_empty() {
            Ok(Some(Item::from(vals)))
        } else {
            Ok(None)
        }
    }

    fn origin(&self) -> &Rc<Consec<I>> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_consec() {
        use super::*;
        test_eval!("[2, 3, -2, -3, 4].consec{#>0}" : 10 => "[[2, 3], [4]]");
        test_eval!("[2, 3, -2, -3, 4].consec{#<0}" : 10 => "[[-2, -3]]");
        test_eval!("\"123, 45, -12\".consec(isdigit):strnum" => "[123, 45, 12]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("consec", eval_consec, r#"
A stream of chunks of consecutive items `x` from `stream` for which `x.func == true`.
= stream.?(func)
= string.?(func)
> (20..30).?{!#.?isprime} : 15 => [[20, 21, 22], [24, 25, 26, 27, 28], [30]]
> "hello world".?(isalpha) => ["hello", "world"]
: collect
: runs
: select
"#);
}
