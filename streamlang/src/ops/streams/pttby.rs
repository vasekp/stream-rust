use crate::base::*;

fn eval_partitionby(node: &Node, env: &Env) -> SResult<Item> {
    let func = if let [Expr::Eval(body)] = &node.args[..] && body.source.is_none() {
        body
    } else {
        return Err(StreamError::usage(&node.head));
    };
    match node.source_checked()?.eval(env)? {
        Item::Stream(stm) => Ok(Item::new_stream(PartitionBy{
            head: node.head.clone(),
            source: stm,
            func: Rc::clone(func),
            env: env.clone(),
        })),
        Item::String(stm) => Ok(Item::new_stream(PartitionBy{
            head: node.head.clone(),
            source: stm,
            func: Rc::clone(func),
            env: env.clone(),
        })),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct PartitionBy<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    func: Rc<Node>,
    env: Env,
}

impl<I: ItemType> Describe for PartitionBy<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.func)
            .finish(prec)
    }
}

impl<I: ItemType> Stream for PartitionBy<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iter = self.source.iter();
        PartitionByIter{iter, cached: None, done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct PartitionByIter<I: ItemType> {
    node: Rc<PartitionBy<I>>,
    iter: Box<dyn SIterator<I>>,
    cached: Option<(Item, I)>,
    done: bool,
}

impl<I: ItemType> PreIterator for PartitionByIter<I> {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.done {
            return Ok(None);
        }
        let mut vals = Vec::new();
        let key = if let Some((key, val)) = self.cached.take() {
            vals.push(val);
            key
        } else {
            let val = iter_try!(self.iter.next());
            let key = self.node.func
                .with_source(val.to_item().into())?
                .eval(&self.node.env)?;
            vals.push(val);
            key
        };
        while let Some(val) = self.iter.next()? {
            check_stop!();
            let new_key = self.node.func
                .with_source(val.to_item().into())?
                .eval(&self.node.env)?;
            if new_key.try_eq(&key)? {
                vals.push(val);
                continue;
            } else {
                self.cached = Some((new_key, val));
                return Ok(Some(Item::from([key, Item::from(vals)])));
            }
        }
        self.done = true;
        return Ok(Some(Item::from([key, Item::from(vals)])));
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.iter.len_remain())
    }

    fn origin(&self) -> &Rc<PartitionBy<I>> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_partitionby() {
        use super::*;
        test_eval!("(1..7).partitionby{#/3}" : 20 => "[[0, [1, 2]], [1, [3, 4, 5]], [2, [6, 7]]]");
        test_eval!("\"hello world\".partitionby(isalpha)" : 10 => "[[true, \"hello\"], [false, \" \"], [true, \"world\"]]");
        test_eval!("[].partitionby{#/3}" => "[]");
        test_eval!("\"\".partitionby{#/3}" => "[]");
        test_eval!("[1].partitionby{\"\"}" => "[[\"\", [1]]]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["partitionby", "partby", "pttby"], eval_partitionby, r#"
A stream of chunks of consecutive items `x` from `stream` which agree in `x.func`.
Each element is a pair `[value, [keys...]]` with `value` the shared value of `func` of all the `keys`, and the latter in order of appearance in `stream`.
= stream.?(func)
= string.?(func)
> ["test", "one", "two", "three"].?(?first) : 15 => [['t', ["test"]], ['o', ["one"]], ['t', ["two", "three"]]]
> "hello world".?(isalpha) : 10 => [[true, "hello"], [false, " "], [true, "world"]]
: consec
: partition
: groupby
: select
"#);
}
