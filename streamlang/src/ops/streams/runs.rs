use crate::base::*;

fn eval_runs(node: &Node, env: &Env) -> SResult<Item> {
    match (node.source_checked()?.eval(env)?, &node.args[..]) {
        (Item::Stream(stm), []) =>
            Ok(Item::new_stream(Runs{
                head: node.head.clone(),
                source: Rc::clone(&stm),
            })),
        (Item::Stream(stm), [expr]) =>
            Ok(Item::new_stream(RunsBy{
                head: node.head.clone(),
                source: Rc::clone(&stm),
                func: Rc::clone(expr.as_func()?),
                env: env.clone(),
            })),
        (Item::String(stm), [expr]) =>
            Ok(Item::new_stream(RunsBy{
                head: node.head.clone(),
                source: Rc::clone(&stm),
                func: Rc::clone(expr.as_func()?),
                env: env.clone(),
            })),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct Runs {
    head: Head,
    source: Rc<dyn Stream>,
}

impl Describe for Runs {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for Runs {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        RunsIter{iter: self.source.iter(), last: None, done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct RunsIter {
    node: Rc<Runs>,
    iter: Box<dyn SIterator>,
    last: Option<Item>,
    done: bool,
}

impl PreIterator for RunsIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.done {
            return Ok(None);
        }
        let curr_item = match self.last.take() {
            Some(last) => last,
            None => iter_try!(self.iter.next()),
        };
        let mut count = 1usize;
        loop {
            check_stop!();
            match self.iter.next()? {
                Some(item) => if !item.try_eq(&curr_item)? {
                    self.last = Some(item);
                    break;
                },
                None => {
                    self.last = None;
                    self.done = true;
                    break;
                }
            }
            count += 1;
        }
        Ok(Some(Item::new_stream(List::from([curr_item, Item::new_number(count)]))))
    }

    fn origin(&self) -> &Rc<Runs> {
        &self.node
    }
}

struct RunsBy<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    func: Rc<Node>,
    env: Env,
}

impl<I: ItemType> Describe for RunsBy<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.func)
            .finish(prec)
    }
}

impl<I: ItemType> Stream for RunsBy<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let iter = self.source.iter();
        RunsByIter{iter, cached: None, done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct RunsByIter<I: ItemType> {
    node: Rc<RunsBy<I>>,
    iter: Box<dyn SIterator<I>>,
    cached: Option<(Item, I)>,
    done: bool,
}

impl<I: ItemType> PreIterator for RunsByIter<I> {
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
        Ok(Some(Item::from([key, Item::from(vals)])))
    }

    fn origin(&self) -> &Rc<RunsBy<I>> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_runs() {
        use super::*;
        test_eval!("seq.runs" : 9 => "[[1, 1], [2, 1], [3, 1], ...]");
        test_eval!("(seq/5).runs" : 9 => "[[0, 4], [1, 5], [2, 5], ...]");
        test_eval!("(1..3):{1..#}.flatten.runs" : 9 => "[[1, 2], [2, 1], [1, 1], ...]");
        test_len!("[].runs" => 0);

        test_eval!("(1..7).runs{#/3}" : 20 => "[[0, [1, 2]], [1, [3, 4, 5]], [2, [6, 7]]]");
        test_eval!("\"hello world\".runs(isalpha)" : 10 => "[[true, \"hello\"], [false, \" \"], [true, \"world\"]]");
        test_eval!("[].runs{#/3}" => "[]");
        test_eval!("\"\".runs{#/3}" => "[]");
        test_eval!("[1].runs{\"\"}" => "[[\"\", [1]]]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["runs", "reps"], eval_runs, r#"
Replaces every chain of repeated items in `stream` by a pair `[item, count]`.
= stream.?
> "mummy".?chars.? : 15 => [['m', 1], ['u', 1], ['m', 2], ['y', 1]]
: drep
: counts
: consec
: collect
"#);
}
