use crate::base::*;
use super::join::{Joinable, JoinIter};

fn eval_prepend(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    match node.source_checked()? {
        Item::Stream(stm) => {
            let elems = node.args.iter()
                .cloned()
                .map(Joinable::Single)
                .collect::<Vec<_>>();
            Ok(Item::new_stream(Prepend{source: Rc::clone(stm), elems, head: node.head}))
        },
        Item::String(stm) => {
            let elems = node.args.iter()
                .cloned()
                .map(|item| match item {
                    Item::Char(ch) => Joinable::Single(ch),
                    Item::String(stm) => Joinable::Stream(stm),
                    _ => unreachable!()
                })
                .collect::<Vec<_>>();
            Ok(Item::new_string(Prepend{source: Rc::clone(stm), elems, head: node.head}))
        },
        _ => Err(StreamError::usage(&node.head)),
    }
}

struct Prepend<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    elems: Vec<Joinable<I>>
}

impl<I: ItemType> Describe for Prepend<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.elems)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for Prepend<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        let mut iters = Vec::with_capacity(1 + self.elems.len());
        for elem in &self.elems {
            match elem {
                Joinable::Stream(stm) => iters.push(stm.iter()),
                Joinable::Single(item) => iters.push(Box::new(std::iter::once(Ok(item.clone())))),
            }
        }
        iters.push(self.source.iter());
        JoinIter{iters, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.elems.iter()
            .map(|item| match item {
                Joinable::Single(_) => Length::Exact(UNumber::one()),
                Joinable::Stream(stm) => stm.len()
            })
            .fold(self.source.len(), |acc, e| acc + e)
    }
}

fn eval_append(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    match node.source_checked()? {
        Item::Stream(stm) => {
            let elems = node.args.iter()
                .cloned()
                .map(Joinable::Single)
                .collect::<Vec<_>>();
            Ok(Item::new_stream(Append{source: Rc::clone(stm), elems, head: node.head}))
        },
        Item::String(stm) => {
            let elems = node.args.iter()
                .cloned()
                .map(|item| match item {
                    Item::Char(ch) => Joinable::Single(ch),
                    Item::String(stm) => Joinable::Stream(stm),
                    _ => unreachable!()
                })
                .collect::<Vec<_>>();
            Ok(Item::new_string(Append{source: Rc::clone(stm), elems, head: node.head}))
        },
        _ => Err(StreamError::usage(&node.head)),
    }
}

struct Append<I: ItemType> {
    head: Head,
    source: Rc<dyn Stream<I>>,
    elems: Vec<Joinable<I>>
}

impl<I: ItemType> Describe for Append<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.elems)
            .finish(prec)
    }
}

impl<I: ItemType> Stream<I> for Append<I> {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<I>> {
        let mut iters = Vec::with_capacity(1 + self.elems.len());
        iters.push(self.source.iter());
        for elem in &self.elems {
            match elem {
                Joinable::Stream(stm) => iters.push(stm.iter()),
                Joinable::Single(item) => iters.push(Box::new(std::iter::once(Ok(item.clone())))),
            }
        }
        JoinIter{iters, node: self}.wrap()
    }

    fn len(&self) -> Length {
        self.elems.iter()
            .map(|item| match item {
                Joinable::Single(_) => Length::Exact(UNumber::one()),
                Joinable::Stream(stm) => stm.len()
            })
            .fold(self.source.len(), |acc, e| acc + e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prepend() {
        test_eval!("(1..3).prepend(5)" => "[5, 1, 2, 3]");
        test_eval!("(1..3).prepend(5, 0)" => "[5, 0, 1, 2, 3]");
        test_eval!("seq.prepend(5)" => "[5, 1, 2, 3, 4, ...]");
        test_eval!("seq.prepend(seq)" => "[[1, 2, 3, 4, ...], ...]");
        test_eval!("\"test\".prepend(\"This is\", ' ', \"\", \"a \")" => "\"This is a test\"");
        test_advance("(1..10^5).prepend@(1..10)");
        test_advance("\"abcde\".prepend(\"f\", \"\", \"\", 'g')");
    }

    #[test]
    fn test_append() {
        test_eval!("(1..3).append(5)" => "[1, 2, 3, 5]");
        test_eval!("(1..3).append(5, 0)" => "[1, 2, 3, 5, 0]");
        test_eval!("seq.append(5)" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("[5].append(seq)" => "[5, [1, 2, 3, ...]]");
        test_eval!("\"This is\".append(' ', \"a \", \"\", \"test\")" => "\"This is a test\"");
        test_advance("(1..10^5).append@(1..10)");
        test_advance("\"abcde\".append(\"f\", \"\", \"\", 'g')");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("prepend", eval_prepend, r#"
Prepends `item`s (in order of the arguments) to `stream` or `string`.
In the case of `string` source, the `items` may be characters or strings.
This is equivalent to `join` or `cat`, but better suited for chaining.
= stream.?(item, ...)
= string.?(item, ...)
> (1..3).?(5) => [5, 1, 2, 3]
> "one".?("1:") => "1:one"
: append
: join
: cat
: replace
: remove
"#);
    symbols.insert("append", eval_append, r#"
Appends `item`s (in order of the arguments) to `stream` or `string`.
In the case of `string` source, the `items` may be characters or strings.
This is equivalent to `join` or `cat`, but better suited for chaining.
= stream.?(item, ...)
= string.?(item, ...)
> (1..3).?(5) => [1, 2, 3, 5]
> "1".?(':',"one") => "1:one"
: prepend
: join
: cat
: replace
: remove
"#);
}
