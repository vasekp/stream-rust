use crate::base::*;

fn eval_ddup(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = match &node.args[..] {
        [] => None,
        [expr] => Some(Function::new(expr.as_func()?, env)),
        _ => return Err(StreamError::usage(&node.head)),
    };
    Ok(Item::new_stream(DDup { head: node.head.clone(), source: stm, func }))
}

struct DDup {
    head: Head,
    source: Rc<dyn Stream>,
    func: Option<Function>,
}

impl Describe for DDup {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_func(&self.head, env, self.func.as_ref())
            .set_source(&self.source)
            .push_args(&self.func)
            .finish(prec)
    }
}

impl Stream for DDup {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        DDupIter { iter: self.source.iter(), seen: Distinct::default(), node: self }.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

struct DDupIter {
    node: Rc<DDup>,
    iter: Box<dyn SIterator>,
    seen: Distinct,
}

impl PreIterator for DDupIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        loop {
            check_stop!();
            let item = iter_try!(self.iter.next());
            let key = match &self.node.func {
                None => item.clone(),
                Some(func) => func.call_source(item.clone())?,
            };
            if self.seen.insert(&key)?.1 {
                return Ok(Some(item));
            }
        }
    }

    fn origin(&self) -> &Rc<DDup> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["ddup", "uniq"], eval_ddup, r#"
Keeps only the first occurrence of each distinct item of `stream`, preserving order.
If `func` is given, two items are considered equal when `item.func` agrees; the original items are returned, not the keys.
= stream.?
= stream.?(func)
> [1, 2, 1, 2, 3].? => [1, 2, 3]
> [1, 2, 4, 5, 6].?{#%3} => [1, 2, 6]
> "Abracadabra".?chars.? : 6 => ['A', 'b', 'r', 'a', 'c', 'd']
> "Abracadabra".?chars.?(?lcase) => ['A', 'b', 'r', 'c', 'd']
: drep
: counts
"#);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ddup() {
        use super::*;
        test_eval!("seq.ddup" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("(seq/5).ddup" => "[0, 1, 2, 3, 4, ...]");
        test_eval!("(1..3):{1..#}.flatten.ddup" => "[1, 2, 3]");
        test_eval!("(1..3).repeat(5).ddup" => "[1, 2, 3]");
        test_len!("[].ddup" => 0);
        test_describe!("seq.ddup" => "seq.ddup");

        test_eval!("seq.ddup{#/5}" => "[1, 5, 10, 15, 20, ...]");
        test_eval!("[1, 2, 4, 5, 6].ddup{#%3}" => "[1, 2, 6]");
        test_eval!("(1..3).repeat(5).ddup{#}" => "[1, 2, 3]");
        test_len!("[].ddup{#+'a'}" => 0);
        test_describe!("seq.ddup{#}" => "seq.ddup({#})");

        test_eval!("[1,[1],1,[1],\"a\",'a',[1,2],[1,2]].ddup" : 80 => "[1, [1], \"a\", 'a', [1, 2]]");
        test_eval!("range(30000).ddup.len" => "30000");
        test_eval!("range(10^4).repeat(3).ddup.len" => "10000");
        test_eval!("range(30000).ddup{#%7}" => "[1, 2, 3, 4, 5, ...]");
    }
}
