use crate::base::*;

fn eval_enum(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_stream()?;
    Ok(Item::new_stream(Enum{head: node.head.clone(), stream: stm}))
}

struct Enum {
    head: Head,
    stream: Rc<dyn Stream>,
}


impl Describe for Enum {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.stream)
            .finish(prec)
    }
}

impl Stream for Enum {
    fn iter(&self) -> SResult<Box<dyn SIterator + '_>> {
        Ok(Box::new(EnumIter{iter: self.stream.iter(), index: UNumber::zero()}))
    }

    fn len(&self) -> Length {
        self.stream.len()
    }
}

struct EnumIter<'node> {
    iter: Box<dyn SIterator + 'node>,
    index: UNumber
}

impl SIterator for EnumIter<'_> {
    fn next(&mut self) -> SResult<Option<Item>> {
        let item = iter_try!(self.iter.next());
        self.index += 1;
        Ok(Some(vec![item, Item::new_number(self.index.clone())].into()))
    }

    fn advance(&mut self, n: UNumber) -> SResult<Option<UNumber>> {
        self.index += &n;
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_riffle() {
        use super::*;
        test_eval!("\"abc\".chars.enum" => "[['a', 1], ['b', ...], ...]");
        test_eval!("\"abc\".enum" => err);
        test_advance("seq.enum");
        test_advance("(1..(10^20)).enum");
        test_advance("[].enum");
        test_describe!("seq.enum" => "seq.enum");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("enum", eval_enum, r#"
Accompanies every item in `stream` with its position in it.
* Equivalent to `stream.?zip(?seq)`.
= stream.?
> ["one", "two", "three"].? : 10 => [["one", 1], ["two", 2], ["three", 3]]
: index
"#);
}
