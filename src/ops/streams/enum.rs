use crate::base::*;

fn eval_enum(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let RNodeS{head, source: Item::Stream(stm), args: RArgs::Zero} = rnode else {
        return Err(StreamError::new("expected: stream.enum", rnode));
    };
    Ok(Item::new_stream(Enum{head, stream: stm.into()}))
}

#[derive(Clone)]
struct Enum {
    head: Head,
    stream: BoxedStream,
}


impl Describe for Enum {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.stream)
            .finish(prec)
    }
}

impl Stream for Enum {
    fn iter(&self) -> Box<dyn SIterator + '_> {
        Box::new(EnumIter{iter: self.stream.iter(), index: UNumber::zero()})
    }

    fn len(&self) -> Length {
        self.stream.len()
    }
}

struct EnumIter<'node> {
    iter: Box<dyn SIterator + 'node>,
    index: UNumber
}

impl Iterator for EnumIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = iter_try_expr!(self.iter.next()?);
        self.index.inc();
        Some(Ok(vec![item, Item::new_number(self.index.clone())].into()))
    }
}

impl SIterator for EnumIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
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

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("enum", eval_enum);
}
