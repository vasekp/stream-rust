use crate::base::*;

fn eval_stride(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_stream()?;
    let (stride, offset) = match &node.args[..] {
        [Item::Number(stride), Item::Number(offset)]
            => (stride, Some(offset.try_unsign()?)),
        [Item::Number(stride)] => (stride, None),
        _ => return Err(StreamError::usage(&node.head)),
    };
    let stride = stride.try_cast_within(UNumber::one()..)?;
    Ok(Item::new_stream(Stride{
        head: node.head.clone(),
        stream: Rc::clone(stm),
        stride, offset,
    }))
}

struct Stride {
    head: Head,
    stream: Rc<dyn Stream>,
    stride: UNumber,
    offset: Option<UNumber>,
}

impl Describe for Stride {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.stream)
            .push_arg(&self.stride)
            .push_args(&self.offset)
            .finish(prec)
    }
}

impl Stream for Stride {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let mut iter = self.stream.iter();
        if let Some(n) = &self.offset {
            match iter.advance(n.clone()) {
                Ok(None) => (),
                Ok(Some(_)) => return Box::new(std::iter::empty()),
                Err(err) => return iter_error(err, &self),
            };
        }
        StrideIter{
            iter,
            first: true,
            node: self
        }.wrap()
    }

    fn len(&self) -> Length {
        self.stream.len()
            .map(|len| (len + &self.stride - self.offset.as_ref().cloned().unwrap_or_default() - 1) / &self.stride)
    }
}

struct StrideIter {
    node: Rc<Stride>,
    iter: Box<dyn SIterator>,
    first: bool,
}

impl PreIterator for StrideIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if !self.first {
            if self.iter.advance(&self.node.stride - 1)?.is_some() {
                return Ok(None);
            }
        } else {
            self.first = false;
        }
        self.iter.next()
    }

    fn advance(&mut self, mut n: UNumber) -> SResult<Option<UNumber>> {
        if n.is_zero() { return Ok(None); }
        if self.first {
            if self.iter.next()?.is_none() {
                return Ok(Some(n));
            } else {
                self.first = false;
                n -= 1;
            }
        }
        match self.iter.advance(&n * &self.node.stride)? {
            Some(remain) =>
                Ok(Some((remain + &self.node.stride - 1) / &self.node.stride)),
            None => Ok(None)
        }
    }

    fn len_remain(&self) -> Length {
        if self.first {
            self.iter.len_remain()
                .map(|len| (len + &self.node.stride - 1) / &self.node.stride)
        } else {
            self.iter.len_remain().map(|len| len / &self.node.stride)
        }
    }

    fn origin(&self) -> &Rc<Stride> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stride() {
        test_eval!("(1..5).stride(2)" => "[1, 3, 5]");
        test_eval!("(1..5).stride(2, -1)" => err);
        test_eval!("(1..5).stride(2, 0)" => "[1, 3, 5]");
        test_eval!("(1..5).stride(2, 1)" => "[2, 4]");
        test_eval!("(1..5).stride(2, 4)" => "[5]");
        test_eval!("(1..5).stride(2, 5)" => "[]");
        test_eval!("(1..5).stride(2, 6)" => "[]");
        test_eval!("(1..5).stride(5)" => "[1]");
        test_eval!("(1..5).stride(5,0)" => "[1]");
        test_eval!("(1..5).stride(5,1)" => "[2]");
        test_eval!("(1..5).stride(5,5)" => "[]");
        test_eval!("seq.stride(10^6, 10^4)" : 3 => "[10001, 1010001, 2010001, ...]");
        test_len!("(1..5).stride(2)" => 3);
        test_len!("(1..5).stride(2, 1)" => 2);
        test_len!("(1..5).stride(2, 4)" => 1);
        test_len!("(1..5).stride(2, 5)" => 0);
        test_len!("(1..5).stride(2, 6)" => 0);
        test_advance("(1..10).stride(6)");
        test_advance("seq.stride(10^6, 10^4)");
        test_advance("(1..10^10).stride(10^6)");
        test_advance("(1..10^10).stride(10^6, 10^4)");
        test_advance("(1..10^10).stride(10^6, 10^6 - 1)");
        test_advance("(1..10^10).stride(10^6, 10^10)");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["stride", "step"], eval_stride, r#"
Extracts a subsequence of `stream` at indices `1`, `1 + step`, `1 + 2*step`, etc.
If `offset` is specified, the subsequence is shifted by it.
= stream.?(step)
= stream.?(step, offset)
> ?seq.?(5) => [1, 6, 11, 16, 21, ...]
> ?seq.?(5, 2) => [3, 8, 13, 18, 23, ...]
: part
"#);
}
