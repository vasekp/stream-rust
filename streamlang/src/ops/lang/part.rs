use crate::base::*;

fn eval_part(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_source()?;
    node.check_args_nonempty()?;
    eval_enode(&node, env)
}

fn eval_enode(node: &Node<Item>, env: &Env) -> SResult<Item> {
    let source = node
        .source
        .as_ref()
        .expect("source should be nonempty by precondition");
    match node
        .args
        .split_first()
        .expect("args should be nonempty by precondition")
    {
        (Item::Number(index), rest) => {
            let part = match source {
                Item::Stream(stm) => eval_index_impl(&mut RandomAccess::new(Rc::clone(stm)), index)?,
                Item::String(stm) => eval_index_impl(&mut RandomAccess::new(Rc::clone(stm)), index).map(Item::Char)?,
                _ => return Err("expected stream or string".into()),
            };
            eval_rest(node, part, rest, env)
        },
        (Item::Stream(indices), rest) => match source {
            Item::Stream(stm) => Ok(Item::new_stream(Part {
                source: stm.clone(),
                indices: Rc::clone(indices),
                rest: rest.to_owned(),
                env: env.clone(),
                head: node.head.clone(),
            })),
            Item::String(stm) if rest.is_empty() => Ok(Item::new_string(StringPart {
                source: stm.clone(),
                indices: Rc::clone(indices),
                head: node.head.clone(),
            })),
            Item::String(_) => Err("expected only one level of parts".into()),
            _ => Err("expected stream or string".into()),
        },
        _ => Err("expected number or stream".into()),
    }
}

fn eval_rest(node: &Node<Item>, part: Item, rest: &[Item], env: &Env) -> SResult<Item> {
    if rest.is_empty() {
        Ok(part)
    } else {
        let nnode = Node { head: node.head.clone(), source: Some(part), args: rest.to_owned() };
        eval_enode(&nnode, env).map_err(|err| err.wrap(&nnode))
    }
}

fn eval_index_impl<I: ItemType>(cursor: &mut RandomAccess<I>, index: &Number) -> SResult<I> {
    let index = index.try_cast_within(UNumber::one()..)?;
    cursor
        .nth_from_start(index - 1u32)?
        .ok_or_else(|| "index past end of stream".into())
}

struct Part {
    source: Rc<dyn Stream>,
    indices: Rc<dyn Stream>,
    rest: Vec<Item>,
    env: Env,
    head: Head,
}

impl Stream for Part {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        PartIter {
            iter: self.indices.iter(),
            cursor: RandomAccess::new(Rc::clone(&self.source)),
            node: self,
        }
        .wrap()
    }

    fn len(&self) -> Length {
        self.indices.len()
    }
}

impl Describe for Part {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.indices)
            .push_args(&self.rest)
            .finish(prec)
    }
}

struct PartIter {
    node: Rc<Part>,
    iter: Box<dyn SIterator>,
    cursor: RandomAccess,
}

impl PreIterator for PartIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        let index = iter_try!(self.iter.next()).into_num()?;
        let part = eval_index_impl(&mut self.cursor, &index);
        let mut args = self.node.rest.clone();
        args.insert(0, Item::Number(index));
        let node = Node { head: LangItem::Part.into(), source: Some(self.node.source.clone().into()), args };
        part.and_then(|part| eval_rest(&node, part, &self.node.rest, &self.node.env))
            .map(Option::Some)
            .map_err(|err| err.wrap(&node))
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
    }

    fn origin(&self) -> &Rc<Part> {
        &self.node
    }
}

struct StringPart {
    source: Rc<dyn Stream<Char>>,
    indices: Rc<dyn Stream>,
    head: Head,
}

impl Stream<Char> for StringPart {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<Char>> {
        StringPartIter {
            iter: self.indices.iter(),
            cursor: RandomAccess::new(Rc::clone(&self.source)),
            node: self,
        }
        .wrap()
    }

    fn len(&self) -> Length {
        self.indices.len()
    }
}

impl Describe for StringPart {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&*self.indices)
            .finish(prec)
    }
}

struct StringPartIter {
    node: Rc<StringPart>,
    iter: Box<dyn SIterator>,
    cursor: RandomAccess<Char>,
}

impl PreIterator<Char> for StringPartIter {
    fn next(&mut self) -> SResult<Option<Char>> {
        let index = iter_try!(self.iter.next()).into_num()?;
        eval_index_impl(&mut self.cursor, &index).map(Option::Some)
    }

    fn advance(&mut self, n: &UNumber) -> SResult<Option<UNumber>> {
        self.iter.advance(n)
    }

    fn origin(&self) -> &Rc<StringPart> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_raw("[part]", eval_part);
    symbols.insert("part", eval_part, r#"
In its simplest form `stream.?(index)`, evaluates to the `index`-th item in `stream`, counting from 1.
Part specifications can be given to greater depth using multiple arguments.
If an argument is a stream of indices, evaluates to the stream (or string) of the corresponding parts.
The shorthand for `stream.?(arg1, arg2, ...)` is `stream[arg1, arg2, ...]`.
* Items before the requested one are skipped without being evaluated, so an error among them goes unreported.
= stream.?(arg1, arg2, ...)
= string.?(index or stream)
= stream[arg1, arg2, ...]
= string[index or stream]
> ?range(10, 20).?(2) => 11
> ?range(10, 20).?([2, 5]) => [11, 14]
> ?range(10, 20).?(2..5) => [11, 12, 13, 14]
> [[1, 2], ['a', 'b']].?(2, 1) => 'a' ; take second item (['a', 'b']), then first item of that
> "abcde".?(3..4) => "cd"
> (10/?range(-3, 3)).?(4) => !division by zero
> (10/?range(-3, 3)).?(7) => 3 ; items before the index are skipped, not evaluated
> [1, 2, 3].?(4) => !index past end of stream
: range
: first
: last
"#);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part() {
        test_eval!("range(3)[1]" => "1");
        test_eval!("range(3)[3]" => "3");
        test_eval!("range(3)[4]" => err);
        test_eval!("range(3)[10]" => err);
        test_eval!("range(3)[0]" => err);
        test_eval!("range(3)[-1]" => err);
        test_eval!("[[1,2],[3,4]][2,1]" => "3");
        test_eval!("[[1,2],[3,4]][2][1]" => "3");
        test_eval!("[[1,2],[3,4]].part(2,1)" => "3");
        test_eval!("\"abc\"[2]" => "'b'");
        test_eval!("\"abcdef\"[3..5]" => "\"cde\"");

        test_eval!("seq(5,2)[100.repeat]" => "[203, 203, 203, 203, 203, ...]");
        test_eval!("seq(5,2)[2*seq+1]" => "[9, 13, 17, 21, 25, ...]");
        test_eval!("seq[seq][seq]" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("seq[seq, seq]" => "[<!>");
        test_eval!("seq:{seq^#}[seq,4]" => "[4, 16, 64, 256, 1024, ...]");
        test_eval!("seq:{seq^#}[seq][4]" => "[1, 16, 81, 256, 625, ...]");
        test_eval!("seq:{seq^#}[4,seq]" => "[1, 16, 81, 256, 625, ...]");
        test_eval!("seq:{seq^#}[4][seq]" => "[1, 16, 81, 256, 625, ...]");
        test_eval!("seq:{seq^#}[[1,2],[1,2,3]]" => "[[1, 2, 3], [...]]");
        test_eval!("seq[2,5]" => err);
        test_eval!("seq[[2,5]]" => "[2, 5]");
        test_eval!("seq[[[2,5]]]" => "[<!>");
        test_eval!("seq[[2,[5]]]" => "[2, <!>");
        test_eval!("seq[[[2,5]]][1]" => err);
        test_eval!("seq[['a']]" => "[<!>");
        test_eval!("\"abc\"[['a']]" => "\"<!>");
        test_eval!("\"abc\"[['a']][1]" => err);
        test_eval!("\"abc\"[[[1,2]]][1]" => err);
        test_eval!("\"abc\"[[1,'a']][1]" => "'a'");
        test_eval!("\"abc\"[[1,'a']][2]" => err);
        test_len!("seq[[2,5]]" => 2);
        // indices are looked up from the last position, restarting only when going backwards
        test_eval!("seq[[5, 2, 8, 8, 1]]" => "[5, 2, 8, 8, 1]");
        test_eval!("(1..5).while{true}[[3, 9]]" => "[3, <!>");
        test_eval!("(1..5)[[2, 2, 9]]" => "[2, 2, <!>");
        test_eval!("\"abcde\"[[5, 2, 5]]" => "\"ebe\"");
        test_eval!("seq.while{true}[1..6000].countif{true}" => "6000");
        test_len!("seq[[]]" => 0);

        test_describe!("seq[[3]]" => "seq[[3]]");
        test_describe!("seq.part([3])" => "seq.part([3])");
    }
}
