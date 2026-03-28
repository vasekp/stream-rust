use crate::base::*;

use std::collections::VecDeque;

fn eval_replace(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    match (node.source_checked()?, &node.args[..]) {
        (Item::String(stm), [orig, repl]) => {
            let (orig, repl) = match (orig, repl) {
                (Item::Char(_) | Item::String(_), Item::Char(_) | Item::String(_))
                    => (vec![orig.to_char_vec()?], vec![repl.to_char_vec()?]),
                (Item::Stream(s1), Item::Stream(s2))
                    => (s1.listout_with(|item| item.to_char_vec())?, s2.listout_with(|item| item.to_char_vec())?),
                _ => return Err(StreamError::usage(&node.head))
            };
            if orig.len() != repl.len() {
                return Err("the replacements lists must be of same length".into());
            }
            if orig.iter().any(Vec::is_empty) {
                return Err("the sought string can't be empty".into());
            }
            let longest = orig.iter().map(Vec::len).reduce(std::cmp::max).unwrap(); // len ≥ 1
            Ok(Item::new_string(StringReplace { head: node.head.clone(), source: Rc::clone(stm), orig, repl, longest }))
        },
        (Item::Stream(stm), [orig, repl]) =>
            Ok(Item::new_stream(StreamReplace { head: node.head.clone(), source: Rc::clone(stm), orig: orig.clone(), repl: repl.clone() })),
        _ => Err(StreamError::usage(&node.head))
    }
}

struct StringReplace {
    head: Head,
    source: Rc<dyn Stream<Char>>,
    orig: Vec<Vec<Char>>,
    repl: Vec<Vec<Char>>,
    longest: usize,
}

impl Describe for StringReplace {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        let orig = Item::from(self.orig.iter().cloned().map(Item::from).collect::<Vec<_>>());
        let repl = Item::from(self.repl.iter().cloned().map(Item::from).collect::<Vec<_>>());
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&orig)
            .push_arg(&repl)
            .finish(prec)
    }
}

impl Stream<Char> for StringReplace {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<Char>> {
        StringReplaceIter::new(self).wrap()
    }

    fn len(&self) -> Length {
        match self.source.len() {
            Length::Exact(_) | Length::AtMost(_) | Length::UnknownFinite => 
                Length::UnknownFinite,
            Length::Infinite | Length::Unknown => Length::Unknown
        }
    }
}

struct StringReplaceIter {
    node: Rc<StringReplace>,
    source: Box<dyn SIterator<Char>>,
    cache: VecDeque<Char>,
    queued: Option<(Box<dyn Iterator<Item = Char>>, bool)>,
}

impl StringReplaceIter {
    fn new(node: Rc<StringReplace>) -> Self {
        StringReplaceIter {
            source: node.source.iter(),
            cache: VecDeque::new(),
            queued: None,
            node
        }
    }
}

impl PreIterator<Char> for StringReplaceIter {
    fn next(&mut self) -> SResult<Option<Char>> {
        loop {
            check_stop!();
            if let Some((deplete, done)) = &mut self.queued {
                if let Some(item) = deplete.next() {
                    return Ok(Some(item));
                } else if *done {
                    return Ok(None);
                } else {
                    self.queued = None;
                }
            }
            if self.cache.len() == self.node.longest
                && let Some(item) = self.cache.pop_front() {
                    return Ok(Some(item));
                }
            if let Some(item) = self.source.next()? {
                self.cache.push_back(item);
                'a: for (patt, repl) in self.node.orig.iter().zip(self.node.repl.iter()) {
                    if patt.len() > self.cache.len() { continue; }
                    /* Match found */
                    let bkpt = self.cache.len() - patt.len();
                    if self.cache.range(bkpt..).eq(patt) {
                        self.cache.truncate(bkpt);
                        self.queued = Some((Box::new(std::mem::take(&mut self.cache).into_iter()
                                .chain(repl.clone().into_iter())), false));
                        break 'a;
                    }
                }
            } else {
                self.queued = Some((Box::new(std::mem::take(&mut self.cache).into_iter()), true));
            }
        }
    }

    fn len_remain(&self) -> Length {
        match self.source.len_remain() {
            Length::Exact(_) | Length::AtMost(_) | Length::UnknownFinite => 
                Length::UnknownFinite,
            Length::Infinite | Length::Unknown => Length::Unknown
        }
    }

    fn origin(&self) -> &Rc<StringReplace> {
        &self.node
    }
}

struct StreamReplace {
    head: Head,
    source: Rc<dyn Stream>,
    orig: Item,
    repl: Item,
}

impl Describe for StreamReplace {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&self.orig)
            .push_arg(&self.repl)
            .finish(prec)
    }
}

impl Stream for StreamReplace {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let orig = self.orig.clone();
        let repl = self.repl.clone();
        Box::new(SMap::new(&self.source,
            move |item| if item.try_eq(&orig)? { Ok(repl.clone()) } else { Ok(item) },
            &self))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_replace() {
        use super::*;
        test_eval!("\"abc\".replace('a', \"a\")" => "\"abc\"");
        test_eval!("\"abc\".replace(['a', 'b'], [\"a\", '1'])" => "\"a1c\"");
        test_eval!("\"abc\".replace(\"\", \"\")" => err);
        test_eval!("\"abc\".replace(['a', 'b'], [\"a\"])" => err);
        test_eval!("\"abcde\".replace('b', \"bb\")" => "\"abbcde\"");
        test_eval!("\"abcde\".replace(['b', 'e'], ['q', \"\"])" => "\"aqcd\"");
        test_eval!("seq.replace(3,[])" => "[1, 2, [], 4, 5, ...]");
        test_eval!("\"abc\".replace()" => err);
        test_eval!("'a'.repeat.replace('b','B')" => "\"aaaaaaaaaaaaaaaaaaaa...");
        test_describe!("\"abc\".replace('a', \"a\")" => "\"abc\".replace([\"a\"], [\"a\"])");
        test_describe!("\"abc\".replace(['a', 'b'], [\"a\", '1'])" => "\"abc\".replace([\"a\", \"b\"], [\"a\", \"1\"])");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("replace", eval_replace, r#"
Replaces occurrences of `patt` by `repl`, or occurrences of `pattM` by `replM`.
For strings, each `patt` and each `repl` may be a character or a substring.
For streams, the latter variant is not available, as `[patt1, ...]` is treated as a valid `patt` itself.
= string.?(patt, repl)
= string.?([patt1, patt2, ...], [repl1, repl2, ...])
= stream.?(patt, repl)
> "abc".?('b', ", ") => "a, c"
> ?seq:{[#]}.?([3], 0) : 7 => [[1], [2], 0, [4], ...]
> "two  spaces".?([" ", "  "], ['1', '2']) => "two11spaces" ; " " is encountered before "  "
: ucase
: lcase
"#);
}
