use crate::base::*;

use std::collections::VecDeque;

fn eval_replace(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    match (node.source_checked()?, &node.args[..]) {
        (Item::String(stm), [x, y]) => {
            let (orig, repl) = match (x, y) {
                (Item::Char(c1), Item::Char(c2)) => (vec![vec![*c1]], vec![vec![*c2]]),
                (Item::Char(c1), Item::String(s2)) => (vec![vec![*c1]], vec![s2.listout()?]),
                (Item::String(s1), Item::Char(c2)) => (vec![s1.listout()?], vec![vec![*c2]]),
                (Item::String(s1), Item::String(s2)) => (vec![s1.listout()?], vec![s2.listout()?]),
                (Item::Stream(s1), Item::Stream(s2)) => (read_stream(s1)?, read_stream(s2)?),
                _ => return Err(StreamError::usage(&node.head))
            };
            if orig.len() != repl.len() {
                return Err(StreamError::new0("the replacements lists must be of same length"));
            }
            if orig.iter().any(Vec::is_empty) {
                return Err(StreamError::new0("the sought string can't be empty"));
            }
            let longest = orig.iter().map(Vec::len).reduce(std::cmp::max).unwrap(); // len ≥ 1
            Ok(Item::new_string(StringReplace { head: node.head.clone(), source: Rc::clone(stm), orig, repl, longest }))
        },
        (Item::Stream(stm), [orig, repl]) =>
            Ok(Item::new_stream(StreamReplace { head: node.head.clone(), source: Rc::clone(stm), orig: orig.clone(), repl: repl.clone() })),
        _ => Err(StreamError::usage(&node.head))
    }
}

fn read_stream(stm: &Rc<dyn Stream>) -> Result<Vec<Vec<Char>>, StreamError> {
    stm.iter().transposed()
        .map(|item| {
            check_stop!();
            match item? {
                Item::Char(ch) => Ok(vec![ch]),
                Item::String(s) => s.listout(),
                _item => Err(StreamError::new0("expected character or string"))
            }})
        .collect()
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
    fn iter0<'node>(&'node self) -> Result<Box<dyn SIterator<Char> + 'node>, StreamError> {
        Ok(Box::new(StringReplaceIter::new(self)))
    }

    fn len(&self) -> Length {
        match self.source.len() {
            Length::Exact(_) | Length::AtMost(_) | Length::UnknownFinite => 
                Length::UnknownFinite,
            Length::Infinite | Length::Unknown => Length::Unknown
        }
    }
}

struct StringReplaceIter<'node> {
    source: Box<dyn SIterator<Char> + 'node>,
    orig: &'node Vec<Vec<Char>>,
    repl: &'node Vec<Vec<Char>>,
    longest: usize,
    cache: VecDeque<Char>,
    queued: Option<(Box<dyn Iterator<Item = Char>>, bool)>,
}

impl<'node> StringReplaceIter<'node> {
    fn new(parent: &'node StringReplace) -> Self {
        StringReplaceIter {
            source: parent.source.iter(),
            orig: &parent.orig,
            repl: &parent.repl,
            longest: parent.longest,
            cache: VecDeque::new(),
            queued: None,
        }
    }
}

impl SIterator<Char> for StringReplaceIter<'_> {
    fn next(&mut self) -> Result<Option<Char>, StreamError> {
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
            if self.cache.len() == self.longest
                && let Some(item) = self.cache.pop_front() {
                    return Ok(Some(item));
                }
            if let Some(item) = self.source.next()? {
                self.cache.push_back(item);
                'a: for (patt, repl) in self.orig.iter().zip(self.repl.iter()) {
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
    fn iter0<'node>(&'node self) -> Result<Box<dyn SIterator + 'node>, StreamError> {
        Ok(self.source.map_iter(|item| Ok(if item.try_eq(&self.orig)? { self.repl.clone() } else { item })))
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
