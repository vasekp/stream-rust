use crate::base::*;
use crate::docs::DocRecord;
use std::collections::HashMap;
use std::sync::Arc;
use once_cell::sync::Lazy;

type Constructor = fn(Node, &'_ Env) -> Result<Item, StreamError>;

#[derive(Default)]
pub(crate) struct Symbols(HashMap<&'static str, Arc<(Constructor, Option<DocRecord>)>>);

static SYMBOLS: Lazy<Symbols> = Lazy::new(|| {
    let mut symbols = Default::default();
    crate::ops::init(&mut symbols);
    symbols
});

impl Symbols {
    pub(crate) fn insert(&mut self, names: impl AsSlice<&'static str>, ctor: Constructor) {
        let rec = Arc::new((ctor, None));
        for sym in names.as_slice() {
            self.0.insert(sym, Arc::clone(&rec));
        }
    }

    pub(crate) fn insert_with_docs(&mut self, names: impl AsSlice<&'static str>,
            ctor: Constructor, doc_string: &'static str) {
        let mut docs = DocRecord::from(doc_string);
        debug_assert!(docs.symbols.is_empty());
        docs.symbols = names.as_slice().to_vec();
        let rec = Arc::new((ctor, Some(docs)));
        for sym in names.as_slice() {
            self.0.insert(sym, Arc::clone(&rec));
        }
    }

    pub(crate) fn find_ctor(name: &str) -> Option<Constructor> {
        Some(SYMBOLS.0.get(name)?.0)
    }

    pub(crate) fn find_docs(name: &str) -> Option<&DocRecord> {
        SYMBOLS.0.get(name)?.1.as_ref()
    }
}

pub(crate) trait AsSlice<T> {
    fn as_slice(&self) -> &[T];
}

impl<T> AsSlice<T> for T {
    fn as_slice(&self) -> &[T] { std::slice::from_ref(self) }
}

impl<T, const N: usize> AsSlice<T> for [T; N] {
    fn as_slice(&self) -> &[T] { self }
}

pub fn find_docs(name: &str) -> Option<&DocRecord> {
    Symbols::find_docs(name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::docs;

    #[derive(Debug)]
    struct DocError;

    #[test]
    fn test_docs() -> Result<(), DocError> {
        use std::collections::HashSet;
        let mut res = Ok(());
        let mut visited = HashSet::new();
        let mut missing = Vec::new();
        for (sym, rec) in &SYMBOLS.0 {
            if !(sym.as_bytes()[0] as char).is_ascii_alphabetic() { continue; }
            if !visited.insert(Arc::as_ptr(rec)) { continue; }
            let (_, Some(docs)) = &**rec else {
                missing.push(sym);
                continue;
            };
            if docs.desc.is_empty() {
                println!("{sym}: empty description");
                res = Err(DocError);
            }
            for (line, _) in &docs.desc {
                res = res.and(check_refs(line, sym));
            }
            if docs.usage.is_empty() {
                println!("{sym}: empty usage");
                res = Err(DocError);
            }
            if docs.examples.is_empty() {
                println!("{sym}: no examples");
                res = Err(DocError);
            }
            for ex in &docs.examples {
                res = res.and(test_example(ex, sym));
                res = res.and(check_refs(ex.input, sym));
            }
            for see in &docs.see {
                res = res.and(check_ref(see, sym));
            }
        }
        let mut iter = missing.into_iter();
        if let Some(sym) = iter.next() {
            print!("No documentation: {sym}");
            for sym in iter {
                print!(", {sym}");
            }
            println!();
            res = Err(DocError);
        }
        res
    }

    fn test_example(ex: &docs::Example, sym: &str) -> Result<(), DocError> {
        let [line] = &docs::parse_line(&ex.input, sym)[..] else {
            panic!("malformed example in {}: {}", sym, ex.input);
        };
        let input = line.flatten();
        let Ok(expr) = crate::parse(&input) else {
            panic!("failed to parse example in {}: {:?}", sym, input);
        };
        let res = expr.eval_default();
        let success = match (&ex.width, ex.output, res) {
            (None, Ok(out), res) => res.is_ok_and(|item| item.to_string() == out),
            (Some(width), Ok(out), res) => res.is_ok_and(|item| format!("{:1$}", item, width) == out),
            (_, Err(_), res) => res.is_err(),
        };
        if !success {
            println!("{sym}: failed example: {input:?}");
            Err(DocError)
        } else {
            Ok(())
        }
    }

    fn check_refs(line: &str, sym: &str) -> Result<(), DocError> {
        use crate::docs;
        use crate::docs::RefStringItem;
        let mut res = Ok(());
        for part in docs::parse_line(line, sym) {
            for rsym in part.content.iter()
                .filter_map(|item| match item { RefStringItem::Ref(rsym) => Some(rsym), _ => None })
                .filter(|rsym| **rsym != sym) {
                    res = res.and(check_ref(rsym, sym));
            }
        }
        res
    }

    fn check_ref(sym: &str, referrer: &str) -> Result<(), DocError> {
        if SYMBOLS.0.get(sym).is_none() {
            println!("{referrer} references {sym} which does not exist");
            Err(DocError)
        } else {
            Ok(())
        }
    }
}
