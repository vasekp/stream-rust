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
            ctor: Constructor, mut docs: DocRecord) {
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

    #[derive(Debug)]
    struct DocError;

    #[test]
    fn test_docs() -> Result<(), DocError> {
        let mut res = Ok(());
        let mut missing = Vec::new();
        for (sym, rec) in &SYMBOLS.0 {
            let (_, Some(docs)) = &**rec else {
                missing.push(sym);
                continue;
            };
            if docs.desc.is_empty() {
                println!("{sym}: empty description");
                res = Err(DocError);
            }
            if docs.usage.is_empty() {
                println!("{sym}: empty usage");
                res = Err(DocError);
            }
            if docs.examples.is_empty() {
                println!("{sym}: no examples");
                res = Err(DocError);
            }
        }
        let mut iter = missing.into_iter();
        if let Some(sym) = iter.next() {
            print!("No documentation: {sym}");
        }
        for sym in iter {
            print!(", {sym}");
        }
        println!();
        res
    }

    #[test]
    fn test_doc_examples() -> Result<(), DocError> {
        use crate::parser::parse;
        use std::collections::HashSet;
        use crate::docs;
        let mut visited = HashSet::new();
        let mut res = Ok(());
        for (sym, rec) in &SYMBOLS.0 {
            if !visited.insert(Arc::as_ptr(rec)) { continue; }
            let Some(docs) = &rec.1 else { continue; };
            for ex in &docs.examples {
                let [line] = &docs::parse_line(&ex.input, sym)[..] else {
                    panic!("malformed example in {}: {}", sym, ex.input);
                };
                let input = line.flatten();
                let Ok(expr) = parse(&input) else {
                    panic!("failed to parse example in {}: {:?}", sym, input);
                };
                let res = expr.eval_default();
                match (&ex.width, ex.output) {
                    (None, Ok(out)) => assert_eq!(res.as_ref().map(Item::to_string),
                        Ok(out.into()), "failed example in {}: {:?}", sym, input),
                    (Some(width), Ok(out)) => assert_eq!(res.map(|item| format!("{:1$}", item, width)),
                        Ok(out.into()), "failed example in {}: {:?}", sym, input),
                    (_, Err(_)) => assert!(res.is_err(), "failed example in {}: {:?}", sym, input)
                }
            }
            for line in &docs.desc {
                res = res.and(check_refs(line, sym));
            }
            for ex in &docs.examples {
                res = res.and(check_refs(ex.input, sym));
            }
            for see in &docs.see {
                res = res.and(check_ref(see, sym));
            }
        }
        res
    }

    fn check_refs(line: &str, sym: &str) -> Result<(), DocError> {
        use crate::docs;
        use crate::docs::RefStringItem;
        let mut res = Ok(());
        for part in docs::parse_line(line, sym) {
            for rsym in part.content.iter()
                .filter_map(|item| match item { RefStringItem::Ref(rsym) => Some(rsym), _ => None })
                .filter(|rsym| *rsym != sym) {
                    res = res.and(check_ref(rsym, sym));
            }
        }
        res
    }

    fn check_ref(sym: &str, referrer: &str) -> Result<(), DocError> {
        let Some(docs) = SYMBOLS.0.get(sym) else {
            eprintln!("{referrer} references {sym} which does not exist");
            return Err(DocError);
        };
        if docs.1.is_none() {
            eprintln!("{referrer} references {sym} which does not have docs");
            Err(DocError)
        } else {
            Ok(())
        }
    }
}
