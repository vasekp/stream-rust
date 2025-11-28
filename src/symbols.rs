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
        docs.symbols = names.as_slice().iter().copied().collect();
        let rec = Arc::new((ctor, Some(docs)));
        for sym in names.as_slice() {
            self.0.insert(sym, Arc::clone(&rec));
        }
    }

    pub(crate) fn find_ctor(name: &str) -> Option<Constructor> {
        Some(SYMBOLS.0.get(name)?.0)
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

#[test]
fn test_doc_examples() {
    use crate::parser::parse;
    use std::collections::HashSet;
    let mut visited = HashSet::new();
    for (sym, rec) in &SYMBOLS.0 {
        if !visited.insert(Arc::as_ptr(rec)) { continue; }
        let Some(docs) = &rec.1 else { continue; };
        for ex in &docs.examples {
            let Ok(expr) = parse(&ex.input.replace("?", sym)) else {
                panic!("failed to parse example in {}: {:?}", sym, ex);
            };
            let res = expr.eval_default();
            match (&ex.width, ex.output) {
                (None, Ok(out)) => assert_eq!(res.as_ref().map(Item::to_string),
                    Ok(out.into()), "failed example in {}: {:?}", sym, ex),
                (Some(width), Ok(out)) => assert_eq!(res.map(|item| format!("{:1$}", item, width)),
                    Ok(out.into()), "failed example in {}: {:?}", sym, ex),
                (_, Err(_)) => assert!(res.is_err(), "failed example in {}: {:?}", sym, ex)
            }
        }
    }
}
