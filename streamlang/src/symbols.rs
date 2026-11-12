use crate::base::*;
use crate::docs::DocRecord;
use crate::interner;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock};

type Constructor = fn(&Node, &'_ Env) -> SResult<Item>;

#[derive(Default)]
pub(crate) struct Symbols(HashMap<&'static str, Arc<(Constructor, Option<DocRecord>)>>);

static SYMBOLS: LazyLock<Symbols> = LazyLock::new(|| {
    let mut symbols = Default::default();
    crate::ops::init(&mut symbols);
    symbols
});

impl Symbols {
    pub(crate) fn insert_raw(&mut self, names: impl AsSlice<&'static str>, ctor: Constructor) {
        let rec = Arc::new((ctor, None));
        for sym in names.as_slice() {
            let sym = interner::intern(&sym.to_lowercase());
            let prev = self.0.insert(sym, Arc::clone(&rec));
            if prev.is_some() {
                panic!("Duplicate definition of {sym}");
            }
        }
    }

    pub(crate) fn insert(&mut self, names: impl AsSlice<&'static str>, ctor: Constructor, doc_string: &'static str) {
        let mut docs = DocRecord::from(doc_string);
        debug_assert!(docs.symbols.is_empty());
        docs.symbols = names.as_slice().to_vec();
        let rec = Arc::new((ctor, Some(docs)));
        for sym in names.as_slice() {
            let sym = interner::intern(&sym.to_lowercase());
            let prev = self.0.insert(sym, Arc::clone(&rec));
            if prev.is_some() {
                panic!("Duplicate definition of {sym}");
            }
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
    fn as_slice(&self) -> &[T] {
        std::slice::from_ref(self)
    }
}

impl<T, const N: usize> AsSlice<T> for [T; N] {
    fn as_slice(&self) -> &[T] {
        self
    }
}

/// Finds the documentation of a symbol, or a help page on the language: the quick guide under
/// an empty `name` and its sections under the names it refers to, which start with `?`.
pub fn find_docs(name: &str) -> Option<&DocRecord> {
    Symbols::find_docs(name).or_else(|| crate::help::Topics::find_docs(name))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::docs;

    #[derive(Debug)]
    pub(crate) struct DocError;

    /// The first name of every documented symbol which is a word rather than an operator.
    pub(crate) fn documented_names() -> Vec<&'static str> {
        let mut names = SYMBOLS
            .0
            .values()
            .filter_map(|rec| rec.1.as_ref())
            .map(|docs| docs.symbols[0])
            .filter(|sym| sym.starts_with(|c: char| c.is_ascii_alphabetic()))
            .collect::<Vec<_>>();
        names.sort();
        names.dedup();
        names
    }

    #[test]
    fn test_docs() -> Result<(), DocError> {
        use std::collections::HashSet;
        let mut res = Ok(());
        let mut visited = HashSet::new();
        for (sym, rec) in &SYMBOLS.0 {
            if !visited.insert(Arc::as_ptr(rec)) {
                continue;
            }
            let (_, Some(docs)) = &**rec else {
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
                if let Some(comment) = ex.comment {
                    res = res.and(check_refs(comment, sym));
                }
            }
            for see in &docs.see {
                res = res.and(check_ref(see, sym));
            }
        }
        res
    }

    pub(crate) fn test_example(ex: &docs::Example, sym: &str) -> Result<(), DocError> {
        let [line] = &docs::parse_line(ex.input, sym)[..] else {
            panic!("malformed example in {}: {}", sym, ex.input);
        };
        let input = line.flatten();
        let expr = match crate::parse(&input) {
            Ok(expr) => expr,
            Err(err) => match ex.output {
                // The quoted error message must match the actual one, so that the help text
                // stays truthful.
                Err(msg) if err.to_string() == msg => return Ok(()),
                Err(msg) => {
                    println!("{sym}: example {input:?} gives parse error {:?}, documented as {msg:?}", err.to_string());
                    return Err(DocError);
                },
                Ok(_) => panic!("failed to parse example in {}: {:?}", sym, input),
            },
        };
        let res = expr.eval_default();
        let success = match (&ex.width, ex.output, &res) {
            (None, Ok(out), res) => res.as_ref().is_ok_and(|item| item.to_string() == out),
            (Some(width), Ok(out), res) => res.as_ref().is_ok_and(|item| format!("{:1$}", item, width) == out),
            (_, Err(msg), res) => res.as_ref().is_err_and(|err| err.reason().to_string() == msg),
        };
        if !success {
            match (ex.output, res) {
                (Err(msg), Err(err)) => println!(
                    "{sym}: example {input:?} gives error {:?}, documented as {msg:?}",
                    err.reason().to_string()
                ),
                _ => println!("{sym}: failed example: {input:?}"),
            }
            Err(DocError)
        } else {
            Ok(())
        }
    }

    pub(crate) fn check_refs(line: &str, sym: &str) -> Result<(), DocError> {
        use crate::docs;
        use crate::docs::RefStringItem;
        let mut res = Ok(());
        for part in docs::parse_line(line, sym) {
            for rsym in part
                .content
                .iter()
                .filter_map(|item| match item {
                    RefStringItem::Ref(rsym) => Some(rsym),
                    _ => None,
                })
                .filter(|rsym| **rsym != sym)
            {
                res = res.and(check_ref(rsym, sym));
            }
        }
        res
    }

    pub(crate) fn check_ref(sym: &str, referrer: &str) -> Result<(), DocError> {
        if !SYMBOLS.0.contains_key(sym) && crate::help::Topics::find_docs(sym).is_none() {
            println!("{referrer} references {sym} which does not exist");
            Err(DocError)
        } else {
            Ok(())
        }
    }
}
