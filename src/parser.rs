use std::str::CharIndices;
use std::iter::Peekable;
use crate::base::{StreamError, StreamResult};

pub struct Tokenizer<'a> {
    input: &'a str,
    iter: Peekable<CharIndices<'a>>,
}

#[derive(PartialEq)]
enum CharClass {
    Space,
    Ident,
    Delim,
    Rel,
    Other
}

fn char_class(c: char) -> CharClass {
    match c {
        ' ' | '\t' | '\n' => CharClass::Space,
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => CharClass::Ident,
        '"' | '\'' => CharClass::Delim,
        '<' | '=' | '>' => CharClass::Rel,
        _ => CharClass::Other
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer{input, iter: input.char_indices().peekable()}
    }

    fn skip_same(&mut self, class: &CharClass) {
        while let Some(&(_, ch)) = self.iter.peek() {
            if char_class(ch) == *class {
                self.iter.next();
            } else {
                break;
            }
        }
    }

    fn skip_until(&mut self, delim: char) -> Result<(), StreamError> {
        while let Some((_, ch)) = self.iter.next() {
            if ch == '\\' {
                if self.iter.next().is_none() {
                    return Err(StreamError("unterminated string".to_string()));
                }
            } else if ch == delim {
                return Ok(());
            }
        }
        return Err(StreamError("unterminated string".to_string()));
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = StreamResult<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((start, ch)) = self.iter.next() {
            let class = char_class(ch);
            let res = match class {
                CharClass::Ident | CharClass::Rel | CharClass::Space => Ok(self.skip_same(&class)),
                CharClass::Delim => self.skip_until(ch),
                CharClass::Other => Ok(())
            };
            if class == CharClass::Space {
                return self.next();
            }
            let end = match self.iter.peek() {
                Some(&(pos, _)) => pos,
                None => self.input.len()
            };
            Some(res.map(|_| &self.input[start..end]))
        } else {
            None
        }
    }
}

#[test]
fn test_parser() {
    let mut tk = Tokenizer::new(r#"a""d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Err(StreamError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"""d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Err(StreamError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\""d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\\\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\\"d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\\\\\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a\"d"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\\")));
    assert_eq!(tk.next(), Some(Err(StreamError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a💖b"#);
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("💖")));
    assert_eq!(tk.next(), Some(Ok("b")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"abc_12  3.:<=>'a"b'c"d'e"f"#);
    assert_eq!(tk.next(), Some(Ok("abc_12")));
    assert_eq!(tk.next(), Some(Ok("3")));
    assert_eq!(tk.next(), Some(Ok(".")));
    assert_eq!(tk.next(), Some(Ok(":")));
    assert_eq!(tk.next(), Some(Ok("<=>")));
    assert_eq!(tk.next(), Some(Ok("'a\"b'")));
    assert_eq!(tk.next(), Some(Ok("c")));
    assert_eq!(tk.next(), Some(Ok("\"d'e\"")));
    assert_eq!(tk.next(), Some(Ok("f")));
    assert_eq!(tk.next(), None);
}
