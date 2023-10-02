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

    fn byte_pos(&mut self) -> usize {
        match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        }
    }

    fn read_single(&mut self) -> Result<(), StreamError> {
        self.iter.next();
        Ok(())
    }

    fn read_same(&mut self, class: CharClass) -> Result<(), StreamError> {
        while let Some(&(_, ch)) = self.iter.peek() {
            if char_class(ch) == class {
                self.iter.next();
            } else {
                break;
            }
        }
        Ok(())
    }

    fn read_string(&mut self) -> Result<(), StreamError> {
        let (_, delim) = self.iter.next().unwrap();
        'b: {
            while let Some((_, ch)) = self.iter.next() {
                if ch == '\\' {
                    if self.iter.next().is_none() {
                        return Err(StreamError("unterminated string".to_string()));
                    }
                } else if ch == delim {
                    break 'b;
                }
            }
            return Err(StreamError("unterminated string".to_string()));
        }
        Ok(())
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = StreamResult<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&(_, ch)) = self.iter.peek() {
            let start = self.byte_pos();
            let res = match char_class(ch) {
                CharClass::Space => {
                    self.iter.next();
                    return self.next();
                },
                class @ (CharClass::Ident | CharClass::Rel) => self.read_same(class),
                CharClass::Delim => self.read_string(),
                CharClass::Other => self.read_single()
            };
            let end = self.byte_pos();
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
