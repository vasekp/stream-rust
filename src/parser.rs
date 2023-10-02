use std::str::CharIndices;
use std::iter::Peekable;
use crate::base::BaseError;

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

    fn skip_until(&mut self, delim: char) -> Result<(), BaseError> {
        while let Some((_, ch)) = self.iter.next() {
            if ch == '\\' {
                if self.iter.next().is_none() {
                    return Err(BaseError("unterminated string".to_string()));
                }
            } else if ch == delim {
                return Ok(());
            }
        }
        return Err(BaseError("unterminated string".to_string()));
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<&'a str, BaseError>;

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
    let mut tk = Tokenizer::new(r#"a""d"#); // empty string
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"d"#); // single "
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Err(BaseError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"""d"#); // triple "
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Err(BaseError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a""""d"#); // quadruple "
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Ok("\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\""d"#); // escaped "
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\\\"\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\\"d"#); // escaped \
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\"\\\\\"")));
    assert_eq!(tk.next(), Some(Ok("d")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a\"d"#); // backslash out of string (not escape!)
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("\\")));
    assert_eq!(tk.next(), Some(Err(BaseError("unterminated string".to_string()))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a💖b"#); // wide character
    assert_eq!(tk.next(), Some(Ok("a")));
    assert_eq!(tk.next(), Some(Ok("💖")));
    assert_eq!(tk.next(), Some(Ok("b")));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"abc_12  3..:<=>xy'a"b'c"d'e""""#); // character classes
    assert_eq!(tk.next(), Some(Ok("abc_12"))); // mixed alpha, numeric, _ should be single token
    assert_eq!(tk.next(), Some(Ok("3"))); // space ignored, but prevents gluing to previous
    assert_eq!(tk.next(), Some(Ok("."))); // should remain single character
    assert_eq!(tk.next(), Some(Ok("."))); // should remain separate
    assert_eq!(tk.next(), Some(Ok(":"))); // ditto
    assert_eq!(tk.next(), Some(Ok("<=>"))); // relational symbols merged
    assert_eq!(tk.next(), Some(Ok("xy"))); // alphanumeric merged, but not with previous
    assert_eq!(tk.next(), Some(Ok("'a\"b'"))); // " within '
    assert_eq!(tk.next(), Some(Ok("c"))); // outside any string
    assert_eq!(tk.next(), Some(Ok("\"d'e\""))); // ' within "
    assert_eq!(tk.next(), Some(Ok("\"\""))); // correctly paired
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#" " " " " "#); // spaces
    // leading space ignored
    assert_eq!(tk.next(), Some(Ok("\" \""))); // within string
    // space outside strings ignored
    assert_eq!(tk.next(), Some(Ok("\" \""))); // within string
    // tailing space ignored
    assert_eq!(tk.next(), None);
}
