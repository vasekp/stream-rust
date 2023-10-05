use std::str::CharIndices;
use std::iter::Peekable;
use crate::base::BaseError;


struct Tokenizer<'a> {
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
    use CharClass::*;
    match c {
        ' ' | '\t' | '\n' => Space,
        'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => Ident,
        '"' | '\'' => Delim,
        '<' | '=' | '>' => Rel,
        _ => Other
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
            use CharClass::*;
            let res = match class {
                Ident | Rel | Space => Ok(self.skip_same(&class)),
                Delim => self.skip_until(ch),
                Other => Ok(())
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


#[derive(PartialEq, Debug)]
enum TokenClass {
    Number,
    BaseNum,
    Ident,
    Str,
    Char,
    Oper,
    Open,
    Close,
    Comma,
    Special,
    Unknown
}

struct Token<'a>(TokenClass, &'a str);

impl TokenClass {
    fn classify(slice: &str) -> TokenClass {
        use TokenClass::*;
        match slice.chars().next().unwrap() {
            '0'..='9' => if slice.contains('_') {
                    BaseNum
                } else {
                    Number
                },
            'a'..='z' | 'A'..='Z' => Ident,
            '"' => Str,
            '\'' => Char,
            '.' | ':' => Oper, // TODO
            '(' | '[' | '{' => Open,
            ')' | ']' | '}' => Close,
            ',' => Comma,
            '#' | '$' => Special,
            _ => Unknown
        }
    }
}

#[test]
fn test_classify() {
    use TokenClass::*;

    let mut it = Tokenizer::new("a.b0_1(3_012,#4)").map(|r| r.map(TokenClass::classify));
    assert_eq!(it.next(), Some(Ok(Ident)));
    assert_eq!(it.next(), Some(Ok(Oper)));
    assert_eq!(it.next(), Some(Ok(Ident)));
    assert_eq!(it.next(), Some(Ok(Open)));
    assert_eq!(it.next(), Some(Ok(BaseNum)));
    assert_eq!(it.next(), Some(Ok(Comma)));
    assert_eq!(it.next(), Some(Ok(Special)));
    assert_eq!(it.next(), Some(Ok(Number)));
    assert_eq!(it.next(), Some(Ok(Close)));
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#""a'b"c'd"é'ř"#).map(|r| r.map(TokenClass::classify));
    assert_eq!(it.next(), Some(Ok(Str)));
    assert_eq!(it.next(), Some(Ok(Ident)));
    assert_eq!(it.next(), Some(Ok(Char)));
    assert_eq!(it.next(), Some(Ok(Unknown)));
    assert_eq!(it.next(), None);
}
