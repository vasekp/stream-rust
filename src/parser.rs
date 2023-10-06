use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::{Display, Formatter, Debug};
use crate::base::BaseError;


#[derive(Debug)]
pub struct ParseError<'a> {
    base: BaseError,
    slice: &'a str
}

impl<'a> ParseError<'a> {
    fn new<T>(text: T, slice: &'a str) -> ParseError<'a> where T: Into<BaseError> {
        ParseError{base: text.into(), slice}
    }

    #[cfg(test)]
    fn cmp_ref<T>(text: T) -> ParseError<'a> where T: Into<BaseError> {
        Self::new(text, Default::default())
    }

    fn display(&self, input: &'a str) {
        let start = unsafe { self.slice.as_ptr().offset_from(input.as_ptr()) } as usize;
        let length = self.slice.len();
        println!("\x1b[8m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)]);
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        Display::fmt(&self.base, f)
    }
}

impl<'a> std::error::Error for ParseError<'a> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.base)
    }
}

impl<'a> PartialEq for ParseError<'a> {
    fn eq(&self, other: &ParseError<'a>) -> bool {
        self.base == other.base
    }
}


pub fn parse(input: &str) {
    let tk = Tokenizer::new(input);
    for t in tk {
        if let Err(err) = t {
            err.display(input);
            println!("{}", err);
        }
    }
}


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
    Special
}

fn token_class(slice: &str) -> Result<TokenClass, ParseError> {
    use TokenClass::*;
    let class = match slice.chars().next().unwrap() {
        '0'..='9' => if slice.contains('_') {
                BaseNum
            } else {
                Number
            },
        'a'..='z' | 'A'..='Z' => Ident,
        '"' => Str,
        '\'' => Char,
        '.' | ':' | '<' | '=' | '>' => Oper, // TODO
        '(' | '[' | '{' => Open,
        ')' | ']' | '}' => Close,
        ',' => Comma,
        '#' | '$' => Special,
        _ => return Err(ParseError::new("invalid character", slice))
    };
    Ok(class)
}

#[derive(PartialEq, Debug)]
struct Token<'a>(TokenClass, &'a str);

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
                    return Err(BaseError::from("unterminated string"));
                }
            } else if ch == delim {
                return Ok(());
            }
        }
        return Err(BaseError::from("unterminated string"));
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, ParseError<'a>>;

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
            let slice = &self.input[start..end];
            Some(res
                .map_err(|base| ParseError::new(base, slice))
                .and_then(|_| token_class(slice).map(|class| Token(class, slice)) ))
        } else {
            None
        }
    }
}

#[test]
fn test_parser() {
    use TokenClass::*;

    let mut tk = Tokenizer::new(r#"a""d"#); // empty string
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"d"#); // single "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("unterminated string"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"""d"#); // triple "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\""))));
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("unterminated string"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a""""d"#); // quadruple "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\""d"#); // escaped "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\\\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\\"d"#); // escaped \
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\\\\\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a\"d"#); // backslash out of string (not escape!)
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("invalid character"))));
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("unterminated string"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a💖b"#); // wide character
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("invalid character"))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "b"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"abc_12  3..:<=>xy'a"b'c"d'e""""#); // character classes
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "abc_12")))); // mixed alpha, numeric, _ should be single token
    assert_eq!(tk.next(), Some(Ok(Token(Number, "3")))); // space ignored, but prevents gluing to previous
    assert_eq!(tk.next(), Some(Ok(Token(Oper, ".")))); // should remain single character
    assert_eq!(tk.next(), Some(Ok(Token(Oper, ".")))); // should remain separate
    assert_eq!(tk.next(), Some(Ok(Token(Oper, ":")))); // ditto
    assert_eq!(tk.next(), Some(Ok(Token(Oper, "<=>")))); // relational symbols merged
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "xy")))); // alphanumeric merged, but not with previous
    assert_eq!(tk.next(), Some(Ok(Token(Char, "'a\"b'")))); // " within '
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "c")))); // outside any string
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"d'e\"")))); // ' within "
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\"\"")))); // correctly paired
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#" " " " " "#); // spaces
    // leading space ignored
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\" \"")))); // within string
    // space outside strings ignored
    assert_eq!(tk.next(), Some(Ok(Token(Str, "\" \"")))); // within string
    // tailing space ignored
    assert_eq!(tk.next(), None);

    let mut it = Tokenizer::new("a.b0_1(3_012,#4)");
    assert_eq!(it.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(it.next(), Some(Ok(Token(Oper, "."))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "b0_1"))));
    assert_eq!(it.next(), Some(Ok(Token(Open, "("))));
    assert_eq!(it.next(), Some(Ok(Token(BaseNum, "3_012"))));
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(Special, "#"))));
    assert_eq!(it.next(), Some(Ok(Token(Number, "4"))));
    assert_eq!(it.next(), Some(Ok(Token(Close, ")"))));
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#""a'b"c'd"é'ř"#);
    assert_eq!(it.next(), Some(Ok(Token(Str, "\"a'b\""))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "c"))));
    assert_eq!(it.next(), Some(Ok(Token(Char, "'d\"é'")))); // non-ASCII in quotes
    assert_eq!(it.next(), Some(Err(ParseError::cmp_ref("invalid character")))); // non-ASCII
    assert_eq!(it.next(), None);
}
