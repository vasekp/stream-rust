use std::str::CharIndices;
use std::iter::Peekable;
use super::ParseError;

pub struct Tokenizer<'str> {
    input: &'str str,
    iter: Peekable<CharIndices<'str>>,
    peek: Option<Option<Token<'str>>>,
}

#[derive(PartialEq)]
enum CharClass {
    Space,
    Ident,
    Delim,
    Rel,
    Comment,
    Other
}

impl CharClass {
    fn of(c: char) -> CharClass {
        use CharClass::*;
        match c {
            ' ' | '\t' | '\n' => Space,
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => Ident,
            '"' | '\'' => Delim,
            '<' | '=' | '>' => Rel,
            ';' => Comment,
            _ => Other
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenClass {
    Number,
    BaseNum,
    CBaseNum,
    Bool(bool),
    Ident,
    String,
    Char,
    Chain,
    Oper,
    Open,
    Close,
    Comma,
    Special
}

#[derive(PartialEq, Debug)]
pub struct Token<'str>(pub TokenClass, pub &'str str);

impl Token<'_> {
    pub fn new(slice: &str) -> Result<Token<'_>, ParseError<'_>> {
        use TokenClass::*;
        const OPERS: &[u8] = b"+-*/^~&|!<=>";
        let class = match slice.as_bytes() {
            [b'0'..=b'9', ..] if slice.contains('_') => BaseNum,
            [b'0', b'x' | b'b' | b'o', ..] => CBaseNum,
            [b'0'..=b'9', ..] => Number,
            b"true" => Bool(true),
            b"false" => Bool(false),
            [b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => Ident,
            [b'"', ..] => String,
            [b'\'', ..] => Char,
            b".." => Oper,
            [b'.' | b':' | b'@'] => Chain,
            [c, ..] if OPERS.contains(c) => Oper,
            [b'(' | b'[' | b'{'] => Open,
            [b')' | b']' | b'}'] => Close,
            [b','] => Comma,
            [b'#' | b'$' | b'%'] => Special, // '%' can double as an operator
            _ => return Err(ParseError::new("invalid character", slice))
        };
        Ok(Token(class, slice))
    }
}

impl<'str> Tokenizer<'str> {
    pub fn new(input: &'str str) -> Tokenizer<'str> {
        Tokenizer{input, iter: input.char_indices().peekable(), peek: None}
    }

    fn skip_same(&mut self, class: &CharClass) {
        while let Some(&(_, ch)) = self.iter.peek() {
            if CharClass::of(ch) == *class {
                self.iter.next();
            } else {
                break;
            }
        }
    }

    fn skip_until(&mut self, delim: char) -> Result<(), &'static str> {
        while let Some((_, ch)) = self.iter.next() {
            if ch == '\\' {
                if self.iter.next().is_none() {
                    return Err("unterminated string");
                }
            } else if ch == delim {
                return Ok(());
            }
        }
        Err("unterminated string")
    }

    pub fn slice_from(&mut self, start: &'str str) -> &'str str {
        let start_index = unsafe { start.as_ptr().offset_from(self.input.as_ptr()) } as usize;
        let end_index = match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        };
        &self.input[start_index..end_index]
    }

    pub fn peek(&mut self) -> Result<Option<&Token<'str>>, ParseError<'str>> {
        if let Some(ref ret) = self.peek { return Ok(ret.as_ref()); }
        let next = self.next_tr()?;
        Ok(self.peek.insert(next).as_ref())
    }

    pub fn unread(&mut self, token: Token<'str>) {
        if self.peek.is_some() {
            panic!("Tokenizer::unread() called with peek nonempty");
        }
        self.peek = Some(Some(token));
    }

    pub fn next_tr(&mut self) -> Result<Option<Token<'str>>, ParseError<'str>> {
        self.next().transpose()
    }

    pub fn merge(&self, prev: Token<'str>, next: Token<'str>, cls: TokenClass) -> Token<'str> {
        let prev_start = unsafe { prev.1.as_ptr().offset_from(self.input.as_ptr()) } as usize;
        let prev_end = prev_start + prev.1.len();
        let next_start = unsafe { next.1.as_ptr().offset_from(self.input.as_ptr()) } as usize;
        let next_end = next_start + next.1.len();
        assert_eq!(prev_end, next_start);
        Token(cls, &self.input[prev_start..next_end])
    }
}

impl<'str> Iterator for Tokenizer<'str> {
    type Item = Result<Token<'str>, ParseError<'str>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.peek.take() {
            return Ok(ret).transpose();
        }
        let (start, ch) = self.iter.next()?;
        let class = CharClass::of(ch);
        use CharClass::*;
        match class {
            Ident | Rel => self.skip_same(&class),
            Delim => if let Err(err) = self.skip_until(ch) {
                return Some(Err(ParseError::new(err,
                            &self.input[start..(start + 1)])));
            },
            Other if ch == '.' =>
                if matches!(self.iter.peek(), Some(&(_, '.'))) {
                    self.iter.next();
                },
            Other => (),
            Space => {
                self.skip_same(&class);
                return self.next();
            },
            Comment => {
                self.iter = "".char_indices().peekable();
                return None;
            }
        };
        let end = match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        };
        let slice = &self.input[start..end];
        Some(Token::new(slice))
    }
}

#[cfg(test)]
#[test]
fn test_tokenizer() {
    use TokenClass::*;

    let mut tk = Tokenizer::new(r#"a""d"#); // empty string
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"d"#); // single "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert!(matches!(tk.next(), Some(Err(_)))); // unterminated string
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"""d"#); // triple "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\""))));
    assert!(matches!(tk.next(), Some(Err(_)))); // unterminated string
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a""""d"#); // quadruple "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\""d"#); // escaped "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\\\"\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"\\"d"#); // escaped \
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\\\\\""))));
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "d"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a\"d"#); // backslash out of string (not escape!)
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert!(matches!(tk.next(), Some(Err(_)))); // invalid character
    assert!(matches!(tk.next(), Some(Err(_)))); // unterminated string
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a💖b"#); // wide character
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert!(matches!(tk.next(), Some(Err(_)))); // invalid character
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "b"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"abc_12  3::<=>xy'a"b'c"d'e""""#); // character classes
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "abc_12")))); // mixed alpha, numeric, _ should be single token
    assert_eq!(tk.next(), Some(Ok(Token(Number, "3")))); // space ignored, but prevents gluing to previous
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ":")))); // should remain single character
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ":")))); // should remain separate
    assert_eq!(tk.next(), Some(Ok(Token(Oper, "<=>")))); // relational symbols merged
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "xy")))); // alphanumeric merged, but not with previous
    assert_eq!(tk.next(), Some(Ok(Token(Char, "'a\"b'")))); // " within '
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "c")))); // outside any string
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"d'e\"")))); // ' within "
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\"")))); // correctly paired
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new("..."); // ., .. bunching
    assert_eq!(tk.next(), Some(Ok(Token(Oper, "..")))); // greedy
    assert_eq!(tk.next(), Some(Ok(Token(Chain, "."))));

    let mut tk = Tokenizer::new(r#" " " " " "#); // spaces
    // leading space ignored
    assert_eq!(tk.next(), Some(Ok(Token(String, "\" \"")))); // within string
    // space outside strings ignored
    assert_eq!(tk.next(), Some(Ok(Token(String, "\" \"")))); // within string
    // tailing space ignored
    assert_eq!(tk.next(), None);

    let mut it = Tokenizer::new("a.b0_1(3_012,0b012,#4,true@q)");
    assert_eq!(it.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(it.next(), Some(Ok(Token(Chain, "."))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "b0_1"))));
    assert_eq!(it.next(), Some(Ok(Token(Open, "("))));
    assert_eq!(it.next(), Some(Ok(Token(BaseNum, "3_012"))));
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(CBaseNum, "0b012")))); // error later
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(Special, "#"))));
    assert_eq!(it.next(), Some(Ok(Token(Number, "4"))));
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(Bool(true), "true"))));
    assert_eq!(it.next(), Some(Ok(Token(Chain, "@"))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "q"))));
    assert_eq!(it.next(), Some(Ok(Token(Close, ")"))));
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#""a'b"c'd"é'ř"#);
    assert_eq!(it.next(), Some(Ok(Token(String, "\"a'b\""))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "c"))));
    assert_eq!(it.next(), Some(Ok(Token(Char, "'d\"é'")))); // non-ASCII in quotes
    assert!(matches!(it.next(), Some(Err(_)))); // invalid character
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#"1;á"#);
    assert_eq!(it.next(), Some(Ok(Token(Number, "1"))));
    assert_eq!(it.next(), None);
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#"";";";"#);
    assert_eq!(it.next(), Some(Ok(Token(String, "\";\""))));
    assert_eq!(it.next(), None);
    assert_eq!(it.next(), None);
}

