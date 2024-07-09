use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::{Display, Formatter, Debug};
use crate::base::*;
use crate::lang::LiteralString;
use num::BigInt;


/// The error type returned by [`parse`]. Contains the description of the error and its location
/// within the input string. The lifetime is bound to the lifetime of the input string.
#[derive(Debug, PartialEq)]
pub struct ParseError<'str> {
    reason: String,
    slice: &'str str
}

impl<'str> ParseError<'str> {
    fn new(text: impl Into<String>, slice: &'str str) -> ParseError<'str> {
        ParseError{reason: text.into(), slice}
    }

    /// Shows the location of the parse error. For this purpose, the input string is reproduced in
    /// full. The part causing the error is highlighted using ANSI color sequences.
    ///
    /// For the actual description of the error, use the `Display` trait.
    pub fn display(&self, input: &'str str) {
        if self.slice.is_empty() { return; }
        let start = unsafe { self.slice.as_ptr().offset_from(input.as_ptr()) } as usize;
        let length = self.slice.len();
        //println!("\x1b[8m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)]);
        println!("{}\x1b[1;31m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)], &input[(start+length)..]);
    }
}

impl<'str> Display for ParseError<'str> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.reason, f)
    }
}


struct Tokenizer<'str> {
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

fn char_class(c: char) -> CharClass {
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

#[derive(PartialEq, Debug, Clone, Copy)]
enum TokenClass {
    Number,
    BaseNum,
    Bool,
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

fn token_class(slice: &str) -> Result<TokenClass, ParseError> {
    use TokenClass::*;
    const OPERS: &str = "+-*/%^~&|<=>";
    let class = match slice.chars().next().unwrap() {
        '0'..='9' => if slice.contains('_') { BaseNum } else { Number },
        'a'..='z' | 'A'..='Z' | '_' => if slice == "true" || slice == "false" { Bool } else { Ident },
        '"' => String,
        '\'' => Char,
        '.' | ':' | '@' => if slice == ".." { Oper } else { Chain },
        c if OPERS.contains(c) => Oper,
        '(' | '[' | '{' => Open,
        ')' | ']' | '}' => Close,
        ',' => Comma,
        '#' | '$' => Special,
        _ => return Err(ParseError::new("invalid character", slice))
    };
    Ok(class)
}

#[derive(PartialEq, Debug)]
struct Token<'str>(TokenClass, &'str str);

impl<'str> Tokenizer<'str> {
    pub fn new(input: &'str str) -> Tokenizer<'str> {
        Tokenizer{input, iter: input.char_indices().peekable(), peek: None}
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
        let next = self.next().transpose()?;
        Ok(self.peek.insert(next).as_ref())
    }

    pub fn unread(&mut self, token: Token<'str>) {
        if self.peek.is_some() {
            panic!("Tokenizer::unread() called with peek nonempty");
        }
        self.peek = Some(Some(token));
    }
}

impl<'str> Iterator for Tokenizer<'str> {
    type Item = Result<Token<'str>, ParseError<'str>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ret) = self.peek.take() {
            return Ok(ret).transpose();
        }
        let (start, ch) = self.iter.next()?;
        let class = char_class(ch);
        use CharClass::*;
        let res = match class {
            Ident | Rel | Space => {
                self.skip_same(&class);
                Ok(())
            },
            Delim => self.skip_until(ch),
            Other => Ok(()),
            Comment => {
                self.iter = "".char_indices().peekable();
                return None
            }
        };
        if class == CharClass::Space {
            return self.next();
        }
        if ch == '.' && matches!(self.iter.peek(), Some(&(_, '.'))) {
            self.iter.next();
        }
        let end = match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        };
        let slice = &self.input[start..end];
        Some(res
            .map_err(|reason| ParseError::new(reason, slice))
            .and_then(|_| token_class(slice).map(|class| Token(class, slice)) ))
    }
}

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

    let mut tk = Tokenizer::new(r#"abc_12  3...::<=>xy'a"b'c"d'e""""#); // character classes
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "abc_12")))); // mixed alpha, numeric, _ should be single token
    assert_eq!(tk.next(), Some(Ok(Token(Number, "3")))); // space ignored, but prevents gluing to previous
    assert_eq!(tk.next(), Some(Ok(Token(Oper, "..")))); // greedy & special case
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ".")))); // third in a row does not merge
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ":")))); // should remain separate
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ":")))); // should remain single character
    assert_eq!(tk.next(), Some(Ok(Token(Oper, "<=>")))); // relational symbols merged
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "xy")))); // alphanumeric merged, but not with previous
    assert_eq!(tk.next(), Some(Ok(Token(Char, "'a\"b'")))); // " within '
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "c")))); // outside any string
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"d'e\"")))); // ' within "
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\"")))); // correctly paired
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#" " " " " "#); // spaces
    // leading space ignored
    assert_eq!(tk.next(), Some(Ok(Token(String, "\" \"")))); // within string
    // space outside strings ignored
    assert_eq!(tk.next(), Some(Ok(Token(String, "\" \"")))); // within string
    // tailing space ignored
    assert_eq!(tk.next(), None);

    let mut it = Tokenizer::new("a.b0_1(3_012,#4,true@q)");
    assert_eq!(it.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(it.next(), Some(Ok(Token(Chain, "."))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "b0_1"))));
    assert_eq!(it.next(), Some(Ok(Token(Open, "("))));
    assert_eq!(it.next(), Some(Ok(Token(BaseNum, "3_012"))));
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(Special, "#"))));
    assert_eq!(it.next(), Some(Ok(Token(Number, "4"))));
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(Bool, "true"))));
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


fn closing(bracket: &str) -> &'static str {
    match bracket {
        "(" => ")",
        "[" => "]",
        "{" => "}",
        _ => unreachable!()
    }
}

fn parse_basenum(slice: &str) -> Result<BigInt, ParseError<'_>> {
    let mut iter = slice.split(|c| c == '_');
    let base_str = iter.next().unwrap();
    let base = base_str.parse::<u32>().map_err(|_| ParseError::new("invalid base", base_str))?;
    if !(2..=36).contains(&base) {
        return Err(ParseError::new("invalid base", base_str));
    }
    let main_str = iter.next().unwrap();
    if main_str.is_empty() {
        return Err(ParseError::new("malformed number", slice));
    }
    let res = BigInt::parse_bytes(main_str.as_bytes(), base)
        .ok_or(ParseError::new(format!("invalid digits in base {base}"), main_str))?;
    if iter.next().is_some() {
        return Err(ParseError::new("malformed number", slice));
    }
    Ok(res)
}

#[test]
fn test_basenum() {
    assert!(parse_basenum("2_").is_err()); // malformed number
    assert!(parse_basenum("2_1_").is_err()); // malformed number
    assert!(parse_basenum("2_1_0").is_err()); // malformed number
    assert!(parse_basenum("1_0").is_err()); // invalid base
    assert!(parse_basenum("0_0").is_err()); // invalid base
    assert!(parse_basenum("37_0").is_err()); // invalid base
    assert!(parse_basenum("999999999999999999_0").is_err()); // invalid base
    assert!(parse_basenum("2a_0").is_err()); // invalid base
    assert_eq!(parse_basenum("2_0"), Ok(BigInt::from(0)));
    assert_eq!(parse_basenum("2_101"), Ok(BigInt::from(5)));
    assert!(parse_basenum("2_102").is_err()); // invalid digits in base 2
    assert_eq!(parse_basenum("10_999999999999999999999999"), Ok("999999999999999999999999".parse::<BigInt>().unwrap()));
    assert_eq!(parse_basenum("16_fffFFffFFfFfFFFFffFF"), Ok("1208925819614629174706175".parse::<BigInt>().unwrap()));
}

fn parse_string(slice: &str) -> Result<String, ParseError<'_>> {
    let mut ret = String::new();
    // First and last characters are guaranteed to be ' or " and thus single-byte
    let inner = &slice[1..(slice.len() - 1)];
    let mut it = inner.char_indices().peekable();
    while let Some((index, c)) = it.next() {
        if c == '\\' {
            let d = it.next().unwrap().1; // \ is guaranteed to be followed by at least 1 char
            match d {
                '\\' => ret.push(d),
                'n' => ret.push('\n'),
                'r' => ret.push('\r'),
                't' => ret.push('\t'),
                '\'' => ret.push('\''),
                '\"' => ret.push('"'),
                _ => {
                    let end_index = match it.peek() {
                        Some(&(pos, _)) => pos,
                        None => inner.len()
                    };
                    return Err(ParseError::new("invalid escape sequence", &inner[index..end_index]));
                }
            }
        } else {
            ret.push(c);
        }
    }
    Ok(ret)
}

fn parse_char(slice: &str) -> Result<Char, ParseError<'_>> {
    let content = parse_string(slice)?;
    if content.is_empty() {
        Err(ParseError::new("empty character", slice))
    } else {
        Ok(Char::from(content))
    }
}

fn get_op(op: &str) -> (u32, bool) {
    match op {
        "~" => (1, true),
        "+" => (2, true),
        "-" => (2, false),
        "*" => (3, true),
        "/" => (3, false),
        "^" => (4, false),
        ".." => (5, false),
        _ => todo!()
    }
}


struct Parser<'str> {
    tk: Tokenizer<'str>
}

impl<'str> Parser<'str> {
    fn read_node(&mut self) -> Result<Option<Node>, ParseError<'str>> {
        let Some(tok) = self.tk.next().transpose()? else { return Ok(None); };
        use TokenClass as TC;
        let head = match tok {
            Token(TC::Ident, name) => Head::Symbol(name.into()),
            Token(TC::Open, bkt @ "{") => {
                match &mut self.read_args(bkt)?[..] {
                    [e] => Head::Block(Box::new(std::mem::take(e))),
                    [] => return Err(ParseError::new("empty block", self.tk.slice_from(bkt))),
                    _ => return Err(ParseError::new("only one expression expected", self.tk.slice_from(bkt)))
                }
            },
            Token(_, tok) => return Err(ParseError::new("cannot appear here", tok))
        };
        Ok(Some(match self.tk.peek()? {
            Some(&Token(TC::Open, bkt @ "(")) => {
                self.tk.next();
                Node{head, source: None, args: self.read_args(bkt)?}
            },
            Some(&Token(TC::Chain, tok @ "@")) => {
                self.tk.next();
                let arg = self.read_expr_part()?
                    .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(tok)))?;
                Node{head: Head::Lang(LangItem::Args(Box::new(head))), source: None, args: vec![arg]}
            },
            _ => Node{head, source: None, args: vec![]}
        }))
    }

    fn read_expr_part(&mut self) -> Result<Option<Expr>, ParseError<'str>> {
        let Some(tok) = self.tk.next().transpose()? else { return Ok(None); };
        use TokenClass as TC;
        Some(match tok {
            Token(TC::Number, value) => Ok(Item::new_number(value.parse::<BigInt>()
                .map_err(|_| ParseError::new("invalid number", value))?).into()),
            Token(TC::BaseNum, value) => Ok(Item::new_number(parse_basenum(value)?).into()),
            Token(TC::Bool, value) => Ok(Item::new_bool(value.parse::<bool>().unwrap()).into()),
            Token(TC::Char, value) => Ok(Item::new_char(parse_char(value)?).into()),
            Token(TC::String, value) => Ok(Item::new_stream(LiteralString::from(parse_string(value)?)).into()),
            Token(TC::Open, bkt @ "[") => Ok(Expr::new_lang(LangItem::List, None, self.read_args(bkt)?)),
            Token(TC::Ident, _) | Token(TC::Open, "{") => {
                self.tk.unread(tok);
                Ok(Expr::Eval(self.read_node()?.unwrap())) // cannot be None after unread()
            },
            Token(TC::Open, bkt @ "(") => {
                match &mut self.read_args(bkt)?[..] {
                    [e] => Ok(std::mem::take(e)),
                    [] => Err(ParseError::new("empty expression", self.tk.slice_from(bkt))),
                    _ => Err(ParseError::new("only one expression expected", self.tk.slice_from(bkt)))
                }
            },
            Token(TC::Special, chr) => {
                match self.tk.peek()? {
                    Some(&Token(TC::Number, _)) => {
                        let value = self.tk.next().unwrap()?.1;
                        match value.parse::<usize>() {
                            Ok(ix @ 1..) => Ok(Expr::new_repl(chr.as_bytes()[0].into(), Some(ix))),
                            Ok(0) => Err(ParseError::new("index can't be zero", self.tk.slice_from(chr))),
                            Err(_) => Err(ParseError::new("index too large", self.tk.slice_from(chr))),
                        }
                    },
                    _ => Ok(Expr::new_repl(chr.as_bytes()[0].into(), None))
                }
            },
            Token(_, tok) => Err(ParseError::new("cannot appear here", tok))
        }).transpose()
    }

    fn read_expr(&mut self) -> Result<Option<Expr>, ParseError<'str>> {
        use TokenClass as TC;

        struct StackEntry {
            op: String,
            prec: u32,
            args: Vec<Expr>
        }

        let mut stack: Vec<StackEntry> = vec![];
        let mut cur = None;

        'a: loop {
            //println!("{:?}", self.tk.peek()); // TODO
            let Some(tok) = self.tk.next().transpose()? else { break; };
            if tok.0 == TC::Close || tok.0 == TC::Comma {
                self.tk.unread(tok);
                break;
            }
            match (cur.take(), tok) {
                (Some(src), Token(TC::Chain, tok @ ".")) => {
                    let mut node = self.read_node()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(tok)))?;
                    assert!(node.source.is_none());
                    node.source = Some(Box::new(src));
                    cur = Some(Expr::Eval(node));
                },
                (Some(src), Token(TC::Chain, tok @ ":")) => {
                    let node = self.read_node()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(tok)))?;
                    assert!(node.source.is_none());
                    cur = Some(Expr::new_lang(LangItem::Map, Some(src), vec![Expr::Eval(node)]));
                },
                (Some(src), Token(TC::Open, bkt @ "[")) => {
                    let args = self.read_args(bkt)?;
                    if args.is_empty() {
                        return Err(ParseError::new("empty parts", self.tk.slice_from(bkt)));
                    }
                    cur = Some(Expr::new_lang(LangItem::Part, Some(src), args));
                },
                (None, Token(TC::Oper, sign @ ("+" | "-"))) => {
                    stack.push(StackEntry{op: sign.into(), prec: get_op(sign).0, args: vec![]});
                    cur = Some(self.read_expr_part()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(sign)))?);
                },
                (Some(mut expr), Token(TC::Oper, op)) => {
                    let (prec, multi) = get_op(op);
                    while let Some(entry) = stack.last_mut() {
                        if entry.prec > prec {
                            let mut entry = stack.pop().unwrap();
                            entry.args.push(expr);
                            expr = Expr::new_op(entry.op, None, entry.args);
                        } else if entry.prec == prec && entry.op == op && multi {
                            entry.args.push(expr);
                            continue 'a;
                        } else if entry.prec == prec {
                            let mut entry = stack.pop().unwrap();
                            entry.args.push(expr);
                            expr = Expr::new_op(entry.op, None, entry.args);
                            break;
                        } else {
                            break;
                        }
                    }
                    stack.push(StackEntry{op: op.into(), prec, args: vec![expr]});
                    cur = Some(self.read_expr_part()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(op)))?);
                },
                (None, tok) => {
                    self.tk.unread(tok);
                    cur = self.read_expr_part()?; // cannot be None after unread
                },
                (_, Token(_, tok)) => return Err(ParseError::new("cannot appear here", tok))
            }
        }
        let Some(mut ret) = cur else { return Ok(None) };
        while let Some(mut entry) = stack.pop() {
            entry.args.push(ret);
            ret = Expr::new_op(entry.op, None, entry.args);
        }
        Ok(Some(ret))
    }

    fn read_args(&mut self, bracket: &'str str) -> Result<Vec<Expr>, ParseError<'str>> {
        use TokenClass as TC;
        let mut ret = vec![];
        if let Some(expr) = self.read_expr()? {
            ret.push(expr);
            while self.tk.peek()? == Some(&Token(TC::Comma, ",")) {
                let comma = self.tk.next().unwrap()?; // can't fail after peek()
                let expr = self.read_expr()?
                    .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(comma.1)))?;
                ret.push(expr);
            }
        }
        match self.tk.next().transpose()? {
            Some(Token(TC::Close, bkt)) => {
                if bkt == closing(bracket) {
                    Ok(ret)
                } else {
                    Err(ParseError::new("unexpected closing bracket", bkt))
                }
            },
            Some(Token(TC::Comma, _)) =>
                Err(ParseError::new("incomplete expression", self.tk.slice_from(bracket))),
            None => Err(ParseError::new("incomplete expression", self.tk.slice_from(bracket))),
            x => panic!("unexpected token in read_args: {x:?}")
        }
    }

    pub fn parse(input: &'str str) -> Result<Expr, ParseError<'str>> {
        let mut parser = Parser{tk: Tokenizer::new(input)};
        let expr = parser.read_expr()?
            .ok_or(ParseError::new("empty input", input))?;
        if let Some(tok) = parser.tk.next().transpose()? {
            Err(ParseError::new("trailing characters", parser.tk.slice_from(tok.1)))
        } else {
            Ok(expr)
        }
    }
}

/// Parse a textual input into an [`Expr`].
///
/// # Examples
/// ```
/// use streamlang::base::*;
/// use streamlang::parser::parse;
/// assert_eq!(parse("a.b(3,4)"),
///     Ok(Expr::new_node("b",
///         Some(Expr::new_node("a", None, vec![])),
///         vec![Item::new_number(3).into(), Item::new_number(4).into()])));
/// ```
pub fn parse(input: &str) -> Result<Expr, ParseError<'_>> {
    Parser::parse(input)
}


#[test]
fn test_parser() {
    assert_eq!(parse("1"), Ok(Item::new_number(1).into()));
    assert_eq!(parse("a"), Ok(Expr::new_node("a", None, vec![])));
    assert_eq!(parse("a(1,2)"), Ok(Expr::new_node("a", None,
        vec![Item::new_number(1).into(), Item::new_number(2).into()])));
    assert_eq!(parse("1.a"), Ok(Expr::new_node("a", Some(Item::new_number(1).into()), vec![])));
    assert_eq!(parse("(1).a"), Ok(Expr::new_node("a", Some(Item::new_number(1).into()), vec![])));
    assert!(parse("(1,2).a").is_err());
    assert_eq!(parse("a.b"), Ok(Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![])));
    assert_eq!(parse("a.b.c"), Ok(Expr::new_node("c",
        Some(Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![])), vec![])));
    assert_eq!(parse("a(1).b(2)"), Ok(Expr::new_node("b",
        Some(Expr::new_node("a", None, vec![Item::new_number(1).into()])),
        vec![Item::new_number(2).into()])));

    assert!(parse("a.1").is_err());
    assert!(parse("2(a)").is_err());
    assert!(parse("1 2").is_err());
    assert!(parse("a b").is_err());
    assert!(parse("1,2").is_err());
    assert!(parse("   ").is_err());
    assert!(parse("1,").is_err());
    assert!(parse(",2").is_err());
    assert!(parse("a(1,,2)").is_err());
    assert!(parse("a(,2)").is_err());
    assert!(parse("a(1,)").is_err());
    assert!(parse("a.").is_err());
    assert_eq!(parse("a()"), parse("a"));
    assert!(parse("a()(1)").is_err());
    assert!(parse("(a)(1)").is_err());
    assert!(parse("a.(1)").is_err());
    assert_eq!(parse("(a)"), parse("a"));
    assert_eq!(parse("((a))"), parse("a"));
    assert_eq!(parse("a((1))"), parse("a(1)"));
    assert!(parse("a((1,2))").is_err());
    assert!(parse("(1]").is_err());
    assert!(parse("(1").is_err());
    assert!(parse("1)").is_err());
    assert!(parse("(1;2)").is_err());

    assert_eq!(parse("a.b..c.d"), Ok(Expr::new_op("..", None,
        vec![Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![]),
        Expr::new_node("d", Some(Expr::new_node("c", None, vec![])), vec![])])));
    assert_eq!(parse("a..b..c"), Ok(Expr::new_op("..", None,
        vec![Expr::new_op("..", None,
            vec![Expr::new_node("a", None, vec![]), Expr::new_node("b", None, vec![])]),
            Expr::new_node("c", None, vec![])])));

    assert_eq!(parse("{1}"), Ok(Expr::new_block(Item::new_number(1).into(), None, vec![])));
    assert!(parse("{}").is_err());
    assert_eq!(parse("1.{2}(3)"), Ok(Expr::new_block(Item::new_number(2).into(),
        Some(Item::new_number(1).into()), vec![Item::new_number(3).into()])));
    assert_eq!(parse("1.{2.a(3)}(4)"), Ok(Expr::new_block(
        Expr::new_node("a", Some(Item::new_number(2).into()), vec![Item::new_number(3).into()]),
        Some(Item::new_number(1).into()), vec![Item::new_number(4).into()])));
    assert_eq!(parse("{1}.{2}"), Ok(Expr::new_block(Item::new_number(2).into(),
        Some(Expr::new_block(Item::new_number(1).into(), None, vec![])), vec![])));

    assert_eq!(parse("[1,2][3,4]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_lang(LangItem::List, None, vec![Item::new_number(1).into(), Item::new_number(2).into()])),
        vec![Item::new_number(3).into(), Item::new_number(4).into()])));
    assert_eq!(parse("[1][2][3]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_lang(LangItem::Part,
            Some(Expr::new_lang(LangItem::List, None, vec![Item::new_number(1).into()])),
            vec![Item::new_number(2).into()])),
        vec![Item::new_number(3).into()])));
    assert_eq!(parse("[][3,4]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_lang(LangItem::List, None, vec![])),
        vec![Item::new_number(3).into(), Item::new_number(4).into()])));
    assert!(parse("[1,2][]").is_err());
    assert!(parse("a.[1]").is_err());
    // The following is legal syntax, but error at runtime
    assert_eq!(parse("1[2]"), Ok(Expr::new_lang(LangItem::Part, Some(Item::new_number(1).into()),
        vec![Item::new_number(2).into()])));
    assert_eq!(parse("a[b]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_node("a", None, vec![])), vec![Expr::new_node("b", None, vec![])])));
    assert_eq!(parse("[[1]][[2]]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_lang(LangItem::List, None, vec![Expr::new_lang(LangItem::List, None, vec![Item::new_number(1).into()])])),
        vec![Expr::new_lang(LangItem::List, None, vec![Item::new_number(2).into()])])));
    assert_eq!(parse("([([(1)])])"), parse("[[1]]"));
    assert_eq!(parse("([1])[2]"), parse("[1][2]"));
    assert!(parse("[1]([2])").is_err());

    assert!(parse("''").is_err());
    assert_eq!(parse("'\\n'"), Ok(Item::new_char('\n').into()));
    assert_eq!(parse("'\\\\'"), Ok(Item::new_char('\\').into()));
    assert_eq!(parse("'\\''"), Ok(Item::new_char('\'').into()));
    assert_eq!(parse("'\\\"'"), Ok(Item::new_char('"').into()));
    assert_eq!(parse("'\"'"), Ok(Item::new_char('"').into()));
    assert!(parse("'\\h'").is_err());
    assert_eq!(parse("true+'1'"), Ok(Expr::new_op("+", None, vec![
        Item::new_bool(true).into(), Item::new_char('1').into()])));

    assert_eq!(parse("#"), Ok(Expr::new_repl('#', None)));
    assert_eq!(parse("#1"), Ok(Expr::new_repl('#', Some(1))));
    assert!(parse("#0").is_err());
    assert!(parse("#18446744073709551616").is_err());
    assert!(parse("##").is_err());
    assert!(parse("#a").is_err());
    assert!(parse("#$").is_err());
    assert!(parse("$list").is_err()); // internal keyword
    assert!(parse("#(1)").is_err());
    assert_eq!(parse("#+$"), Ok(Expr::new_op("+", None, vec![
        Expr::new_repl('#', None), Expr::new_repl('$', None)])));
    assert!(parse("1.#").is_err());
    assert_eq!(parse("1.{#}(2)"), Ok(Expr::new_block(Expr::new_repl('#', None),
        Some(Item::new_number(1).into()), vec![Item::new_number(2).into()])));

    assert_eq!(parse("a.b@c@d[1]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("b".into()))),
            Some(Expr::new_node("a", None, vec![])),
            vec![Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("c".into()))), None,
                vec![Expr::new_node("d", None, vec![])])])),
        vec![Item::new_number(1).into()])));
    // literals, list, #, (x) may follow @
    assert_eq!(parse("a@1"), Ok(Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("a".into()))),
        None, vec![Item::new_number(1).into()])));
    assert_eq!(parse("a@[1,2]"), Ok(Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("a".into()))),
        None, vec![Expr::new_lang(LangItem::List, None,
            vec![Item::new_number(1).into(), Item::new_number(2).into()])])));
    assert_eq!(parse("a@(b.c@#)"), Ok(Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("a".into()))),
        None, vec![Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("c".into()))),
            Some(Expr::new_node("b", None, vec![])), vec![Expr::new_repl('#', None)])])));
    // but not (x,y)
    assert!(parse("a@(b.c@#,d)").is_err());
    // blocks allowed both before and after
    assert_eq!(parse("{a}@{b}"), Ok(Expr::new_lang(LangItem::Args(Box::new(Head::Block(Box::new(
            Expr::new_node("a", None, vec![]))))),
        None, vec![Expr::new_block(Expr::new_node("b", None, vec![]), None, vec![])])));
    // (c) binds to b but [d] to entire expression
    assert_eq!(parse("a@b(c)[d]"), Ok(Expr::new_lang(LangItem::Part,
        Some(Expr::new_lang(LangItem::Args(Box::new(Head::Symbol("a".into()))),
            None, vec![Expr::new_node("b", None, vec![Expr::new_node("c", None, vec![])])])),
        vec![Expr::new_node("d", None, vec![])])));
    // no @ after arguments
    assert!(parse("a(b)@c").is_err());
    assert!(parse("a@@c").is_err());
    assert!(parse("1@a").is_err());
    assert!(parse("a@").is_err());
}

#[test]
fn test_prec() {
    // base precedence tests
    // +(1, *(2, ^(3, 4)), 5)
    assert_eq!(parse("1+2*3^4+5"), Ok(Expr::new_op("+", None, vec![
        Item::new_number(1).into(),
        Expr::new_op("*", None, vec![
            Item::new_number(2).into(),
            Expr::new_op("^", None, vec![Item::new_number(3).into(), Item::new_number(4).into()])]),
        Item::new_number(5).into()
        ])));
    // +(1, *( ^(2, 3), 4), 5)
    assert_eq!(parse("1+2^3*4+5"), Ok(Expr::new_op("+", None, vec![
        Item::new_number(1).into(),
        Expr::new_op("*", None, vec![
            Expr::new_op("^", None, vec![Item::new_number(2).into(), Item::new_number(3).into()]),
            Item::new_number(4).into()]),
        Item::new_number(5).into()
        ])));
    // mixing + and -: same precedence, but - don't mix and stack
    // -( -( -( +(1, 2, 3), 4), 5), 6)
    assert_eq!(parse("1+2+3-4-5-6"), Ok(Expr::new_op("-", None, vec![
        Expr::new_op("-", None, vec![
            Expr::new_op("-", None, vec![
                Expr::new_op("+", None, vec![
                    Item::new_number(1).into(),
                    Item::new_number(2).into(),
                    Item::new_number(3).into()]),
                Item::new_number(4).into()]),
            Item::new_number(5).into()]),
        Item::new_number(6).into()])));
    // chaining takes precedence over everything
    // +(1, a.b, 2)
    assert_eq!(parse("1+a.b+2"), Ok(Expr::new_op("+", None, vec![
        Item::new_number(1).into(),
        Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![]),
        Item::new_number(2).into()])));
    // +(1, 2)
    assert_eq!(parse("+1+2"), Ok(Expr::new_op("+", None, vec![
        Item::new_number(1).into(),
        Item::new_number(2).into()])));
    // -( -(1), 2)
    assert_eq!(parse("-1-2"), Ok(Expr::new_op("-", None, vec![
        Expr::new_op("-", None, vec![Item::new_number(1).into()]),
        Item::new_number(2).into()])));
    // -(1..1) (error)
    assert_eq!(parse("-1..1"), Ok(Expr::new_op("-", None,
        vec![Expr::new_op("..", None, vec![Item::new_number(1).into(), Item::new_number(1).into()])])));
    assert!(parse("--1").is_err());
    assert!(parse("*1*2").is_err());
}
