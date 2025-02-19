use std::str::CharIndices;
use std::iter::Peekable;
use crate::base::*;


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

#[derive(PartialEq, Debug)]
struct Token<'str>(TokenClass, &'str str);

impl Token<'_> {
    fn new(slice: &str) -> Result<Token, ParseError> {
        use TokenClass::*;
        const OPERS: &[u8] = b"+-*/%^~&|<=>";
        let class = match slice.as_bytes() {
            [b'0'..=b'9', ..] => if slice.contains('_') { BaseNum } else { Number },
            b"true" | b"false" => Bool,
            [b'a'..=b'z' | b'A'..=b'Z' | b'_', ..] => Ident,
            [b'"', ..] => String,
            [b'\'', ..] => Char,
            b".." => Oper,
            b"..." => Chain,
            [b'.' | b':' | b'@'] => Chain,
            [c, ..] if OPERS.contains(c) => Oper,
            [b'(' | b'[' | b'{'] => Open,
            [b')' | b']' | b'}'] => Close,
            [b','] => Comma,
            [b'#'] | [b'$'] => Special,
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
        let res = match class {
            Ident | Rel => {
                self.skip_same(&class);
                Ok(())
            },
            Delim => self.skip_until(ch),
            Other => {
                if ch == '.' {
                    for _ in 1..=2 {
                        if matches!(self.iter.peek(), Some(&(_, '.'))) {
                            self.iter.next();
                        } else {
                            break;
                        }
                    }
                }
                Ok(())
            }
            Space => {
                self.skip_same(&class);
                return self.next();
            },
            Comment => {
                self.iter = "".char_indices().peekable();
                return None
            }
        };
        let end = match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        };
        let slice = &self.input[start..end];
        Some(res
            .map_err(|reason| ParseError::new(reason, slice))
            .and_then(|_| Token::new(slice)))
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

    let mut tk = Tokenizer::new("....."); // ., .., ... bunching
    assert_eq!(tk.next(), Some(Ok(Token(Chain, "...")))); // greedy
    assert_eq!(tk.next(), Some(Ok(Token(Oper, "..")))); // three are Chain, two are Oper

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


fn parse_basenum(slice: &str) -> Result<Number, ParseError<'_>> {
    match slice.split('_').collect::<Vec<_>>()[..] {
        [base_str, value_str] if !value_str.is_empty() => {
            let base = base_str.parse::<u32>()
                .map_err(|_| ParseError::new("invalid base", base_str))?;
            if !matches!(base, 2..=36) {
                return Err(ParseError::new("invalid base", base_str));
            }
            Number::parse_bytes(value_str.as_bytes(), base)
                .ok_or(ParseError::new(format!("invalid digits in base {base}"), value_str))
        },
        _ => Err(ParseError::new("malformed number", slice))
    }
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
    assert_eq!(parse_basenum("2_0"), Ok(Number::zero()));
    assert_eq!(parse_basenum("2_101"), Ok(Number::from(5)));
    assert!(parse_basenum("2_102").is_err()); // invalid digits in base 2
    assert_eq!(parse_basenum("10_999999999999999999999999"), Ok("999999999999999999999999".parse().unwrap()));
    assert_eq!(parse_basenum("16_fffFFffFFfFfFFFFffFF"), Ok("1208925819614629174706175".parse().unwrap()));
}

fn parse_string(slice: &str) -> Result<String, ParseError<'_>> {
    let mut ret = String::new();
    // First and last characters are guaranteed to be ' or " and thus single-byte
    let inner = &slice[1..(slice.len() - 1)];
    let mut it = inner.char_indices().peekable();
    while let Some((index, c)) = it.next() {
        if c == '\\' {
            match it.next().unwrap().1 { // \ is guaranteed to be followed by at least 1 char
                d @ ('\\' | '\'' | '"') => ret.push(d),
                'n' => ret.push('\n'),
                'r' => ret.push('\r'),
                't' => ret.push('\t'),
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
    fn read_link(&mut self) -> Result<Option<Link>, ParseError<'str>> {
        let Some(tok) = self.tk.next_tr()? else { return Ok(None); };
        use TokenClass as TC;
        let head = match tok {
            Token(TC::Ident, name) => Head::Symbol(name.into()),
            Token(TC::Open, bkt @ "{") => Head::Block(Box::new(self.read_arg(bkt)?)),
            Token(_, tok) => return Err(ParseError::new("cannot appear here", tok))
        };
        Ok(Some(match self.tk.peek()? {
            Some(&Token(TC::Open, bkt @ "(")) => {
                self.tk.next();
                Link::new(head, self.read_args(bkt)?)
            },
            Some(&Token(TC::Chain, tok @ "@")) => {
                self.tk.next();
                let arg = self.read_expr_part()?
                    .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(tok)))?;
                Link::new(Head::args(head), vec![arg])
            },
            _ => Link::new(head, vec![])
        }))
    }

    fn read_expr_part(&mut self) -> Result<Option<Expr>, ParseError<'str>> {
        let Some(tok) = self.tk.next_tr()? else { return Ok(None); };
        use TokenClass as TC;
        Some(match tok {
            Token(TC::Number, value) => Ok(Expr::new_number(value.parse::<Number>()
                .map_err(|_| ParseError::new("invalid number", value))?)),
            Token(TC::BaseNum, value) => Ok(Expr::new_number(parse_basenum(value)?)),
            Token(TC::Bool, value) => Ok(Expr::new_bool(value.parse().unwrap())),
            Token(TC::Char, value) => Ok(Expr::new_char(parse_char(value)?)),
            Token(TC::String, value) => Ok(Expr::new_string(parse_string(value)?)),
            Token(TC::Open, bkt @ "[") => Ok(Expr::new_node(LangItem::List, self.read_args(bkt)?)),
            Token(TC::Ident, _) | Token(TC::Open, "{") => {
                self.tk.unread(tok);
                Ok(self.read_link()?.unwrap().into()) // cannot be None after unread()
            },
            Token(TC::Open, bkt @ "(") => Ok(self.read_arg(bkt)?),
            Token(TC::Special, chr) => {
                match self.tk.peek()? {
                    Some(&Token(TC::Number, value)) => {
                        self.tk.next();
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

        struct StackEntry<'str> {
            op: &'str str,
            prec: u32,
            args: Vec<Expr>
        }
        let mut stack: Vec<StackEntry> = vec![];

        let mut cur = match self.tk.next_tr()? {
            None => return Ok(None),
            Some(tok @ Token(TC::Close | TC::Comma, _)) => {
                self.tk.unread(tok);
                return Ok(None)
            },
            Some(Token(TC::Oper, sign @ ("+" | "-"))) => {
                stack.push(StackEntry{op: sign, prec: get_op(sign).0, args: vec![]});
                self.read_expr_part()?
                    .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(sign)))?
            },
            Some(tok) => {
                self.tk.unread(tok);
                self.read_expr_part()?.unwrap()
            }
        };

        loop {
            let Some(tok) = self.tk.next_tr()? else { break; };
            if tok.0 == TC::Close || tok.0 == TC::Comma {
                self.tk.unread(tok);
                break;
            }
            cur = match (cur, tok) {
                (src, Token(TC::Chain, tok @ ".")) => {
                    let node = self.read_link()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(tok)))?;
                    src.chain(node)
                },
                (src, Token(TC::Chain, tok @ ":")) => {
                    let node = self.read_link()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(tok)))?;
                    src.chain(Link::new(LangItem::Map, vec![node.into()]))
                },
                (src, Token(TC::Chain, "...")) =>
                    src.chain(Link::new("repeat", vec![])),
                (src, Token(TC::Open, bkt @ "[")) => {
                    let args = self.read_args(bkt)?;
                    if args.is_empty() {
                        return Err(ParseError::new("empty parts", self.tk.slice_from(bkt)));
                    }
                    src.chain(Link::new(LangItem::Part, args))
                },
                (mut expr, Token(TC::Oper, op)) => {
                    let (prec, multi) = get_op(op);
                    loop {
                        let Some(mut entry) = stack.pop() else {
                            // No stack: start new
                            stack.push(StackEntry{op, prec, args: vec![expr]});
                            break;
                        };
                        if entry.prec > prec {
                            // Higher precedence: add current expr, wrap up and replace expr by the result
                            entry.args.push(expr);
                            expr = Expr::new_op(entry.op, entry.args);
                            // Continue down the stack!
                        } else if entry.prec == prec && entry.op == op && multi {
                            // We have the same operator, and it allows multiple parameters:
                            // add current expr to them and put entry back
                            entry.args.push(expr);
                            stack.push(entry);
                            break;
                        } else if entry.prec == prec {
                            // Same precedence but no bunching: finish old operation and
                            // replace stack top by the new one
                            entry.args.push(expr);
                            expr = Expr::new_op(entry.op, entry.args);
                            stack.push(StackEntry{op, prec, args: vec![expr]});
                            break;
                        } else {
                            // Lower precedence: keep the previous stack and add new op on top
                            stack.push(entry);
                            stack.push(StackEntry{op, prec, args: vec![expr]});
                            break;
                        }
                    }
                    self.read_expr_part()?
                        .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(op)))?
                },
                (_, Token(_, tok)) => return Err(ParseError::new("cannot appear here", tok))
            }
        }
        while let Some(mut entry) = stack.pop() {
            entry.args.push(cur);
            cur = Expr::new_op(entry.op, entry.args);
        }
        Ok(Some(cur))
    }

    fn read_arg(&mut self, open: &'str str) -> Result<Expr, ParseError<'str>> {
        let expr = self.read_expr()?
            .ok_or(ParseError::new("empty expression", self.tk.slice_from(open)))?;
        let next = self.tk.next_tr()?
            .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(open)))?;
        let Token(TokenClass::Close, close) = next else {
            return Err(ParseError::new("cannot appear here", next.1));
        };
        match (open, close) {
            ("(", ")") | ("[", "]") | ("{", "}") => Ok(expr),
            (_, close) => Err(ParseError::new("unexpected closing bracket", close))
        }
    }

    fn read_args(&mut self, open: &'str str) -> Result<Vec<Expr>, ParseError<'str>> {
        use TokenClass as TC;
        let mut ret = vec![];
        let close = loop {
            let expr = self.read_expr()?;
            let next = self.tk.next_tr()?
                .ok_or(ParseError::new("incomplete expression", self.tk.slice_from(open)))?;
            match (expr, next) {
                (Some(expr), Token(TC::Comma, ",")) => ret.push(expr),
                (None, Token(TC::Comma, tok)) => return Err(ParseError::new("empty expression", tok)),
                (Some(expr), Token(TC::Close, close)) => {
                    ret.push(expr);
                    break close;
                },
                (None, Token(TC::Close, close)) => {
                    if ret.is_empty() {
                        break close;
                    } else {
                        return Err(ParseError::new("empty expression", close));
                    }
                },
                _ => unreachable!()
            }
        };
        match (open, close) {
            ("(", ")") | ("[", "]") | ("{", "}") => Ok(ret),
            (_, close) => Err(ParseError::new("unexpected closing bracket", close))
        }
    }

    pub fn parse(input: &'str str) -> Result<Expr, ParseError<'str>> {
        let mut parser = Parser{tk: Tokenizer::new(input)};
        let expr = parser.read_expr()?
            .ok_or(ParseError::new("empty input", input))?;
        if let Some(tok) = parser.tk.next_tr()? {
            Err(ParseError::new("cannot appear here", tok.1))
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
///     Ok(Expr::new_node("a", vec![])
///         .chain(Link::new("b", vec![Expr::new_number(3), Expr::new_number(4)]))));
/// ```
pub fn parse(input: &str) -> Result<Expr, ParseError<'_>> {
    Parser::parse(input)
}


#[test]
fn test_parser() {
    assert_eq!(parse("1"), Ok(Expr::new_number(1)));
    assert_eq!(parse("a"), Ok(Expr::new_node("a", vec![])));
    assert_eq!(parse("a(1,2)"), Ok(Expr::new_node("a", vec![Expr::new_number(1), Expr::new_number(2)])));
    assert_eq!(parse("1.a"), Ok(Expr::new_number(1).chain(Link::new("a", vec![]))));
    assert_eq!(parse("(1).a"), Ok(Expr::new_number(1).chain(Link::new("a", vec![]))));
    assert!(parse("(1,2).a").is_err());
    assert_eq!(parse("a.b"), Ok(Expr::new_node("a", vec![]).chain(Link::new("b", vec![]))));
    assert_eq!(parse("a.b.c"), Ok(Expr::new_node("a", vec![]).chain(Link::new("b", vec![]))
        .chain(Link::new("c", vec![]))));
    assert_eq!(parse("a(1).b(2)"), Ok(Expr::new_node("a", vec![Expr::new_number(1)])
        .chain(Link::new("b", vec![Expr::new_number(2)]))));
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
    assert_eq!(parse("a()").unwrap(), parse("a").unwrap());
    assert!(parse("a()(1)").is_err());
    assert!(parse("(a)(1)").is_err());
    assert!(parse("a.(1)").is_err());
    assert_eq!(parse("(a)").unwrap(), parse("a").unwrap());
    assert_eq!(parse("((a))").unwrap(), parse("a").unwrap());
    assert_eq!(parse("a((1))").unwrap(), parse("a(1)").unwrap());
    assert!(parse("a((1,2))").is_err());
    assert!(parse("(1]").is_err());
    assert!(parse("(1").is_err());
    assert!(parse("1)").is_err());
    assert!(parse("(1;2)").is_err());

    assert_eq!(parse("a.b..c.d"), Ok(Expr::new_op("..", vec![
        Expr::new_node("a", vec![]).chain(Link::new("b", vec![])),
        Expr::new_node("c", vec![]).chain(Link::new("d", vec![]))])));
    assert_eq!(parse("a..b..c"), Ok(Expr::new_op("..", vec![
        Expr::new_op("..", vec![Expr::new_node("a", vec![]), Expr::new_node("b", vec![])]),
        Expr::new_node("c", vec![])])));

    assert_eq!(parse("{1}"), Ok(Expr::new_node(Expr::new_number(1), vec![])));
    assert!(parse("{}").is_err());
    assert_eq!(parse("1.{2}(3)"), Ok(Expr::new_number(1)
        .chain(Link::new(Expr::new_number(2), vec![Expr::new_number(3)]))));
    assert_eq!(parse("1.{2.a(3)}(4)"), Ok(Expr::new_number(1).chain(Link::new(
        Expr::new_number(2).chain(Link::new("a", vec![Expr::new_number(3)])),
        vec![Expr::new_number(4)]))));
    assert_eq!(parse("{1}.{2}"), Ok(Expr::new_node(Expr::new_number(1), vec![])
        .chain(Link::new(Expr::new_number(2), vec![]))));

    assert_eq!(parse("[1,2][3,4]"), Ok(Expr::new_node(LangItem::List,
            vec![Expr::new_number(1), Expr::new_number(2)])
        .chain(Link::new(LangItem::Part,
            vec![Expr::new_number(3), Expr::new_number(4)]))));
    assert_eq!(parse("[1][2][3]"), Ok(Expr::new_node(LangItem::List, vec![Expr::new_number(1)])
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(2)]))
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(3)]))));
    assert_eq!(parse("[][3,4]"), Ok(Expr::new_node(LangItem::List, vec![])
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(3), Expr::new_number(4)]))));
    assert!(parse("[1,2][]").is_err());
    assert!(parse("a.[1]").is_err());
    // The following is legal syntax, but error at runtime
    assert_eq!(parse("1[2]"), Ok(Expr::new_number(1)
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(2)]))));
    assert_eq!(parse("a[b]"), Ok(Expr::new_node("a", vec![])
        .chain(Link::new(LangItem::Part, vec![Expr::new_node("b", vec![])]))));
    assert_eq!(parse("[[1]][[2]]"), Ok(Expr::new_node(LangItem::List,
            vec![Expr::new_node(LangItem::List, vec![Expr::new_number(1)])])
        .chain(Link::new(LangItem::Part,
            vec![Expr::new_node(LangItem::List, vec![Expr::new_number(2)])]))));
    assert_eq!(parse("([([(1)])])").unwrap(), parse("[[1]]").unwrap());
    assert_eq!(parse("([1])[2]").unwrap(), parse("[1][2]").unwrap());
    assert!(parse("[1]([2])").is_err());

    assert!(parse("''").is_err());
    assert!(parse("'a''").is_err());
    assert_eq!(parse("'\\n'"), Ok(Expr::new_char('\n')));
    assert_eq!(parse("'\\\\'"), Ok(Expr::new_char('\\')));
    assert_eq!(parse("'\\''"), Ok(Expr::new_char('\'')));
    assert_eq!(parse("'\\\"'"), Ok(Expr::new_char('"')));
    assert_eq!(parse("'\"'"), Ok(Expr::new_char('"')));
    assert!(parse("'\\h'").is_err());
    assert_eq!(parse("true+'1'"), Ok(Expr::new_op("+",
        vec![Expr::new_bool(true), Expr::new_char('1')])));
    assert_eq!(parse(r#""""#), Ok(Expr::new_string("")));
    assert_eq!(parse(r#""abc""#), Ok(Expr::new_string("abc")));
    assert_eq!(parse(r#""'\'\"""#), Ok(Expr::new_string("''\"")));
    assert!(parse(r#"""""#).is_err());

    assert_eq!(parse("#"), Ok(Expr::new_repl('#', None)));
    assert_eq!(parse("#1"), Ok(Expr::new_repl('#', Some(1))));
    assert!(parse("#0").is_err());
    assert!(parse("#18446744073709551616").is_err());
    assert!(parse("##").is_err());
    assert!(parse("#a").is_err());
    assert!(parse("#$").is_err());
    assert!(parse("$list").is_err()); // internal keyword
    assert!(parse("#(1)").is_err());
    assert_eq!(parse("#+$"), Ok(Expr::new_op("+",
        vec![Expr::new_repl('#', None), Expr::new_repl('$', None)])));
    assert!(parse("1.#").is_err());
    assert_eq!(parse("1.{#}(2)"), Ok(Expr::new_number(1)
        .chain(Link::new(Expr::new_repl('#', None), vec![Expr::new_number(2)]))));

    assert_eq!(parse("a.b@c@d[1]"), Ok(Expr::new_node("a", vec![])
        .chain(Link::new(Head::args("b"),
            vec![Expr::new_node(Head::args("c"), vec![Expr::new_node("d", vec![])])]))
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(1)]))));
    // literals, list, #, (x) may follow @
    assert_eq!(parse("a@1"), Ok(Expr::new_node(Head::args("a"),
        vec![Expr::new_number(1)])));
    assert_eq!(parse("a@[1,2]"), Ok(Expr::new_node(Head::args("a"),
        vec![Expr::new_node(LangItem::List, vec![Expr::new_number(1), Expr::new_number(2)])])));
    assert_eq!(parse("a@(b.c@#)"), Ok(Expr::new_node(Head::args("a"),
        vec![Expr::new_node("b", vec![])
            .chain(Link::new(Head::args("c"), vec![Expr::new_repl('#', None)]))])));
    // but not (x,y)
    assert!(parse("a@(b.c@#,d)").is_err());
    // blocks allowed both before and after
    assert_eq!(parse("{a}@{b}"), Ok(Expr::new_node(Head::args(Expr::new_node("a", vec![])),
        vec![Expr::new_node(Expr::new_node("b", vec![]), vec![])])));
    // (c) binds to b but [d] to entire expression
    assert_eq!(parse("a@b(c)[d]"), Ok(Expr::new_node(Head::args("a"),
            vec![Expr::new_node("b", vec![Expr::new_node("c", vec![])])])
        .chain(Link::new(LangItem::Part, vec![Expr::new_node("d", vec![])]))));
    // no @ after arguments
    assert!(parse("a(b)@c").is_err());
    assert!(parse("a@@c").is_err());
    assert!(parse("1@a").is_err());
    assert!(parse("a@").is_err());

    assert_eq!(parse("1..2"), Ok(Expr::new_op("..", vec![Expr::new_number(1), Expr::new_number(2)])));
    assert_eq!(parse("1..."), Ok(Expr::new_number(1).chain(Link::new("repeat", vec![]))));
    assert_eq!(parse("1....len"), Ok(Expr::new_number(1)
            .chain(Link::new("repeat" , vec![])).chain(Link::new("len", vec![]))));
    assert!(parse("1....").is_err());
    assert!(parse("1...(2)").is_err());
    assert!(parse("...").is_err());
}

#[test]
fn test_prec() {
    // base precedence tests
    // +(1, *(2, ^(3, 4)), 5)
    assert_eq!(parse("1+2*3^4+5"), Ok(Expr::new_op("+", vec![
        Expr::new_number(1),
        Expr::new_op("*", vec![
            Expr::new_number(2),
            Expr::new_op("^", vec![Expr::new_number(3), Expr::new_number(4)])]),
        Expr::new_number(5)
        ])));
    // +(1, *( ^(2, 3), 4), 5)
    assert_eq!(parse("1+2^3*4+5"), Ok(Expr::new_op("+", vec![
        Expr::new_number(1),
        Expr::new_op("*", vec![
            Expr::new_op("^", vec![Expr::new_number(2), Expr::new_number(3)]),
            Expr::new_number(4)]),
        Expr::new_number(5)
        ])));
    // mixing + and -: same precedence, but - don't mix and stack
    // -( -( -( +(1, 2, 3), 4), 5), 6)
    assert_eq!(parse("1+2+3-4-5-6"), Ok(Expr::new_op("-", vec![
        Expr::new_op("-", vec![
            Expr::new_op("-", vec![
                Expr::new_op("+", vec![Expr::new_number(1), Expr::new_number(2), Expr::new_number(3)]),
                Expr::new_number(4)]),
            Expr::new_number(5)]),
        Expr::new_number(6)])));
    // chaining takes precedence over everything
    // +(1, a.b, 2)
    assert_eq!(parse("1+a.b+2"), Ok(Expr::new_op("+", vec![
        Expr::new_number(1),
        Expr::new_node("a", vec![]).chain(Link::new("b", vec![])),
        Expr::new_number(2)])));
    // +(1, 2)
    assert_eq!(parse("+1+2"), Ok(Expr::new_op("+", vec![Expr::new_number(1), Expr::new_number(2)])));
    // -( -(1), 2)
    assert_eq!(parse("-1-2"), Ok(Expr::new_op("-", vec![
        Expr::new_op("-", vec![Expr::new_number(1)]),
        Expr::new_number(2)])));
    // -(1..1) (error)
    assert_eq!(parse("-1..1"), Ok(Expr::new_op("-",
        vec![Expr::new_op("..", vec![Expr::new_number(1), Expr::new_number(1)])])));
    assert!(parse("--1").is_err());
    assert!(parse("*1*2").is_err());
}
