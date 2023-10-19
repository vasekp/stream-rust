use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::{Display, Formatter, Debug};
use std::cell::RefCell;
use crate::base::{BaseError, Item, Expr, Char};
use num::BigInt;


/// The error type returned by [`parse`]. Contains the description of the error and its location
/// within the input string. The lifetime is bound to the lifetime of the input string.
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

    /// Shows the location of the parse error. For this purpose, the input string is reproduced in
    /// full. The part causing the error is highlighted using ANSI color sequences.
    ///
    /// For the actual description of the error, use the `Display` trait.
    pub fn display(&self, input: &'a str) {
        let start = unsafe { self.slice.as_ptr().offset_from(input.as_ptr()) } as usize;
        let length = self.slice.len();
        //println!("\x1b[8m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)]);
        println!("{}\x1b[1;31m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)], &input[(start+length)..]);
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
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
    const OPERS: &str = "+-*/%@^~&|<=>";
    let class = match slice.chars().next().unwrap() {
        '0'..='9' => if slice.contains('_') { BaseNum } else { Number },
        'a'..='z' | 'A'..='Z' | '_' => if slice == "true" || slice == "false" { Bool } else { Ident },
        '"' => String,
        '\'' => Char,
        '.' | ':' => Chain,
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
        Err(BaseError::from("unterminated string"))
    }

    fn slice_from(&mut self, start: &'a str) -> &'a str {
        let start_index = unsafe { start.as_ptr().offset_from(self.input.as_ptr()) } as usize;
        let end_index = match self.iter.peek() {
            Some(&(pos, _)) => pos,
            None => self.input.len()
        };
        &self.input[start_index..end_index]
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, ParseError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start, ch) = self.iter.next()?;
        if ch == ';' {
            return None;
        }
        let class = char_class(ch);
        use CharClass::*;
        let res = match class {
            Ident | Rel | Space => {
                self.skip_same(&class);
                Ok(())
            },
            Delim => self.skip_until(ch),
            Other => Ok(()),
            Comment => unreachable!()
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
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("unterminated string"))));
    assert_eq!(tk.next(), None);

    let mut tk = Tokenizer::new(r#"a"""d"#); // triple "
    assert_eq!(tk.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(tk.next(), Some(Ok(Token(String, "\"\""))));
    assert_eq!(tk.next(), Some(Err(ParseError::cmp_ref("unterminated string"))));
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
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ".")))); // should remain single character
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ".")))); // should remain separate
    assert_eq!(tk.next(), Some(Ok(Token(Chain, ":")))); // ditto
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

    let mut it = Tokenizer::new("a.b0_1(3_012,#4,true)");
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
    assert_eq!(it.next(), Some(Ok(Token(Close, ")"))));
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#""a'b"c'd"é'ř"#);
    assert_eq!(it.next(), Some(Ok(Token(String, "\"a'b\""))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "c"))));
    assert_eq!(it.next(), Some(Ok(Token(Char, "'d\"é'")))); // non-ASCII in quotes
    assert_eq!(it.next(), Some(Err(ParseError::cmp_ref("invalid character")))); // non-ASCII
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#"1;á"#);
    assert_eq!(it.next(), Some(Ok(Token(Number, "1"))));
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#"";";";"#);
    assert_eq!(it.next(), Some(Ok(Token(String, "\";\""))));
    assert_eq!(it.next(), None);
}


type PreExpr<'a> = Vec<ExprPart<'a>>;

#[derive(Debug)]
enum ExprPart<'a> {
    Oper(Token<'a>),
    Chain(Token<'a>),
    Term(TTerm<'a>),
    Part(Vec<Expr>)
}

#[derive(Debug)]
enum TTerm<'a> {
    Node(Node<'a>, Option<Vec<Expr>>),
    ParExpr(Box<Expr>),
    Literal(Literal<'a>),
    List(Vec<Expr>),
    Special(Token<'a>, Option<Token<'a>>)
}

#[derive(Debug)]
enum Node<'a> {
    Ident(Token<'a>),
    Block(Box<Expr>)
}

#[derive(Debug)]
enum Literal<'a> {
    Number(Token<'a>),
    BaseNum(Token<'a>),
    Bool(Token<'a>),
    Char(Token<'a>),
    String(Token<'a>)
}

fn closing(bracket: &str) -> &'static str {
    match bracket {
        "(" => ")",
        "[" => "]",
        "{" => "}",
        _ => unreachable!()
    }
}

fn parse_main<'a>(tk: &RefCell<Tokenizer<'a>>, bracket: Option<&'a str>) -> Result<Vec<Expr>, ParseError<'a>> {
    // This may parse several expressions separated by commas.
    let mut exprs: Vec<Expr> = vec![];
    // Each expression is a string of terms separated by operators OR chaining.
    let mut expr: PreExpr = vec![];
    let mut last_comma: Option<Token<'a>> = None;
    use ExprPart::*;
    use TTerm as TT;
    use TokenClass as TC;
    let slice_from = |start| tk.borrow_mut().slice_from(start);
    loop {
        // Get next token.
        let t = {
            // We need the borrow to be temporary and strictly localized as this fn is recursive.
            let mut tk = tk.borrow_mut();
            match tk.next() {
                // Some: test for ParseError
                Some(t0) => t0?,
                // None (end of input): test for unclosed brackets
                None => {
                    if let Some(open) = bracket {
                        return Err(ParseError::new(format!("missing close bracket: '{}'", closing(open)), tk.slice_from(open)));
                    }
                    // NB: after this break it's too late to do so
                    break;
                }
            }
        };
        // At this point t = Token(TokenClass, &str).

        let last = expr.last_mut();
        match (t.0, last, t.1) {
            // Immediate values
            (TC::Number | TC::BaseNum | TC::Bool | TC::String | TC::Char, Some(Oper(_)) | None, _)
                => expr.push(Term(match t {
                    Token(TC::Number, _) => TT::Literal(Literal::Number(t)),
                    Token(TC::BaseNum, _) => TT::Literal(Literal::BaseNum(t)),
                    Token(TC::Bool, _) => TT::Literal(Literal::Bool(t)),
                    Token(TC::String, _) => TT::Literal(Literal::String(t)),
                    Token(TC::Char, _) => TT::Literal(Literal::Char(t)),
                    _ => unreachable!()
                })),
            // Identifier: also can follow ., :
            (TC::Ident, Some(Oper(_) | Chain(_)) | None, _)
                => expr.push(Term(TT::Node(Node::Ident(t), None))),
            // Special use of numbers: # -> #1, $ -> $1
            (TC::Number, Some(Term(TT::Special(_, arg @ None))), _)
                => *arg = Some(t),
            // Operators (+, -, ...)
            (TC::Oper, Some(Term(_) | Part(_)), _)
                => expr.push(Oper(t)),
            // Special case: unary minus, plus can appear at start of expression
            (TC::Oper, None, "-" | "+")
                => expr.push(Oper(t)),
            // Chaining (., :)
            (TC::Chain, Some(Term(_) | Part(_)), _)
                => expr.push(Chain(t)),
            // Parenthesized expression
            (TC::Open, Some(Oper(_)) | None, "(") => {
                let vec = parse_main(tk, Some(t.1))?;
                match vec.len() {
                    1 => {
                        let e = vec.into_iter().next().unwrap();
                        expr.push(Term(TT::ParExpr(Box::new(e))));
                    },
                    0 => return Err(ParseError::new("empty expression", slice_from(t.1))),
                    _ => return Err(ParseError::new("only one expression expected", slice_from(t.1)))
                }
            },
            // Arguments to a node
            (TC::Open, Some(Term(TT::Node(_, args @ None))), "(")
                => *args = Some(parse_main(tk, Some(t.1))?),
            // List
            (TC::Open, Some(Oper(_)) | None, "[")
                => expr.push(Term(TT::List(parse_main(tk, Some(t.1))?))),
            // Parts construction
            (TC::Open, Some(Term(_)), "[") => {
                let vec = parse_main(tk, Some(t.1))?;
                if vec.is_empty() {
                    return Err(ParseError::new("empty parts", slice_from(t.1)));
                }
                expr.push(Part(vec));
            },
            // Block (expression used as a node)
            (TC::Open, Some(Oper(_)) | Some(Chain(_)) | None, "{") => {
                let vec = parse_main(tk, Some(t.1))?;
                match vec.len() {
                    1 => {
                        let e = vec.into_iter().next().unwrap();
                        expr.push(Term(TT::Node(Node::Block(Box::new(e)), None)));
                    },
                    0 => return Err(ParseError::new("empty block", slice_from(t.1))),
                    _ => return Err(ParseError::new("only one expression expected", slice_from(t.1)))
                }
            },
            // Closing bracket
            (TC::Close, _, _) => {
                let Some(open) = bracket else {
                    return Err(ParseError::new("unexpected closing bracket", t.1));
                };
                let close = closing(open);
                if t.1 == close {
                    break;
                } else {
                    return Err(ParseError::new(format!("wrong bracket: expected '{close}'"), t.1));
                }
            },
            // Separator of multiple exprs.
            // Checking whether >1 makes sense is caller's responsibility!
            (TC::Comma, Some(Term(_) | Part(_)), _) => {
                let mut new = vec![];
                std::mem::swap(&mut expr, &mut new);
                exprs.push(into_expr(new)?);
                last_comma = Some(t);
            },
            // Special characters #, $
            (TC::Special, Some(Oper(_) | Chain(_)) | None, _)
                => expr.push(Term(TT::Special(t, None))),
            _ => return Err(ParseError::new("cannot appear here", t.1))
        }
    }
    match expr.last() {
        Some(Term(_) | Part(_)) => exprs.push(into_expr(expr)?),
        Some(Oper(t) | Chain(t)) => return Err(ParseError::new("incomplete expression", slice_from(t.1))),
        None => if let Some(t) = last_comma {
            return Err(ParseError::new("incomplete expression", slice_from(t.1)));
        }
    }
    Ok(exprs)
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
    assert_eq!(parse_basenum("2_"), Err(ParseError::cmp_ref("malformed number")));
    assert_eq!(parse_basenum("2_1_"), Err(ParseError::cmp_ref("malformed number")));
    assert_eq!(parse_basenum("2_1_0"), Err(ParseError::cmp_ref("malformed number")));
    assert_eq!(parse_basenum("1_0"), Err(ParseError::cmp_ref("invalid base")));
    assert_eq!(parse_basenum("0_0"), Err(ParseError::cmp_ref("invalid base")));
    assert_eq!(parse_basenum("37_0"), Err(ParseError::cmp_ref("invalid base")));
    assert_eq!(parse_basenum("999999999999999999_0"), Err(ParseError::cmp_ref("invalid base")));
    assert_eq!(parse_basenum("2a_0"), Err(ParseError::cmp_ref("invalid base")));
    assert_eq!(parse_basenum("2_0"), Ok(BigInt::from(0)));
    assert_eq!(parse_basenum("2_101"), Ok(BigInt::from(5)));
    assert_eq!(parse_basenum("2_102"), Err(ParseError::cmp_ref("invalid digits in base 2")));
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

fn into_expr(input: PreExpr<'_>) -> Result<Expr, ParseError<'_>> {
    struct StackEntry {
        op: String,
        prec: u32,
        args: Vec<Expr>
    }

    debug_assert!(!input.is_empty());
    let mut stack: Vec<StackEntry> = vec![];
    let mut cur = None;
    'a: for part in input {
        use ExprPart::*;
        use TTerm as TT;
        use Node::*;
        use Literal::*;
        match (part, cur.take()) {
            (Term(TT::Literal(Number(tok))), None)
                => cur = Some(Item::new_number(tok.1.parse::<BigInt>()
                    .map_err(|_| ParseError::new("invalid number", tok.1))?).into()),
            (Term(TT::Literal(BaseNum(tok))), None)
                => cur = Some(Item::new_number(parse_basenum(tok.1)?).into()),
            (Term(TT::Literal(Bool(tok))), None)
                => cur = Some(Item::new_bool(tok.1.parse::<bool>().unwrap()).into()),
            (Term(TT::Literal(Char(tok))), None)
                => cur = Some(Item::new_char(parse_char(tok.1)?).into()),
            // TODO: string
            (Term(TT::List(vec)), None)
                => cur = Some(Expr::new_node("list", None, vec)),
            // TODO: Special
            (Term(TT::Node(Ident(tok), args)), src)
                => cur = Some(Expr::new_node(tok.1, src, args.unwrap_or(vec![]))),
            (Term(TT::Node(Block(body), args)), src)
                => cur = Some(Expr::new_block(*body, src, args.unwrap_or(vec![]))),
            (Term(TT::ParExpr(expr)), None)
                => cur = Some(*expr),
            (Chain(Token(_, ".")), prev)
                => cur = prev,
            // TODO: Chain(:)
            (Part(vec), src)
                => cur = Some(Expr::new_node("part", src, vec)),
            (Oper(Token(_, op)), Some(mut prev)) => {
                let (prec, multi) = get_op(op);
                while let Some(entry) = stack.last_mut() {
                    if entry.prec > prec {
                        let mut entry = stack.pop().unwrap();
                        entry.args.push(prev);
                        prev = Expr::new_node(entry.op, None, entry.args);
                    } else if entry.prec == prec && entry.op == op && multi {
                        entry.args.push(prev);
                        continue 'a;
                    } else if entry.prec == prec {
                        let mut entry = stack.pop().unwrap();
                        entry.args.push(prev);
                        prev = Expr::new_node(entry.op, None, entry.args);
                        break;
                    } else {
                        break;
                    }
                }
                stack.push(StackEntry{ op: op.into(), prec, args: vec![prev] });
            },
            (Oper(Token(_, op)), None) if op == "+" || op == "-"
                => stack.push(StackEntry{ op: op.into(), prec: get_op(op).0, args: vec![] }),
            _ => todo!()
        }
    }
    let mut ret = cur.unwrap();
    while let Some(mut entry) = stack.pop() {
        entry.args.push(ret);
        ret = Expr::new_node(entry.op, None, entry.args);
    }
    Ok(ret)
}

fn get_op(op: &str) -> (u32, bool) {
    match op {
        "+" => (1, true),
        "-" => (1, false),
        "*" => (2, true),
        "/" => (2, false),
        "^" => (3, false),
        _ => todo!()
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
pub fn parse(input: &str) -> Result<Expr, ParseError> {
    let mut it = parse_main(&RefCell::new(Tokenizer::new(input)), None)?.into_iter();
    match (it.next(), it.next()) {
        (Some(expr), None) => Ok(expr),
        (None, _) => Err(ParseError::new("empty input", input)),
        (Some(_), Some(_)) => Err(ParseError::new("multiple expressions", input))
    }
}

#[test]
fn test_parser() {
    let err = |text| Err(ParseError::cmp_ref(text));
    assert_eq!(parse("1"), Ok(Item::new_number(1).into()));
    assert_eq!(parse("a"), Ok(Expr::new_node("a", None, vec![])));
    assert_eq!(parse("a(1,2)"), Ok(Expr::new_node("a", None,
        vec![Item::new_number(1).into(), Item::new_number(2).into()])));
    assert_eq!(parse("1.a"), Ok(Expr::new_node("a", Some(Item::new_number(1).into()), vec![])));
    assert_eq!(parse("(1).a"), Ok(Expr::new_node("a", Some(Item::new_number(1).into()), vec![])));
    assert_eq!(parse("(1,2).a"), err("only one expression expected"));
    assert_eq!(parse("a.b"), Ok(Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![])));
    assert_eq!(parse("a.b.c"), Ok(Expr::new_node("c",
        Some(Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![])), vec![])));
    assert_eq!(parse("a(1).b(2)"), Ok(Expr::new_node("b",
        Some(Expr::new_node("a", None, vec![Item::new_number(1).into()])),
        vec![Item::new_number(2).into()])));

    let syntax_err = err("cannot appear here");
    assert_eq!(parse("a.1"), syntax_err);
    assert_eq!(parse("2(a)"), syntax_err);
    assert_eq!(parse("1 2"), syntax_err);
    assert_eq!(parse("a b"), syntax_err);
    assert_eq!(parse("1,2"), err("multiple expressions"));
    assert_eq!(parse("   "), err("empty input"));
    assert_eq!(parse("1,"), err("incomplete expression"));
    assert_eq!(parse(",2"), syntax_err);
    assert_eq!(parse("a(1,,2)"), syntax_err);
    assert_eq!(parse("a(,2)"), syntax_err);
    assert_eq!(parse("a(1,)"), err("incomplete expression"));
    assert_eq!(parse("a."), err("incomplete expression"));
    assert_eq!(parse("a()"), parse("a"));
    assert_eq!(parse("a()(1)"), syntax_err);
    assert_eq!(parse("(a)(1)"), syntax_err);
    assert_eq!(parse("(a)"), parse("a"));
    assert_eq!(parse("((a))"), parse("a"));
    assert_eq!(parse("a((1))"), parse("a(1)"));
    assert_eq!(parse("a((1,2))"), err("only one expression expected")); 
    assert_eq!(parse("(1]"), err("wrong bracket: expected ')'"));
    assert_eq!(parse("(1"), err("missing close bracket: ')'"));
    assert_eq!(parse("1)"), err("unexpected closing bracket"));
    assert_eq!(parse("(1;2)"), err("missing close bracket: ')'"));

    assert_eq!(parse("{1}"), Ok(Expr::new_block(Item::new_number(1).into(), None, vec![])));
    assert_eq!(parse("1.{2}(3)"), Ok(Expr::new_block(Item::new_number(2).into(),
        Some(Item::new_number(1).into()), vec![Item::new_number(3).into()])));
    assert_eq!(parse("1.{2.a(3)}(4)"), Ok(Expr::new_block(
        Expr::new_node("a", Some(Item::new_number(2).into()), vec![Item::new_number(3).into()]),
        Some(Item::new_number(1).into()), vec![Item::new_number(4).into()])));
    assert_eq!(parse("{1}.{2}"), Ok(Expr::new_block(Item::new_number(2).into(),
        Some(Expr::new_block(Item::new_number(1).into(), None, vec![])), vec![])));

    assert_eq!(parse("[1,2][3,4]"), Ok(Expr::new_node("part",
        Some(Expr::new_node("list", None, vec![Item::new_number(1).into(), Item::new_number(2).into()])),
        vec![Item::new_number(3).into(), Item::new_number(4).into()])));
    assert_eq!(parse("[][3,4]"), Ok(Expr::new_node("part",
        Some(Expr::new_node("list", None, vec![])),
        vec![Item::new_number(3).into(), Item::new_number(4).into()])));
    assert_eq!(parse("[1,2][]"), err("empty parts"));
    assert_eq!(parse("[1][2][3]"), syntax_err);
    assert_eq!(parse("a.[1]"), syntax_err);
    // The following is legal syntax, but error at runtime
    assert_eq!(parse("1[2]"), Ok(Expr::new_node("part", Some(Item::new_number(1).into()),
        vec![Item::new_number(2).into()])));
    assert_eq!(parse("a[b]"), Ok(Expr::new_node("part",
        Some(Expr::new_node("a", None, vec![])), vec![Expr::new_node("b", None, vec![])])));
    assert_eq!(parse("[[1]][[2]]"), Ok(Expr::new_node("part",
        Some(Expr::new_node("list", None, vec![Expr::new_node("list", None, vec![Item::new_number(1).into()])])),
        vec![Expr::new_node("list", None, vec![Item::new_number(2).into()])])));
    assert_eq!(parse("([([(1)])])"), parse("[[1]]"));
    assert_eq!(parse("([1])[2]"), parse("[1][2]"));
    assert_eq!(parse("[1]([2])"), syntax_err);

    assert_eq!(parse("''"), err("empty character"));
    assert_eq!(parse("'\\n'"), Ok(Item::new_char('\n').into()));
    assert_eq!(parse("'\\\\'"), Ok(Item::new_char('\\').into()));
    assert_eq!(parse("'\\h'"), err("invalid escape sequence"));
    assert_eq!(parse("true+'1'"), Ok(Expr::new_node("+", None, vec![
        Item::new_bool(true).into(), Item::new_char('1').into()])));
}

#[test]
fn test_prec() {
    let err = |text| Err(ParseError::cmp_ref(text));

    // base precedence tests
    // +(1, *(2, ^(3, 4)), 5)
    assert_eq!(parse("1+2*3^4+5"), Ok(Expr::new_node("+", None, vec![
        Item::new_number(1).into(),
        Expr::new_node("*", None, vec![
            Item::new_number(2).into(),
            Expr::new_node("^", None, vec![Item::new_number(3).into(), Item::new_number(4).into()])]),
        Item::new_number(5).into()
        ])));
    // +(1, *( ^(2, 3), 4), 5)
    assert_eq!(parse("1+2^3*4+5"), Ok(Expr::new_node("+", None, vec![
        Item::new_number(1).into(),
        Expr::new_node("*", None, vec![
            Expr::new_node("^", None, vec![Item::new_number(2).into(), Item::new_number(3).into()]),
            Item::new_number(4).into()]),
        Item::new_number(5).into()
        ])));
    // mixing + and -: same precedence, but - don't mix and stack
    // -( -( -( +(1, 2, 3), 4), 5), 6)
    assert_eq!(parse("1+2+3-4-5-6"), Ok(Expr::new_node("-", None, vec![
        Expr::new_node("-", None, vec![
            Expr::new_node("-", None, vec![
                Expr::new_node("+", None, vec![
                    Item::new_number(1).into(),
                    Item::new_number(2).into(),
                    Item::new_number(3).into()]),
                Item::new_number(4).into()]),
            Item::new_number(5).into()]),
        Item::new_number(6).into()])));
    // chaining takes precedence over everything
    // +(1, a.b, 2)
    assert_eq!(parse("1+a.b+2"), Ok(Expr::new_node("+", None, vec![
        Item::new_number(1).into(),
        Expr::new_node("b", Some(Expr::new_node("a", None, vec![])), vec![]),
        Item::new_number(2).into()])));
    // +(1, 2)
    assert_eq!(parse("+1+2"), Ok(Expr::new_node("+", None, vec![
        Item::new_number(1).into(),
        Item::new_number(2).into()])));
    // -( -(1), 2)
    assert_eq!(parse("-1-2"), Ok(Expr::new_node("-", None, vec![
        Expr::new_node("-", None, vec![Item::new_number(1).into()]),
        Item::new_number(2).into()])));
    assert_eq!(parse("*1*2"), err("cannot appear here"));
}
