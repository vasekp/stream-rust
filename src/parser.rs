use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::{Display, Formatter, Debug};
use std::cell::RefCell;
use crate::base;
use crate::base::{BaseError, Expr, Item, Core};
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

#[derive(PartialEq, Debug, Clone, Copy)]
enum TokenClass {
    Number,
    BaseNum,
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
        '0'..='9' => if slice.contains('_') {
                BaseNum
            } else {
                Number
            },
        'a'..='z' | 'A'..='Z' | '_' => Ident,
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
        if let Some((start, ch)) = self.iter.next() {
            let class = char_class(ch);
            use CharClass::*;
            let res = match class {
                Ident | Rel | Space => {
                    self.skip_same(&class);
                    Ok(())
                },
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

    let mut it = Tokenizer::new("a.b0_1(3_012,#4)");
    assert_eq!(it.next(), Some(Ok(Token(Ident, "a"))));
    assert_eq!(it.next(), Some(Ok(Token(Chain, "."))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "b0_1"))));
    assert_eq!(it.next(), Some(Ok(Token(Open, "("))));
    assert_eq!(it.next(), Some(Ok(Token(BaseNum, "3_012"))));
    assert_eq!(it.next(), Some(Ok(Token(Comma, ","))));
    assert_eq!(it.next(), Some(Ok(Token(Special, "#"))));
    assert_eq!(it.next(), Some(Ok(Token(Number, "4"))));
    assert_eq!(it.next(), Some(Ok(Token(Close, ")"))));
    assert_eq!(it.next(), None);

    let mut it = Tokenizer::new(r#""a'b"c'd"é'ř"#);
    assert_eq!(it.next(), Some(Ok(Token(String, "\"a'b\""))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "c"))));
    assert_eq!(it.next(), Some(Ok(Token(Char, "'d\"é'")))); // non-ASCII in quotes
    assert_eq!(it.next(), Some(Err(ParseError::cmp_ref("invalid character")))); // non-ASCII
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
            (TC::Number | TC::BaseNum | TC::String | TC::Char, Some(Oper(_)) | None, _)
                => expr.push(Term(match t {
                    Token(TC::Number, _) => TT::Literal(Literal::Number(t)),
                    Token(TC::BaseNum, _) => TT::Literal(Literal::BaseNum(t)),
                    Token(TC::String, _) => TT::Literal(Literal::String(t)),
                    Token(TC::Char, _) => TT::Literal(Literal::Char(t)),
                    Token(TC::Ident, "true" | "false") => TT::Literal(Literal::Bool(t)),
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
                if let Some(open) = bracket {
                    let close = closing(open);
                    if t.1 == close {
                        break;
                    } else {
                        return Err(ParseError::new(format!("wrong bracket: expected '{close}'"), t.1));
                    }
                } else {
                    return Err(ParseError::new("unexpected closing bracket", t.1));
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

fn into_expr(input: PreExpr<'_>) -> Result<Expr, ParseError<'_>> {
    assert!(!input.is_empty());
    //let mut stack = vec![];
    let mut cur = None;
    for part in input {
        use ExprPart::*;
        use TTerm as TT;
        use Node::*;
        use Literal::*;
        use Expr::*;
        match (part, cur.take()) {
            (Term(TT::Literal(Number(tok))), None)
                => cur = Some(Imm(Item::new_atomic(tok.1.parse::<BigInt>()
                    .map_err(|_| ParseError::new("invalid number", tok.1))?))),
            (Term(TT::Literal(BaseNum(tok))), None)
                => cur = Some(Imm(Item::new_atomic(parse_basenum(tok.1)?))),
            // TODO: all other value types
            // TODO: list
            // TODO: Special
            (Term(TT::Node(Ident(tok), args)), src)
                => cur = Some(Eval(base::Node{
                    core: Core::Simple(tok.1.into()),
                    source: src.map(Box::new),
                    args: args.unwrap_or(vec![])
                })),
            (Term(TT::Node(Block(body), args)), src)
                => cur = Some(Eval(base::Node{
                    core: Core::Block(body),
                    source: src.map(Box::new),
                    args: args.unwrap_or(vec![])
                })),
            (Term(TT::ParExpr(expr)), None)
                => cur = Some(*expr),
            (Chain(Token(_, ".")), prev)
                => cur = prev,
            // TODO: Chain(:)
            // TODO: Part
            // TODO: all Opers, priority
            _ => todo!()
        }
    }
    Ok(cur.unwrap())
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

/// Parse a textual input into an [`Expr`].
///
/// # Examples
/// ```
/// use streamlang::base::*;
/// use streamlang::parser::parse;
/// assert_eq!(parse("a.b(3,4)"),
///     Ok(Expr::Eval(Node{
///         core: Core::Simple("b".to_string()),
///         source: Some(Box::new(Expr::Eval(Node{
///             core: Core::Simple("a".to_string()),
///             source: None,
///             args: vec![]}))),
///         args: vec![
///             Expr::Imm(Item::new_atomic(3)),
///             Expr::Imm(Item::new_atomic(4))
///         ]}))
///     );
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
    use Expr::*;
    use Core::*;
    use base::Node;

    assert_eq!(parse("1"), Ok(Imm(Item::new_atomic(1))));
    assert_eq!(parse("a"), Ok(Eval(Node{
        core: Simple("a".to_string()), source: None, args: vec![]})));
    assert_eq!(parse("a(1,2)"), Ok(Eval(Node{
        core: Simple("a".to_string()),
        source: None, args: vec![Imm(Item::new_atomic(1)), Imm(Item::new_atomic(2))]})));
    assert_eq!(parse("1.a"), Ok(Eval(Node{
        core: Simple("a".to_string()),
        source: Some(Box::new(Imm(Item::new_atomic(1)))), args: vec![]})));
    assert_eq!(parse("(1).a"), Ok(Eval(Node{
        core: Simple("a".to_string()),
        source: Some(Box::new(Imm(Item::new_atomic(1)))), args: vec![]})));
    assert_eq!(parse("(1,2).a"), Err(ParseError::cmp_ref("only one expression expected")));
    assert_eq!(parse("a.b"), Ok(Eval(Node{
        core: Simple("b".to_string()),
        source: Some(Box::new(Eval(Node{ core: Simple("a".to_string()), source: None, args: vec![] }))),
        args: vec![] })));
    assert_eq!(parse("a.b.c"), Ok(Eval(Node{
        core: Simple("c".to_string()),
        source: Some(Box::new(Eval(Node{
            core: Simple("b".to_string()),
            source: Some(Box::new(Eval(Node{
                core: Simple("a".to_string()),
                source: None,
                args: vec![] }))),
            args: vec![] }))),
        args: vec![] })));
    assert_eq!(parse("a(1).b(2)"), Ok(Eval(Node{
        core: Simple("b".to_string()),
        source: Some(Box::new(Eval(Node{
            core: Simple("a".to_string()),
            source: None,
            args: vec![Imm(Item::new_atomic(1))] }))),
        args: vec![Imm(Item::new_atomic(2))] })));

    let syntax_err = Err(ParseError::cmp_ref("cannot appear here"));
    assert_eq!(parse("a.1"), syntax_err);
    assert_eq!(parse("2(a)"), syntax_err);
    assert_eq!(parse("1 2"), syntax_err);
    assert_eq!(parse("a b"), syntax_err);
    assert_eq!(parse("1,2"), Err(ParseError::cmp_ref("multiple expressions")));
    assert_eq!(parse("   "), Err(ParseError::cmp_ref("empty input")));
    assert_eq!(parse("1,"), Err(ParseError::cmp_ref("incomplete expression")));
    assert_eq!(parse(",2"), syntax_err);
    assert_eq!(parse("a(1,,2)"), syntax_err);
    assert_eq!(parse("a(,2)"), syntax_err);
    assert_eq!(parse("a(1,)"), Err(ParseError::cmp_ref("incomplete expression")));
    assert_eq!(parse("a."), Err(ParseError::cmp_ref("incomplete expression")));
    assert_eq!(parse("a()"), parse("a"));
    assert_eq!(parse("a()(1)"), syntax_err);
    assert_eq!(parse("(a)(1)"), syntax_err);
    assert_eq!(parse("(a)"), parse("a"));
    assert_eq!(parse("((a))"), parse("a"));
    assert_eq!(parse("a((1))"), parse("a(1)"));
    assert_eq!(parse("a((1,2))"), Err(ParseError::cmp_ref("only one expression expected"))); 
    assert_eq!(parse("(1]"), Err(ParseError::cmp_ref("wrong bracket: expected ')'")));
    assert_eq!(parse("(1"), Err(ParseError::cmp_ref("missing close bracket: ')'")));
    assert_eq!(parse("1)"), Err(ParseError::cmp_ref("unexpected closing bracket")));

    assert_eq!(parse("{1}"), Ok(Eval(Node{
        core: Block(Box::new(Imm(Item::new_atomic(1)))), source: None, args: vec![]})));
    assert_eq!(parse("1.{2}(3)"), Ok(Eval(Node{
        core: Block(Box::new(Imm(Item::new_atomic(2)))),
        source: Some(Box::new(Imm(Item::new_atomic(1)))),
        args: vec![Imm(Item::new_atomic(3))]})));
    assert_eq!(parse("1.{2.a(3)}(4)"), Ok(Eval(Node{
        core: Block(Box::new(Eval(Node{
            core: Simple("a".to_string()),
            source: Some(Box::new(Imm(Item::new_atomic(2)))),
            args: vec![Imm(Item::new_atomic(3))]
        }))),
        source: Some(Box::new(Imm(Item::new_atomic(1)))),
        args: vec![Imm(Item::new_atomic(4))]})));
    assert_eq!(parse("{1}.{2}"), Ok(Eval(Node{
        core: Block(Box::new(Imm(Item::new_atomic(2)))),
        source: Some(Box::new(Eval(Node{
            core: Block(Box::new(Imm(Item::new_atomic(1)))),
            source: None, args: vec![]
        }))),
        args: vec![]})));
}
