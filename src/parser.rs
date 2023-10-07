use std::str::CharIndices;
use std::iter::Peekable;
use std::fmt::{Display, Formatter, Debug};
use std::cell::RefCell;
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
        //println!("\x1b[8m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)]);
        println!("{}\x1b[1;31m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)], &input[(start+length)..]);
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
    String,
    Char,
    Oper,
    Open,
    Close,
    Comma,
    Special
}

fn token_class(slice: &str) -> Result<TokenClass, ParseError> {
    use TokenClass::*;
    const OPERS: &str = ".:<=>+-*/%@^~&|";
    let class = match slice.chars().next().unwrap() {
        '0'..='9' => if slice.contains('_') {
                BaseNum
            } else {
                Number
            },
        'a'..='z' | 'A'..='Z' => Ident,
        '"' => String,
        '\'' => Char,
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
fn test_parser() {
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
    assert_eq!(tk.next(), Some(Ok(Token(Oper, ".")))); // should remain single character
    assert_eq!(tk.next(), Some(Ok(Token(Oper, ".")))); // should remain separate
    assert_eq!(tk.next(), Some(Ok(Token(Oper, ":")))); // ditto
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
    assert_eq!(it.next(), Some(Ok(Token(String, "\"a'b\""))));
    assert_eq!(it.next(), Some(Ok(Token(Ident, "c"))));
    assert_eq!(it.next(), Some(Ok(Token(Char, "'d\"é'")))); // non-ASCII in quotes
    assert_eq!(it.next(), Some(Err(ParseError::cmp_ref("invalid character")))); // non-ASCII
    assert_eq!(it.next(), None);
}


type Expr<'a> = Vec<ExprPart<'a>>;

#[derive(Debug)]
enum ExprPart<'a> {
    Op(Token<'a>),
    Term(TTerm<'a>),
    Part(Vec<Expr<'a>>)
}

#[derive(Debug)]
enum TTerm<'a> {
    Node(TNode<'a>, Option<Vec<Expr<'a>>>),
    ParExpr(Box<Expr<'a>>),
    Value(TValue<'a>),
    List(Vec<Expr<'a>>),
    Special(Token<'a>, Option<Token<'a>>)
}

#[derive(Debug)]
enum TNode<'a> {
    Ident(Token<'a>),
    Block(Box<Expr<'a>>)
}

#[derive(Debug)]
enum TValue<'a> {
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

fn parse_main<'a>(tk: &RefCell<Tokenizer<'a>>, bracket: Option<&'a str>) -> Result<Vec<Expr<'a>>, ParseError<'a>> {
    let mut exprs: Vec<Expr> = vec![];
    let mut expr: Expr = vec![];
    let mut last_comma = None;
    use ExprPart::*;
    use TTerm::*;
    use TokenClass::*;
    let slice_from = |start| tk.borrow_mut().slice_from(start);
    loop {
        let t = {
            let mut tk = tk.borrow_mut();
            match tk.next() {
                Some(t0) => t0?,
                None => {
                    if let Some(open) = bracket {
                        return Err(ParseError::new(format!("missing close bracket: '{}'", closing(open)), tk.slice_from(open)));
                    }
                    break;
                }
            }
        };
        let last = expr.last_mut();
        match t.0 {
            Number | BaseNum | String | Char | Ident => match (last, &t.0) {
                (Some(Op(_)) | None, _) => {
                    expr.push(Term(match t {
                        Token(Number, _) => Value(TValue::Number(t)),
                        Token(BaseNum, _) => Value(TValue::BaseNum(t)),
                        Token(String, _) => Value(TValue::String(t)),
                        Token(Char, _) => Value(TValue::Char(t)),
                        Token(Ident, "true" | "false") => Value(TValue::Bool(t)),
                        Token(Ident, _) => Node(TNode::Ident(t), None),
                        _ => unreachable!()
                    }));
                },
                // Special case: # -> #1, $ -> $1
                (Some(Term(TTerm::Special(_, arg @ None))), &Number) => {
                    *arg = Some(t);
                    continue;
                },
                _ => return Err(ParseError::new("cannot appear here", t.1))
            },
            Oper => match (last, t.1) {
                (Some(Term(_) | Part(_)), _) => expr.push(Op(t)),
                // Special case: unary minus
                (None, "-" | "+") => expr.push(Op(t)),
                _ => return Err(ParseError::new("cannot appear here", t.1))
            },
            Open => match (t.1, last) {
                // Parentheses
                ("(", Some(Op(_)) | None) => {
                    let vec = parse_main(tk, Some(t.1))?;
                    match vec.len() {
                        1 => {
                            let e = vec.into_iter().next().unwrap();
                            expr.push(Term(ParExpr(Box::new(e))));
                        },
                        0 => return Err(ParseError::new("empty expression", slice_from(t.1))),
                        _ => return Err(ParseError::new("only one expression expected", slice_from(t.1)))
                    }
                },
                // Arguments
                ("(", Some(Term(Node(_, args @ None)))) => {
                    let vec = parse_main(tk, Some(t.1))?;
                    *args = Some(vec);
                },
                // List
                ("[", Some(Op(_)) | None) => {
                    let vec = parse_main(tk, Some(t.1))?;
                    expr.push(Term(List(vec)));
                },
                // Parts
                ("[", Some(Term(_))) => {
                    let vec = parse_main(tk, Some(t.1))?;
                    if vec.is_empty() {
                        return Err(ParseError::new("empty parts", slice_from(t.1)));
                    }
                    expr.push(Part(vec));
                },
                // Block
                ("{", Some(Op(_)) | None) => {
                    let vec = parse_main(tk, Some(t.1))?;
                    match vec.len() {
                        1 => {
                            let e = vec.into_iter().next().unwrap();
                            expr.push(Term(Node(TNode::Block(Box::new(e)), None)));
                        },
                        0 => return Err(ParseError::new("empty block", slice_from(t.1))),
                        _ => return Err(ParseError::new("only one expression expected", slice_from(t.1)))
                    }
                },
                _ => return Err(ParseError::new("cannot appear here", t.1))
            },
            Close => {
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
            Comma => {
                if let Some(Op(_)) | None = last {
                    return Err(ParseError::new("incomplete expression", slice_from(t.1)));
                }
                let mut new = vec![];
                std::mem::swap(&mut expr, &mut new);
                exprs.push(new);
                last_comma = Some(t);
            },
            TokenClass::Special => match last {
                Some(Op(_)) | None => expr.push(Term(TTerm::Special(t, None))),
                _ => return Err(ParseError::new("cannot appear here", t.1))
            }
        }
    }
    match expr.last() {
        Some(Term(_) | Part(_)) => exprs.push(expr),
        Some(Op(t)) => return Err(ParseError::new("incomplete expression", slice_from(t.1))),
        None => if let Some(t) = last_comma {
            return Err(ParseError::new("incomplete expression", slice_from(t.1)));
        }
    }
    Ok(exprs)
}


pub fn parse(input: &str) {
    match parse_main(&RefCell::new(Tokenizer::new(input)), None) {
        Ok(vec) => match vec.len() {
            1 => println!("{vec:?}"),
            0 => println!("empty input"),
            _ => println!("multiple expressions")
        },
        Err(err) => {
            err.display(input);
            println!("{}", err);
        }
    }
}
