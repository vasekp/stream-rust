mod error;
mod tk;
mod utils;

use crate::base::*;
use crate::interner::intern;

pub use error::ParseError;
use tk::{Tokenizer, Token, TokenClass};
use utils::*;

struct Parser<'str> {
    tk: Tokenizer<'str>
}

impl<'str> Parser<'str> {
    fn read_link(&mut self) -> Result<Option<Link>, ParseError<'str>> {
        let Some(tok) = self.tk.next_tr()? else { return Ok(None); };
        use TokenClass as TC;
        let head = match tok {
            Token(TC::Ident, name) => Head::Symbol(intern(&name.to_lowercase())),
            Token(TC::Special, tk @ "$") => {
                match self.tk.next_tr()? {
                    Some(Token(TC::Ident | TC::Number, name)) =>
                        Head::Symbol(intern(&format!("{tk}{}", &name.to_lowercase()))),
                    Some(Token(TC::Open, bkt @ "{")) =>
                        return Ok(Some(self.read_block_link(bkt, true)?)),
                    Some(Token(_, tok)) =>
                        return Err(ParseError::new("cannot appear here", tok)),
                    None =>
                        return Err(ParseError::new("cannot appear alone", tok.1)),
                }
            },
            Token(TC::Open, bkt @ "{") => return Ok(Some(self.read_block_link(bkt, false)?)),
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
                    .ok_or(ParseError::new("incomplete expression", tok))?;
                Link::new(LangItem::Args, vec![Expr::new_node(head, None, vec![]), arg])
            },
            Some(&Token(TC::Open, bkt @ "{")) => {
                self.tk.next();
                let arg = self.read_block_link(bkt, false)?;
                Link::new(head, vec![arg.into()])
            },
            _ => Link::new(head, vec![])
        }))
    }

    fn read_block_link(&mut self, open: &'str str, reset_env: bool) -> Result<Link, ParseError<'str>> {
        use TokenClass as TC;
        let head = Head::Block{body: self.read_arg(open)?, reset_env};
        Ok(match self.tk.peek()? {
            Some(&Token(TC::Open, bkt @ "(")) => {
                self.tk.next();
                Link::new(head, self.read_args(bkt)?)
            },
            Some(&Token(TC::Chain, tok @ "@")) => {
                self.tk.next();
                let arg = self.read_expr_part()?
                    .ok_or(ParseError::new("incomplete expression", tok))?;
                Link::new(LangItem::Args, vec![Expr::new_node(head, None, vec![]), arg])
            },
            _ => Link::new(head, vec![])
        })
    }

    fn read_expr_part(&mut self) -> Result<Option<Expr>, ParseError<'str>> {
        let Some(tok) = self.tk.next_tr()? else { return Ok(None); };
        use TokenClass as TC;
        Some(match tok {
            Token(TC::Number, value) => Ok(Expr::new_number(value.parse::<Number>()
                .map_err(|_| ParseError::new("invalid number", value))?)),
            Token(TC::BaseNum, value) => Ok(Expr::new_number(parse_basenum(value)?)),
            Token(TC::CBaseNum, value) => Ok(Expr::new_number(parse_c_basenum(value)?)),
            Token(TC::Bool(value), _) => Ok(Expr::new_bool(value)),
            Token(TC::Char, value) => Ok(Expr::new_char(parse_char(value)?)),
            Token(TC::String, value) => Ok(Expr::new_string(&parse_string(value)?)),
            Token(TC::Open, bkt @ "[") => Ok(Expr::new_node(LangItem::List, None, self.read_args(bkt)?)),
            Token(TC::Ident, _) => {
                self.tk.unread(tok);
                Ok(self.read_link()?.unwrap().into()) // cannot be None after unread()
            },
            Token(TC::Open, bkt @ "{") => {
                Ok(self.read_block_link(bkt, false)?.into())
            },
            Token(TC::Open, bkt @ "(") => Ok(self.read_arg(bkt)?),
            Token(TC::Special, "$") => {
                match self.tk.next_tr()? {
                    Some(next @ Token(TC::Ident | TC::Number, _)) => {
                        self.tk.unread(self.tk.merge(tok, next, TC::Ident));
                        Ok(self.read_link()?.unwrap().into()) // cannot be None after unread()
                    },
                    Some(Token(TC::Special, "#")) => Ok(Expr::Repl(Subst::Counter)),
                    Some(Token(TC::Open, bkt @ "{")) => Ok(self.read_block_link(bkt, true)?.into()),
                    Some(Token(_, tok)) => Err(ParseError::new("cannot appear here", tok)),
                    None => Err(ParseError::new("cannot appear alone", tok.1)),
                }
            },
            Token(TC::Special, chr) => {
                let subst = match chr {
                    "#" => Subst::Input,
                    "%" => Subst::History,
                    _ => panic!("unhandled special character '{chr}'")
                };
                match self.tk.peek()? {
                    Some(&Token(TC::Number, value)) => {
                        self.tk.next();
                        match value.parse::<usize>() {
                            Ok(ix @ 1..) => Ok(Expr::Repl(subst(Some(ix)))),
                            Ok(0) => Err(ParseError::new("index can't be zero", self.tk.slice_from(chr))),
                            Err(_) => Err(ParseError::new("index too large", self.tk.slice_from(chr))),
                        }
                    },
                    Some(&Token(TC::Special, "#")) if chr == "#" => {
                        self.tk.next();
                        Ok(Expr::Repl(Subst::InputList))
                    },
                    _ => Ok(Expr::Repl(subst(None)))
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
            multi: bool,
            args: Vec<Expr>
        }
        let mut stack: Vec<StackEntry> = vec![];

        let mut cur = match self.tk.next_tr()? {
            None => return Ok(None),
            Some(tok @ Token(TC::Close | TC::Comma, _)) => {
                self.tk.unread(tok);
                return Ok(None)
            },
            Some(Token(TC::Oper, op @ ("+" | "-" | "!"))) => {
                let (prec, multi) = op_rules(op).unwrap();
                stack.push(StackEntry{op, prec, multi, args: vec![]});
                self.read_expr_part()?
                    .ok_or(ParseError::new("incomplete expression", op))?
            },
            Some(tok) => {
                self.tk.unread(tok);
                self.read_expr_part()?
                    .expect("read_expr_part() should return Ok(Some) or Err in all remaining cases")
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
                        .ok_or(ParseError::new("incomplete expression", tok))?;
                    src.chain(node)
                },
                (src, Token(TC::Chain, tok @ ":")) => {
                    let node = self.read_link()?
                        .ok_or(ParseError::new("incomplete expression", tok))?;
                    src.chain(Link::new(LangItem::Map, vec![node.into()]))
                },
                (src, Token(TC::Open, bkt @ "[")) => {
                    let args = self.read_args(bkt)?;
                    if args.is_empty() {
                        return Err(ParseError::new("empty parts", self.tk.slice_from(bkt)));
                    }
                    src.chain(Link::new(LangItem::Part, args))
                },
                (mut expr, Token(TC::Oper, op) | Token(TC::Special, op @ "%")) => {
                    let (prec, multi) = op_rules(op)
                        .map_err(|err| ParseError::new(err, op))?;
                    loop {
                        let Some(mut entry) = stack.pop() else {
                            // No stack: start new
                            stack.push(StackEntry{op, prec, multi, args: vec![expr]});
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
                        } else if entry.prec == prec && !(entry.multi && multi) {
                            // Same precedence but no bunching: finish old operation and
                            // replace stack top by the new one
                            entry.args.push(expr);
                            expr = Expr::new_op(entry.op, entry.args);
                            stack.push(StackEntry{op, prec, multi, args: vec![expr]});
                            break;
                        } else if entry.prec == prec {
                            return Err(ParseError::new(format!("cannot mix with '{}', please rewrite separately", entry.op), op));
                        } else {
                            // Lower precedence: keep the previous stack and add new op on top
                            stack.push(entry);
                            stack.push(StackEntry{op, prec, multi, args: vec![expr]});
                            break;
                        }
                    }
                    if let Some(Token(TC::Oper, pfx_op)) = self.tk.peek()?
                        && matches!(*pfx_op, "+" | "-" | "!")
                        && let Ok((pfx_prec, pfx_multi)) = op_rules(pfx_op)
                        && pfx_prec > prec {
                            stack.push(StackEntry{op: pfx_op, prec: pfx_prec, multi: pfx_multi, args: vec![]});
                            self.tk.next();
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
            .ok_or(ParseError::new("missing closing bracket", open))?;
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
                .ok_or(ParseError::new("missing closing bracket", open))?;
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
/// use streamlang as stream;
/// use stream::base::*;
///
/// let s1 = stream::parse("range(3):{#^2}").unwrap();
/// assert_eq!(s1.describe(), "range(3):{#^2}");
///
/// // Manual creation
/// let block = Expr::new_op("^", vec![Expr::Repl(Subst::Input(None)), Expr::new_number(2)]);
/// let s2 = Expr::new_node("range", None, vec![Expr::new_number(3)])
///     .chain(Link::new(LangItem::Map, vec![Expr::new_node(block, None, vec![])]));
/// assert_eq!(s2.describe(), "range(3):{#^2}");
/// ```
pub fn parse(input: &str) -> Result<Expr, ParseError<'_>> {
    Parser::parse(input)
}


#[test]
fn test_parser() {
    assert_eq!(parse("1"), Ok(Expr::new_number(1)));
    assert_eq!(parse("a"), Ok(Expr::new_node("a", None, vec![])));
    assert_eq!(parse("A"), Ok(Expr::new_node("a", None, vec![])));
    assert_eq!(parse("a(1,2)"), Ok(Expr::new_node("a", None, vec![Expr::new_number(1), Expr::new_number(2)])));
    assert_eq!(parse("1.a"), Ok(Expr::new_number(1).chain(Link::new("a", vec![]))));
    assert_eq!(parse("(1).a"), Ok(Expr::new_number(1).chain(Link::new("a", vec![]))));
    assert!(parse("(1,2).a").is_err());
    assert_eq!(parse("a.b"), Ok(Expr::new_node("a", None, vec![]).chain(Link::new("b", vec![]))));
    assert_eq!(parse("a.b.c"), Ok(Expr::new_node("a", None, vec![]).chain(Link::new("b", vec![]))
        .chain(Link::new("c", vec![]))));
    assert_eq!(parse("a(1).b(2)"), Ok(Expr::new_node("a", None, vec![Expr::new_number(1)])
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

    assert_eq!(parse("555"), Ok(Expr::new_number(555)));
    assert_eq!(parse("10_555"), Ok(Expr::new_number(555)));
    assert_eq!(parse("8_555"), Ok(Expr::new_number(365)));
    assert_eq!(parse("0o555"), Ok(Expr::new_number(365)));
    assert_eq!(parse("0555"), Ok(Expr::new_number(555)));
    assert!(parse("0z555").is_err());

    assert_eq!(parse("a.b..c.d"), Ok(Expr::new_op("..", vec![
        Expr::new_node("a", None, vec![]).chain(Link::new("b", vec![])),
        Expr::new_node("c", None, vec![]).chain(Link::new("d", vec![]))])));
    assert_eq!(parse("a..b..c"), Ok(Expr::new_op("..", vec![
        Expr::new_op("..", vec![
            Expr::new_node("a", None, vec![]),
            Expr::new_node("b", None, vec![])]),
            Expr::new_node("c", None, vec![])])));

    assert_eq!(parse("{1}"), Ok(Expr::new_node(Expr::new_number(1), None, vec![])));
    assert!(parse("{}").is_err());
    assert_eq!(parse("1.{2}(3)"), Ok(Expr::new_number(1)
        .chain(Link::new(Expr::new_number(2), vec![Expr::new_number(3)]))));
    assert_eq!(parse("1.{2.a(3)}(4)"), Ok(Expr::new_number(1).chain(Link::new(
        Expr::new_number(2).chain(Link::new("a", vec![Expr::new_number(3)])),
        vec![Expr::new_number(4)]))));
    assert_eq!(parse("{1}.{2}"), Ok(Expr::new_node(Expr::new_number(1), None, vec![])
        .chain(Link::new(Expr::new_number(2), vec![]))));

    assert_eq!(parse("[1,2][3,4]"), Ok(Expr::new_node(LangItem::List, None,
            vec![Expr::new_number(1), Expr::new_number(2)])
        .chain(Link::new(LangItem::Part,
            vec![Expr::new_number(3), Expr::new_number(4)]))));
    assert_eq!(parse("[1][2][3]"), Ok(Expr::new_node(LangItem::List, None, vec![Expr::new_number(1)])
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(2)]))
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(3)]))));
    assert_eq!(parse("[][3,4]"), Ok(Expr::new_node(LangItem::List, None, vec![])
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(3), Expr::new_number(4)]))));
    assert!(parse("[1,2][]").is_err());
    assert!(parse("a.[1]").is_err());
    // The following is legal syntax, but error at runtime
    assert_eq!(parse("1[2]"), Ok(Expr::new_number(1)
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(2)]))));
    assert_eq!(parse("a[b]"), Ok(Expr::new_node("a", None, vec![])
        .chain(Link::new(LangItem::Part, vec![Expr::new_node("b", None, vec![])]))));
    assert_eq!(parse("[[1]][[2]]"), Ok(Expr::new_node(LangItem::List, None,
            vec![Expr::new_node(LangItem::List, None, vec![Expr::new_number(1)])])
        .chain(Link::new(LangItem::Part,
            vec![Expr::new_node(LangItem::List, None, vec![Expr::new_number(2)])]))));
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

    assert_eq!(parse("#"), Ok(Expr::Repl(Subst::Input(None))));
    assert_eq!(parse("#1"), Ok(Expr::Repl(Subst::Input(Some(1)))));
    assert!(parse("#0").is_err());
    assert!(parse("#18446744073709551616").is_err());
    assert_eq!(parse("##"), Ok(Expr::Repl(Subst::InputList)));
    assert!(parse("###").is_err());
    assert!(parse("#a").is_err());
    assert!(parse("#%").is_err());
    assert_eq!(parse("$#"), Ok(Expr::Repl(Subst::Counter)));
    assert_eq!(parse("$name"), Ok(Expr::new_node("$name", None, vec![])));
    assert_eq!(parse("$NaMe"), Ok(Expr::new_node("$name", None, vec![])));
    assert_eq!(parse("a.$b@$c(1)"), Ok(Expr::new_node("a", None, vec![])
            .chain(Link::new(LangItem::Args, vec![
                    Expr::new_node("$b", None, vec![]),
                    Expr::new_node("$c", None, vec![Expr::new_number(1)])]))));
    assert!(parse("$").is_err());
    assert!(parse("1.$").is_err());
    assert_eq!(parse("$1"), Ok(Expr::new_node("$1", None, vec![])));
    assert_eq!(parse("$1x"), Ok(Expr::new_node("$1x", None, vec![])));
    assert!(parse("$$").is_err());
    assert!(parse("$a$").is_err());
    assert!(parse("#(1)").is_err());
    assert_eq!(parse("#+%"), Ok(Expr::new_op("+", vec![
        Expr::Repl(Subst::Input(None)),
        Expr::Repl(Subst::History(None))])));
    assert!(parse("1.#").is_err());
    assert_eq!(parse("1.{#}(2)"), Ok(Expr::new_number(1)
        .chain(Link::new(Expr::Repl(Subst::Input(None)), vec![Expr::new_number(2)]))));

    assert_eq!(parse("1.${2}(3)"), Ok(Expr::new_number(1)
        .chain(Link::new(Head::Block{body: Expr::new_number(2), reset_env: true}, vec![
            Expr::new_number(3)]))));
    assert!(parse("${}").is_err());

    assert_eq!(parse("a.b@c@d[1]"), Ok(Expr::new_node("a", None, vec![])
        .chain(Link::new(LangItem::Args, vec![
            Expr::new_node("b", None, vec![]),
            Expr::new_node(LangItem::Args, None, vec![
                Expr::new_node("c", None, vec![]),
                Expr::new_node("d", None, vec![])])]))
        .chain(Link::new(LangItem::Part, vec![Expr::new_number(1)]))));
    // literals, list, #, (x) may follow @
    assert_eq!(parse("a@1"), Ok(Expr::new_node(LangItem::Args, None,
        vec![Expr::new_node("a", None, vec![]), Expr::new_number(1)])));
    assert_eq!(parse("a@[1,2]"), Ok(Expr::new_node(LangItem::Args, None,
        vec![Expr::new_node("a", None, vec![]), Expr::new_node(LangItem::List, None,
            vec![Expr::new_number(1), Expr::new_number(2)])])));
    assert_eq!(parse("a@(b.c@#)"), Ok(Expr::new_node(LangItem::Args, None,
        vec![Expr::new_node("a", None, vec![]), Expr::new_node("b", None, vec![])
            .chain(Link::new(LangItem::Args, vec![
                    Expr::new_node("c", None, vec![]),
                    Expr::Repl(Subst::Input(None))]))])));
    // but not (x,y)
    assert!(parse("a@(b.c@#,d)").is_err());
    // blocks allowed both before and after
    assert_eq!(parse("{a}@{b}"), Ok(Expr::new_node(LangItem::Args, None, vec![
        Expr::new_node(Expr::new_node("a", None, vec![]), None, vec![]),
        Expr::new_node(Expr::new_node("b", None, vec![]), None, vec![])])));
    // (c) binds to b but [d] to entire expression
    assert_eq!(parse("a@b(c)[d]"), Ok(Expr::new_node(LangItem::Args, None, vec![
            Expr::new_node("a", None, vec![]),
            Expr::new_node("b", None, vec![Expr::new_node("c", None, vec![])])])
        .chain(Link::new(LangItem::Part, vec![Expr::new_node("d", None, vec![])]))));
    // no @ after arguments
    assert!(parse("a(b)@c").is_err());
    assert!(parse("a@@c").is_err());
    assert!(parse("1@a").is_err());
    assert!(parse("a@").is_err());
    assert_eq!(parse("a.b{c}(d)"), parse("a.b({c}(d))"));
    assert!(parse("a.b{c}(d)(e)").is_err());
    assert_eq!(parse("a.b{c}@d"), parse("a.b({c}@d)"));
    assert!(parse("{a}{b}").is_err());

    assert_eq!(parse("1..2"), Ok(Expr::new_op("..", vec![Expr::new_number(1), Expr::new_number(2)])));
    assert_eq!(parse("1%2"), Ok(Expr::new_op("%", vec![Expr::new_number(1), Expr::new_number(2)])));
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
        Expr::new_node("a", None, vec![]).chain(Link::new("b", vec![])),
        Expr::new_number(2)])));
    // +(1, 2)
    assert_eq!(parse("+1+2"), Ok(Expr::new_op("+", vec![Expr::new_number(1), Expr::new_number(2)])));
    // -( -(1), 2)
    assert_eq!(parse("-1-2"), Ok(Expr::new_op("-", vec![
        Expr::new_op("-", vec![Expr::new_number(1)]),
        Expr::new_number(2)])));
    // -(1..1) (error)
    assert_eq!(parse("-1..2^3"), Ok(Expr::new_op("..", vec![
            Expr::new_op("-", vec![Expr::new_number(1)]),
            Expr::new_op("^", vec![Expr::new_number(2), Expr::new_number(3)])])));
    assert!(parse("--1").is_err());
    assert!(parse("*1*2").is_err());
    // relations and parentheses
    assert_eq!(parse("(1==2)==(3==4==5)"), Ok(Expr::new_op("==", vec![
        Expr::new_op("==", vec![Expr::new_number(1), Expr::new_number(2)]),
        Expr::new_op("==", vec![Expr::new_number(3), Expr::new_number(4), Expr::new_number(5)])])));
    assert_eq!(parse("1<=2<=3"), Ok(Expr::new_op("<=", vec![
        Expr::new_number(1), Expr::new_number(2), Expr::new_number(3)])));
    // This parses but gives a runtime error
    assert_eq!(parse("1<>2<>3"), Ok(Expr::new_op("<>", vec![
        Expr::new_number(1), Expr::new_number(2), Expr::new_number(3)])));
    // Different operations can't be chained
    assert!(parse("1<2<=3").is_err());
    assert!(parse("1===3").is_err());

    assert_eq!(parse("a=-1"), Ok(Expr::new_op("=", vec![
        Expr::new_node("a", None, vec![]),
        Expr::new_op("-", vec![Expr::new_number(1)])])));
    assert_eq!(parse("-1^2"), Ok(Expr::new_op("-", vec![
        Expr::new_op("^", vec![Expr::new_number(1), Expr::new_number(2)])])));
    assert_eq!(parse("-2..-1"), Ok(Expr::new_op("..", vec![
        Expr::new_op("-", vec![Expr::new_number(2)]),
        Expr::new_op("-", vec![Expr::new_number(1)])])));
    assert_eq!(parse("+1>-1"), Ok(Expr::new_op(">", vec![
        Expr::new_op("+", vec![Expr::new_number(1)]),
        Expr::new_op("-", vec![Expr::new_number(1)])])));
    assert_eq!(parse("true==!false"), Ok(Expr::new_op("==", vec![
        Expr::new_bool(true), Expr::new_op("!", vec![Expr::new_bool(false)])])));
}
