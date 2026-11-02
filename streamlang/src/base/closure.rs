use crate::base::*;

/// A body closed over the [`Env`] in effect where it was defined: a function variable of `with`, a
/// session global `$f = {...}`, a hand-written `${...}` block, or a value spliced into an
/// expression by one of those mechanisms (see [`splice_value()`]).
///
/// The body is applied by substituting `#`, `#1`, ... (see [`Expr::apply()`]) and evaluated in the
/// captured environment, never in the caller's. The substitution pass of `with` does not enter a
/// closure, which is what makes the scoping of function variables lexical.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Closure {
    body: Expr,
    env: Env,
}

impl Closure {
    pub fn new(body: Expr, env: Env) -> Closure {
        Closure { body, env }
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    /// Substitutes `source` and `args` for `#`, `#1`, ... in the body and evaluates it in the
    /// captured environment.
    pub fn call(&self, source: &Option<Item>, args: &Vec<Item>) -> SResult<Item> {
        self.body.apply(source, args)?.eval(&self.env)
    }
}

impl Describe for Closure {
    /// Describes as `${body}`, or `${alpha(X, body)}` if the captured alphabet is not the default.
    /// This is independent of `env`: a closure means the same thing wherever it appears, and so
    /// does its description.
    fn describe_inner(&self, _prec: u32, _env: &Env) -> String {
        let base = Env::default();
        format!("${{{}}}", Env::wrap_describe(&base, &self.env, |_| self.body.describe_inner(0, &self.env), 0))
    }
}

/// Turns an evaluated value into the expression which stands for it when it is substituted into an
/// unevaluated expression by `with`, or by a session for `%n` and `$name`.
///
/// A value whose description cannot contain a symbol (a number, boolean, character, literal string,
/// or a list of such) is substituted as is. Any other value is wrapped in a [`Closure`], so that its
/// description reads `${...}` and a `with` enclosing it in the described expression cannot
/// substitute into it on re-parsing.
pub(crate) fn splice_value(item: Item) -> Expr {
    if is_symbol_free(&item) {
        Expr::Imm(item)
    } else {
        Expr::Eval(Rc::new(Node { head: Head::Closure(Closure::new(Expr::Imm(item), Env::default())), source: None, args: vec![] }))
    }
}

fn is_symbol_free(item: &Item) -> bool {
    match item {
        Item::Number(_) | Item::Bool(_) | Item::Char(_) => true,
        Item::String(stm) => stm.as_list().is_some(),
        Item::Stream(stm) => stm.as_list().is_some_and(|items| items.iter().all(is_symbol_free)),
    }
}
