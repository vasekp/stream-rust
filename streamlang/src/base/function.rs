use crate::base::*;

use std::borrow::Cow;

/// A function node (what [`Expr::as_func()`] returns: a symbol such as `ucase`, a call `f(2)`, a
/// block `{#+1}` or a closure `$g`) together with the [`Env`] in effect where it was written.
///
/// This is what an op which evaluates a user-supplied function lazily keeps, instead of the node
/// and an environment separately. The function is applied by [`call()`](Self::call), which
/// completes the node with a source and arguments and evaluates it in the captured environment,
/// so that nothing has to be supplied at the time of the call (iteration has no environment of
/// its own, and passing one would invite passing the wrong one).
///
/// Unlike a [`Closure`], which holds a *body* with placeholders, this holds a *node* to be
/// completed, because what follows `seq:` may be a symbol or a call as well as a block. An op and a
/// function written in place always share one environment, so the only way a `Function` holds an
/// environment other than its op's is through a closure head, which carries its own; that is why
/// `DescribeBuilder::new_with_func()` adds the
/// `alpha(...)` wrapper for the op only when the function is not a closure.
#[derive(Clone)]
pub struct Function {
    node: Rc<Node>,
    env: Env,
}

impl Function {
    pub fn new(node: &Rc<Node>, env: &Env) -> Function {
        Function { node: Rc::clone(node), env: env.clone() }
    }

    pub fn env(&self) -> &Env {
        &self.env
    }

    pub fn head(&self) -> &Head {
        &self.node.head
    }

    /// Completes the node with `source` and `args`, if given, and evaluates it in the captured
    /// environment. Fails if the node already has a source or arguments, respectively.
    pub fn call(&self, source: Option<Item>, args: Vec<Item>) -> SResult<Item> {
        let node = match source {
            Some(source) => Cow::Owned(self.node.with_source(source.into())?),
            None => Cow::Borrowed(&*self.node),
        };
        let node = if args.is_empty() { node } else { Cow::Owned(node.with_args(args.into_iter().map(Expr::from).collect())?) };
        node.eval(&self.env)
    }

    pub fn call_source(&self, source: Item) -> SResult<Item> {
        self.call(Some(source), vec![])
    }

    pub fn call_args(&self, args: Vec<Item>) -> SResult<Item> {
        self.call(None, args)
    }

    /// Whether the function is a [`Closure`], which carries its own environment and describes
    /// itself independently of the context.
    pub(crate) fn is_closure(&self) -> bool {
        matches!(self.node.head, Head::Closure(_))
    }
}

impl Describe for Function {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        self.node.describe_inner(prec, env)
    }
}
