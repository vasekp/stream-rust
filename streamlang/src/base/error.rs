use crate::base::*;
use std::fmt::{Display, Formatter, Debug};

pub type SResult<T> = std::result::Result<T, StreamError>;

/// The runtime error type.
#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct StreamError {
    reason: Reason,
    trace: Vec<Expr>,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Reason {
    Generic(String),
    Usage(&'static str),
    Interrupt,
}

impl StreamError {
    pub fn with_expr(reason: impl Into<Reason>, expr: &impl ToExpr) -> Self {
        Self{reason: reason.into(), trace: vec![expr.to_expr()]}
    }

    pub fn usage(head: &Head) -> Self {
        let head_str = head.as_str().expect("StreamError::usage should be called with Head::Symbol or Head::Oper");
        Self{reason: Reason::Usage(head_str), trace: vec![]}
    }

    pub fn interrupt() -> Self {
        Self{reason: Reason::Interrupt, trace: vec![]}
    }

    pub(crate) fn wrap(mut self, expr: &impl ToExpr) -> Self {
        self.trace.push(expr.to_expr());
        self
    }

    pub fn backtrace(&self) -> &[Expr] {
        &self.trace[..]
    }

    pub fn reason(&self) -> impl Display {
        &self.reason
    }
}

impl std::error::Error for StreamError { }

impl<T: Into<Reason>> From<T> for StreamError {
    fn from(reason: T) -> Self {
        Self{reason: reason.into(), trace: vec![]}
    }
}

impl Display for StreamError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(expr) = &self.trace.first() {
            write!(f, "{}: ", expr.describe())?;
        }
        write!(f, "{}", self.reason)
    }
}

impl<T: Into<String>> From<T> for Reason {
    fn from(s: T) -> Self {
        Reason::Generic(s.into())
    }
}

impl Display for Reason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generic(s) => write!(f, "{s}"),
            Self::Usage(sym) => write!(f, "invalid call pattern: see ?{sym}"),
            Self::Interrupt => write!(f, "interrupted")
        }
    }
}

pub trait ToExpr {
    fn to_expr(&self) -> Expr;
}

impl ToExpr for Expr {
    fn to_expr(&self) -> Expr {
        self.clone()
    }
}

impl ToExpr for Node {
    fn to_expr(&self) -> Expr {
        Expr::Eval(Rc::new(self.clone()))
    }
}

impl ToExpr for Node<Item> {
    fn to_expr(&self) -> Expr {
        Expr::Eval(Rc::new(self.clone().into()))
    }
}

impl ToExpr for Rc<Node> {
    fn to_expr(&self) -> Expr {
        Expr::Eval(Rc::clone(self))
    }
}

impl ToExpr for Item {
    fn to_expr(&self) -> Expr {
        Expr::Imm(self.clone())
    }
}

impl<I: ItemType> ToExpr for Rc<dyn Stream<I>> {
    fn to_expr(&self) -> Expr {
        Expr::Imm(Item::from(self))
    }
}

impl ToExpr for Char {
    fn to_expr(&self) -> Expr {
        Expr::Imm(Item::from(*self))
    }
}


macro_rules! check_stop {
    () => {
        if stop::should_stop() {
            stop::reset_stop();
            Err(StreamError::interrupt())?;
        }
    };
}

pub(crate) use check_stop;
