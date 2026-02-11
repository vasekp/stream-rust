use crate::base::*;

pub(crate) struct DescribeBuilder<'a> {
    head: &'a Head,
    inner_prec: u32,
    env: &'a Env,
    source: Option<String>,
    args: Vec<String>,
}

impl<'a> DescribeBuilder<'a> {
    pub(crate) fn new(head: &'a Head, env: &'a Env) -> Self {
        let inner_prec = match head {
            Head::Oper(op) => op_prec(op).unwrap_or(0),
            Head::Lang(LangItem::Args) => u32::MAX,
            _ => 0,
        };
        Self {
            head,
            env,
            inner_prec,
            source: None,
            args: Vec::new(),
        }
    }

    pub(crate) fn set_source(&mut self, src: &impl Describe) -> &mut Self {
        self.source = Some(src.describe_inner(u32::MAX, self.env));
        self
    }

    pub(crate) fn set_source_opt(&mut self, src: &Option<impl Describe>) -> &mut Self {
        self.source = src.as_ref().map(|s| s.describe_inner(u32::MAX, self.env));
        self
    }

    pub(crate) fn push_arg(&mut self, arg: &(impl Describe + ?Sized)) -> &mut Self {
        self.args.push(arg.describe_inner(self.inner_prec, self.env));
        self
    }

    pub(crate) fn push_args<'b>(&mut self, args: impl IntoIterator<Item = &'b (impl Describe + 'b)>) -> &mut Self {
        for arg in args {
            self.push_arg(arg);
        }
        self
    }

    pub(crate) fn finish(&mut self, outer_prec: u32) -> String {
        let mut ret = String::new();
        match self.head {
            Head::Symbol(_) | Head::Block(_) => {
                if let Some(src) = &self.source { ret += &format!("{src}."); }
                match self.head {
                    Head::Symbol(sym) => ret += sym,
                    Head::Block(blk) => ret += &format!("{{{}}}", blk.describe_inner(0, self.env)),
                    _ => unreachable!()
                };
                if !self.args.is_empty() {
                    ret.push('(');
                    ret += &self.join_args(", ");
                    ret.push(')');
                }
            },
            Head::Oper(op) => {
                debug_assert!(self.source.is_none());
                let parens = self.inner_prec <= outer_prec;
                if parens { ret.push('('); }
                if self.args.len() == 1 { ret += op; }
                ret += &self.join_args(op);
                if parens { ret.push(')'); }
            },
            Head::Lang(LangItem::List) => {
                debug_assert!(self.source.is_none());
                ret.push('[');
                ret += &self.join_args(", ");
                ret.push(']');
            },
            Head::Lang(LangItem::Part) => {
                ret += self.source.as_ref().expect("*part should have source by construction");
                ret.push('[');
                ret += &self.join_args(", ");
                ret.push(']');
            },
            Head::Lang(LangItem::Map) => {
                ret += self.source.as_ref().expect("*part should have source by construction");
                ret.push(':');
                debug_assert!(self.args.len() == 1);
                ret += &self.args[0];
            },
            Head::Lang(LangItem::Args) => {
                if let Some(src) = &self.source { ret += &format!("{src}."); }
                debug_assert!(self.args.len() == 2);
                ret += &format!("{}@({})", self.args[0], self.args[1]);
            },
        }
        ret
    }

    fn join_args(&self, sep: &str) -> String {
        let mut iter = self.args.iter();
        let Some(mut ret) = iter.next().cloned() else { return String::new(); };
        for s in iter {
            ret += sep;
            ret += s;
        }
        ret
    }
}
