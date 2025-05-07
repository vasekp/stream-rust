use crate::base::*;
use std::collections::VecDeque;

#[derive(Clone)]
struct NestSource {
    source: Item,
    body: Node,
    head: Head,
    env: Rc<Env>
}

struct NestIterSource<'node> {
    body: &'node Node,
    prev: Item,
    env: &'node Rc<Env>
}

#[derive(Clone)]
struct NestArgs {
    body: Node,
    args: VecDeque<Item>,
    head: Head,
    env: Rc<Env>
}

struct NestIterArgs<'node> {
    body: &'node Node,
    prev: VecDeque<Item>,
    env: &'node Rc<Env>
}

fn eval_nest(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    match node.resolve() {
        RNode::Source(RNodeS { head, source, args: RArgs::One(Expr::Eval(body)) }) if body.source.is_none() && body.args.is_empty() => {
            Ok(Item::new_stream(NestSource{head, source: source.eval(env)?, body, env: Rc::clone(env)}))
        },
        RNode::NoSource(RNodeNS { head, args: RArgs::One(Expr::Eval(mut body)) }) if body.source.is_none() && !body.args.is_empty() => {
            let args = std::mem::take(&mut body.args)
                .into_iter()
                .map(|arg| arg.eval(env))
                .collect::<Result<VecDeque<_>, _>>()?;
            Ok(Item::new_stream(NestArgs{head, body, args, env: Rc::clone(env)}))
        },
        node => Err(StreamError::new("expected: source.nest({body}) or nest({body}(args))", node))
    }
}

impl Describe for NestSource {
    fn describe_prec(&self, prec: u32) -> String {
        self.env.wrap_describe(|prec|
            Node::describe_helper(&self.head, Some(&self.source), [&self.body], prec),
            prec)
    }
}

impl Describe for NestArgs {
    fn describe_prec(&self, prec: u32) -> String {
        self.env.wrap_describe(|prec|
            Node::describe_helper(&self.head, None::<&Item>, [&self.body], prec),
            prec)
    }
}

impl Stream for NestSource {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(NestIterSource{body: &self.body, prev: self.source.clone(), env: &self.env})
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Stream for NestArgs {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(NestIterArgs{body: &self.body, prev: self.args.clone(), env: &self.env})
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Iterator for NestIterSource<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.body.clone()
            .with_source(Expr::Imm(std::mem::take(&mut self.prev)))
            .and_then(|expr| expr.eval(self.env)) {
                Ok(item) => {
                    self.prev = item.clone();
                    Some(Ok(item))
                },
                Err(err) => Some(Err(err))
            }
    }
}

impl SIterator for NestIterSource<'_> {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }
}

impl Iterator for NestIterArgs<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let args = self.prev.iter()
            .map(|item| Expr::Imm(item.to_owned()))
            .collect();
        match self.body.clone()
            .with_args(args)
            .and_then(|expr| expr.eval(self.env)) {
                Ok(item) => {
                    self.prev.pop_front();
                    self.prev.push_back(item.clone());
                    Some(Ok(item))
                },
                Err(err) => Some(Err(err))
            }
    }
}

impl SIterator for NestIterArgs<'_> {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }
}

#[cfg(test)]
mod tests {
    //use super::*;

    #[test]
    fn test_map() {
        use crate::parser::parse;
        assert_eq!(parse("1.nest{#+1}").unwrap().eval_default().unwrap().to_string(), "[2, 3, 4, 5, 6, ...]");
        assert_eq!(parse("1.nest({#})").unwrap().eval_default().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
        assert_eq!(parse("'T'.nest{#+2}").unwrap().eval_default().unwrap().to_string(), "['V', 'X', 'Z', 'B', 'D', ...]");
        assert_eq!(parse("1.nest{#1}").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert!(parse("1.nest(2.{#})").unwrap().eval_default().is_err());
        assert!(parse("1.nest{#}(1)").unwrap().eval_default().is_err());
        assert_eq!(parse("nest{#1+#2}(1,1)").unwrap().eval_default().unwrap().to_string(), "[2, 3, 5, 8, 13, ...]");
        assert_eq!(parse("nest{#1+#2}(1)").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert_eq!(parse("nest{#}(1,1)").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert!(parse("nest({#}())").unwrap().eval_default().is_err());

        assert_eq!(parse("1.nest{#*2}[64]").unwrap().eval_default().unwrap().to_string(), "18446744073709551616");
        assert_eq!(parse("[].nest{[#]}[3]").unwrap().eval_default().unwrap().to_string(), "[[[[]]]]");
        assert_eq!(parse("[].nest{[#, #]}[2]").unwrap().eval_default().unwrap().to_string(), "[[[], []], [[], ...]]");
        // Von Neumann numerals
        assert_eq!(parse("[].nest{#~[#]}[3]").unwrap().eval_default().unwrap().to_string(), "[[], [[]], [[], ...]]");
        // Binomial coefficients
        assert_eq!(parse("[1].nest{(0~#)+(#~0)}[4]").unwrap().eval_default().unwrap().to_string(), "[1, 4, 6, 4, 1]");
        assert_eq!(parse("\"caesar\".nest{#.shift(1)}").unwrap().eval_default().unwrap().to_string(), "[\"dbftbs\", \"ecguct\", \"fdhvdu\", \"geiwev\", \"hfjxfw\", ...]");
        assert_eq!(parse("[0,1]~[1].nest{#~(#+1)}.flatten").unwrap().eval_default().unwrap().to_string(), "[0, 1, 1, 2, 1, ...]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("nest", eval_nest);
}
