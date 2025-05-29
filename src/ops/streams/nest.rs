use crate::base::*;
use std::collections::VecDeque;

#[derive(Clone)]
struct NestSource {
    source: Item,
    body: Node,
    head: Head,
    env: Env
}

struct NestIterSource<'node> {
    body: &'node Node,
    prev: Item,
    env: &'node Env
}

#[derive(Clone)]
struct NestArgs {
    body: ENode,
    head: Head,
    env: Env
}

struct NestIterArgs<'node> {
    body: &'node ENode,
    prev: VecDeque<Item>,
    env: &'node Env
}

fn eval_nest(node: Node, env: &Env) -> Result<Item, StreamError> {
    match node.resolve() {
        RNode::Source(RNodeS { head, source, args: RArgs::One(Expr::Eval(body)) }) if body.source.is_none() && body.args.is_empty() => {
            Ok(Item::new_stream(NestSource{head, source: source.eval(env)?, body, env: env.clone()}))
        },
        RNode::NoSource(RNodeNS { head, args: RArgs::One(Expr::Eval(body)) }) if body.source.is_none() && !body.args.is_empty() => {
            Ok(Item::new_stream(NestArgs{head, body: body.eval_all(env)?, env: env.clone()}))
        },
        node => Err(StreamError::new("expected: source.nest({body}) or nest({body}(args))", node))
    }
}

impl Describe for NestSource {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, Some(&self.source), [&self.body], prec, env)
    }
}

impl Describe for NestArgs {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_with_env(&self.env, &self.head, None::<&Item>, [&self.body], prec, env)
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
        let args = self.body.args.iter().cloned().collect();
        Box::new(NestIterArgs{body: &self.body, prev: args, env: &self.env})
    }

    fn length(&self) -> Length {
        Length::Infinite
    }
}

impl Iterator for NestIterSource<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = Node::new(self.body.head.clone(),
            Some(std::mem::take(&mut self.prev).into()),
            vec![]);
        let item = match node.eval(self.env) {
            Ok(item) => item,
            Err(err) => return Some(Err(err))
        };
        self.prev = item.clone();
        Some(Ok(item))
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
        let node = Node::new(self.body.head.clone(), None, args);
        let item = match node.eval(self.env) {
            Ok(item) => item,
            Err(err) => return Some(Err(err))
        };
        self.prev.pop_front();
        self.prev.push_back(item.clone());
        Some(Ok(item))
    }
}

impl SIterator for NestIterArgs<'_> {
    fn len_remain(&self) -> Length {
        Length::Infinite
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_nest() {
        use super::*;
        use crate::parser::parse;
        assert_eq!(parse("1.nest{#+1}").unwrap().eval_default().unwrap().to_string(), "[2, 3, 4, 5, 6, ...]");
        assert_eq!(parse("1.nest({#})").unwrap().eval_default().unwrap().to_string(), "[1, 1, 1, 1, 1, ...]");
        assert_eq!(parse("'T'.nest{#+2}").unwrap().eval_default().unwrap().to_string(), "['V', 'X', 'Z', 'B', 'D', ...]");
        assert_eq!(parse("\"a\".nest({#~'x'})").unwrap().eval_default().unwrap().to_string(), "[\"ax\", \"axx\", \"axxx\", \"axxxx\", \"axxxxx\", ...]");
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
        // Fibonacci
        assert_eq!(parse("nest{#1+#2}(1,1)").unwrap().eval_default().unwrap().to_string(), "[2, 3, 5, 8, 13, ...]");
        // Von Neumann numerals
        assert_eq!(parse("[].nest{#~[#]}[3]").unwrap().eval_default().unwrap().to_string(), "[[], [[]], [[], ...]]");
        // Binomial coefficients
        assert_eq!(parse("[1].nest{(0~#)+(#~0)}[4]").unwrap().eval_default().unwrap().to_string(), "[1, 4, 6, 4, 1]");
        assert_eq!(parse("\"caesar\".nest{#+1}").unwrap().eval_default().unwrap().to_string(), "[\"dbftbs\", \"ecguct\", \"fdhvdu\", \"geiwev\", \"hfjxfw\", ...]");
        assert_eq!(parse("[0,1]~[1].nest{#~(#+1)}.flatten").unwrap().eval_default().unwrap().to_string(), "[0, 1, 1, 2, 1, ...]");
        assert_eq!(parse("nest{#1+#2}(1,1)").unwrap().eval_default().unwrap().describe(), "nest({#1+#2}(1, 1))");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("nest", eval_nest);
}
