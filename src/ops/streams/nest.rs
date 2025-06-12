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
        let item = iter_try_expr!(node.eval(self.env));
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
        let item = iter_try_expr!(node.eval(self.env));
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
        test_eval!("1.nest{#+1}" => "[2, 3, 4, 5, 6, ...]");
        test_eval!("1.nest({#})" => "[1, 1, 1, 1, 1, ...]");
        test_eval!("'T'.nest{#+2}" => "['V', 'X', 'Z', 'B', 'D', ...]");
        test_eval!("\"a\".nest({#~'x'})" => "[\"ax\", \"axx\", \"axxx\", \"axxxx\", \"axxxxx\", ...]");
        test_eval!("1.nest{#1}" => "[<!>");
        test_eval!("1.nest(2.{#})" => err);
        test_eval!("1.nest{#}(1)" => err);
        test_eval!("nest{#1+#2}(1,1)" => "[2, 3, 5, 8, 13, ...]");
        test_eval!("nest{#1+#2}(1)" => "[<!>");
        test_eval!("nest{#}(1,1)" => "[<!>");
        test_eval!("nest({#}())" => err);

        test_eval!("1.nest{#*2}[64]" => "18446744073709551616");
        test_eval!("[].nest{[#]}[3]" => "[[[[]]]]");
        test_eval!("[].nest{[#, #]}[2]" => "[[[], []], [[], ...]]");
        // Fibonacci
        test_eval!("nest{#1+#2}(1,1)" => "[2, 3, 5, 8, 13, ...]");
        // Von Neumann numerals
        test_eval!("[].nest{#~[#]}[3]" => "[[], [[]], [[], ...]]");
        // Binomial coefficients
        test_eval!("[1].nest{(0~#)+(#~0)}[4]" => "[1, 4, 6, 4, 1]");
        test_eval!("\"caesar\".nest{#+1}" => "[\"dbftbs\", \"ecguct\", \"fdhvdu\", \"geiwev\", \"hfjxfw\", ...]");
        test_eval!("[0,1]~[1].nest{#~(#+1)}.flatten" => "[0, 1, 1, 2, 1, ...]");
        test_describe!("nest{#1+#2}(1,1)" => "nest({#1+#2}(1, 1))");
        // 10-adic automorphic number
        test_eval!("5.nest{#^2%10^50}.windows(2).select{#[1]==#[2]}.first.first" => "57423423230896109004106619977392256259918212890625");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("nest", eval_nest);
}
