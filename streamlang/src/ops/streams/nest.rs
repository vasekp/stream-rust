use crate::base::*;
use std::collections::VecDeque;

struct NestSource {
    source: Item,
    body: Rc<Node>,
    head: Head,
    env: Env
}

struct NestIterSource {
    node: Rc<NestSource>,
    prev: Item,
}

struct NestArgs {
    body: Node<Item>,
    head: Head,
    env: Env
}

struct NestIterArgs {
    node: Rc<NestArgs>,
    prev: VecDeque<Item>,
}

fn eval_nest(node: &Node, env: &Env) -> SResult<Item> {
    let body = if let [Expr::Eval(body)] = &node.args[..] && body.source.is_none() {
        body
    } else {
        return Err(StreamError::usage(&node.head));
    };
    if body.args.is_empty() && let Some(source) = &node.source {
        Ok(Item::new_stream(NestSource{
            head: node.head.clone(),
            source: source.eval(env)?,
            body: Rc::clone(body),
            env: env.clone()}))
    } else if !body.args.is_empty() && node.source.is_none() {
        Ok(Item::new_stream(NestArgs{
            head: node.head.clone(),
            body: body.eval_all(env)?,
            env: env.clone()}))
    } else {
        Err(StreamError::usage(&node.head))
    }
}

impl Describe for NestSource {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.body)
            .finish(prec)
    }
}

impl Describe for NestArgs {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .push_arg(&self.body)
            .finish(prec)
    }
}

impl Stream for NestSource {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        NestIterSource{prev: self.source.clone(), node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Stream for NestArgs {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let args = self.body.args.iter().cloned().collect();
        NestIterArgs{prev: args, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl PreIterator for NestIterSource {
    fn next(&mut self) -> SResult<Option<Item>> {
        let node = Node::new(self.node.body.head.clone(),
            Some(Expr::from(&self.prev)),
            vec![]);
        let item = node.eval(&self.node.env)?;
        self.prev = item.clone();
        Ok(Some(item))
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn origin(&self) -> &Rc<NestSource> {
        &self.node
    }
}

impl PreIterator for NestIterArgs {
    fn next(&mut self) -> SResult<Option<Item>> {
        let args = self.prev.iter()
            .map(|item| Expr::Imm(item.to_owned()))
            .collect();
        let node = Node::new(self.node.body.head.clone(), None, args);
        let item = node.eval(&self.node.env)?;
        self.prev.pop_front();
        self.prev.push_back(item.clone());
        Ok(Some(item))
    }

    fn len_remain(&self) -> Length {
        Length::Infinite
    }

    fn origin(&self) -> &Rc<NestArgs> {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_nest() {
        use super::*;

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
        test_eval!("\"caesar\".nest{#+1}" => "[\"dbftbs\", \"ecguct\", \"fdhvdu\", \"geiwev\", \"hfjxfw\", ...]");
        test_eval!("[0,1]~[1].nest{#~(#+1)}.flatten" => "[0, 1, 1, 2, 1, ...]");
        test_describe!("nest{#1+#2}(1,1)" => "nest({#1+#2}(1, 1))");
        test_describe!("[].nest{[#]}" => "[].nest({[#]})");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("nest", eval_nest, r#"
Input-form: a stream where `s[n]` is the result of `s[n-1].func` (`input.func` if first).
Argument-form: a stream where `s[n]` is computed from `m` previous items as `func(..., s[n-1])`, 
starting with `func(arg1, ..., argM)`.
= input.?{func}
= ?{func}(arg1, ..., argM)
> 1.?{#*2} => [2, 4, 8, 16, 32, ...]
> ?{#1+#2}(1,1) => [2, 3, 5, 8, 13, ...] ; Fibonacci sequence
> "abc".?{#+1} => ["bcd", "cde", "def", "efg", "fgh", ...]
: fold
"#);
}
