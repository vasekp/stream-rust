use crate::base::*;
use std::collections::VecDeque;

struct NestSource {
    source: Item,
    body: Function,
    head: Head,
}

struct NestIterSource {
    node: Rc<NestSource>,
    prev: Item,
}

struct NestArgs {
    /// The function, stripped of its arguments, which hold the initial values instead.
    body: Function,
    init: Vec<Item>,
    head: Head,
}

struct NestIterArgs {
    node: Rc<NestArgs>,
    prev: VecDeque<Item>,
}

fn eval_nest(node: &Node, env: &Env) -> SResult<Item> {
    let body = node.only_arg_checked()?.as_func()?;
    if body.args.is_empty()
        && let Some(source) = &node.source
    {
        Ok(Item::new_stream(NestSource {
            head: node.head.clone(),
            source: source.eval(env)?,
            body: Function::new(body, env),
        }))
    } else if !body.args.is_empty() && node.source.is_none() {
        let init = body
            .args
            .iter()
            .map(|arg| arg.eval(env))
            .collect::<SResult<Vec<_>>>()?;
        let bare = Rc::new(Node { head: body.head.clone(), source: None, args: vec![] });
        Ok(Item::new_stream(NestArgs { head: node.head.clone(), body: Function::new(&bare, env), init }))
    } else {
        Err(StreamError::usage(&node.head))
    }
}

impl Describe for NestSource {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_func(&self.head, env, Some(&self.body))
            .set_source(&self.source)
            .push_arg(&self.body)
            .finish(prec)
    }
}

impl Describe for NestArgs {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        let body = Node { head: self.body.head().clone(), source: None, args: self.init.clone() };
        DescribeBuilder::new_with_func(&self.head, env, Some(&self.body))
            .push_arg(&body)
            .finish(prec)
    }
}

impl Stream for NestSource {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        NestIterSource { prev: self.source.clone(), node: self }.wrap()
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl Stream for NestArgs {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        let init = self.init.iter().cloned().collect();
        NestIterArgs { prev: init, node: self }.wrap()
    }

    fn len(&self) -> Length {
        Length::Infinite
    }
}

impl PreIterator for NestIterSource {
    fn next(&mut self) -> SResult<Option<Item>> {
        let item = self.node.body.call_source(self.prev.clone())?;
        self.prev = item.clone();
        Ok(Some(item))
    }

    fn origin(&self) -> &Rc<NestSource> {
        &self.node
    }
}

impl PreIterator for NestIterArgs {
    fn next(&mut self) -> SResult<Option<Item>> {
        let item = self.node.body.call_args(self.prev.iter().cloned().collect())?;
        self.prev.pop_front();
        self.prev.push_back(item.clone());
        Ok(Some(item))
    }

    fn origin(&self) -> &Rc<NestArgs> {
        &self.node
    }
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("nest", eval_nest, r#"
Source form: an infinite stream whose first item is `input.func` and each further item is `func` applied to the previous one.
Argument form: an infinite stream whose first item is `func(arg1, ..., argM)` and each further item is `func` applied to the `M` most recent items (the arguments count as the first ones).
= input.?{func}
= ?{func}(arg1, ..., argM)
> 1.?{#*2} => [2, 4, 8, 16, 32, ...]
> ?{#1+#2}(1,1) => [2, 3, 5, 8, 13, ...] ; Fibonacci sequence
> "abc".?{#+1} => ["bcd", "cde", "def", "efg", "fgh", ...]
: fold
"#);
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
