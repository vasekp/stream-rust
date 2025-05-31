use crate::base::*;

fn eval_class(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    match &rnode {
        RNodeS { source: item, args: RArgs::Zero, head: Head::Symbol(head) } =>
            Ok(try_with!(rnode, eval_inner(head, item, env).map(Item::Bool)?)),
        _ => Err(StreamError::new("no arguments accepted", rnode))
    }
}

fn eval_inner(head: &str, item: &Item, env: &Env) -> Result<bool, BaseError> {
    match head {
        "isnum" => Ok(matches!(item, Item::Number(_))),
        "isbool" => Ok(matches!(item, Item::Bool(_))),
        "ischar" => Ok(matches!(item, Item::Char(_))),
        "isstream" => Ok(matches!(item, Item::Stream(_))),
        "isstring" => Ok(matches!(item, Item::String(_))),
        "isodd" => Ok(item.as_num()?.is_odd()),
        "iseven" => Ok(item.as_num()?.is_even()),
        "isempty" => match item {
            Item::Stream(stm) | Item::String(stm) => Ok(stm.is_empty()),
            _ => Err(format!("expected stream or string, found {:?}", item).into())
        },
        "isalpha" => Ok(env.alpha.contains(item.as_char()?)),
        "isascii" => Ok(match item.as_char()? {
            Char::Single(ch) => ch.is_ascii(),
            _ => false
        }),
        "isdigit" => Ok(match item.as_char()? {
            Char::Single(ch) => ch.is_ascii_digit(),
            _ => false
        }),
        "iswhite" => Ok(match item.as_char()? {
            Char::Single(ch) => ch.is_whitespace(),
            _ => false
        }),
        "isupper" => Ok(item.as_char()?.case() == CharCase::Upper),
        "islower" => Ok(item.as_char()?.case() == CharCase::Lower),
        _ => unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_class() {
        //use super::*;
        use crate::parser::parse;
        assert_eq!(parse("[1,true,'2',\"3\",[]]:isnum").unwrap().eval_default().unwrap().to_string(), "[true, false, false, false, false]");
        assert_eq!(parse("[1,true,'2',\"3\",[]]:isbool").unwrap().eval_default().unwrap().to_string(), "[false, true, false, false, false]");
        assert_eq!(parse("[1,true,'2',\"3\",[]]:ischar").unwrap().eval_default().unwrap().to_string(), "[false, false, true, false, false]");
        assert_eq!(parse("[1,true,'2',\"3\",[]]:isstream").unwrap().eval_default().unwrap().to_string(), "[false, false, false, false, true]");
        assert_eq!(parse("[1,true,'2',\"3\",[]]:isstring").unwrap().eval_default().unwrap().to_string(), "[false, false, false, true, false]");
        assert_eq!(parse("[0,1,2,3,'a']:isodd").unwrap().eval_default().unwrap().to_string(), "[false, true, false, true, <!>");
        assert_eq!(parse("[0,1,2,3,'a']:iseven").unwrap().eval_default().unwrap().to_string(), "[true, false, true, false, <!>");
        assert_eq!(parse("[\"\",[],\"a\",[0],0]:isempty").unwrap().eval_default().unwrap().to_string(), "[true, true, false, false, <!>");
        assert_eq!(parse("['a','A','á',\"a\"]:isalpha").unwrap().eval_default().unwrap().to_string(), "[true, true, false, <!>");
        assert_eq!(parse("alpha(['Á'],['a','A','á','Á',\"a\"]:isalpha)").unwrap().eval_default().unwrap().to_string(), "[false, false, true, true, <!>");
        assert_eq!(parse("['a','á','ch',\"a\"]:isascii").unwrap().eval_default().unwrap().to_string(), "[true, false, false, <!>");
        assert_eq!(parse("['0','00',\"0\"]:isdigit").unwrap().eval_default().unwrap().to_string(), "[true, false, <!>");
        assert_eq!(parse("[' ','.','\r','\n']:iswhite").unwrap().eval_default().unwrap().to_string(), "[true, false, true, true]");
        assert_eq!(parse("['a','A','ch','Ch','CH']:isupper").unwrap().eval_default().unwrap().to_string(), "[false, true, false, false, true]");
        assert_eq!(parse("['a','A','ch','Ch','CH']:islower").unwrap().eval_default().unwrap().to_string(), "[true, false, true, false, false]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("isnum", eval_class);
    keywords.insert("isbool", eval_class);
    keywords.insert("ischar", eval_class);
    keywords.insert("isstream", eval_class);
    keywords.insert("isstring", eval_class);
    keywords.insert("isodd", eval_class);
    keywords.insert("iseven", eval_class);
    keywords.insert("isempty", eval_class);
    keywords.insert("isalpha", eval_class);
    keywords.insert("isascii", eval_class);
    keywords.insert("isdigit", eval_class);
    keywords.insert("iswhite", eval_class);
    keywords.insert("isupper", eval_class);
    keywords.insert("islower", eval_class);
}
