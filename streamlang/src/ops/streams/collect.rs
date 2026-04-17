use crate::base::*;

fn eval_collect(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = node.only_arg_checked()?.as_func()?;
    let mut map: Vec<(Item, Vec<Item>)> = Vec::new();
    'a: for res in stm.iter().transposed() {
        check_stop!();
        let item = res?;
        let new_key = func.with_source((&item).into())?.eval(env)?;
        for (key, vec) in &mut map {
            if key.try_eq(&new_key)? {
                vec.push(item);
                continue 'a;
            }
        }
        // not found
        map.push((new_key, vec![item]));
    }
    Ok(Item::from(map.into_iter()
        .map(|(key, val)| Item::from([key, Item::from(val)]))
        .collect::<Vec<_>>()))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_collect() {
        use super::*;
        test_eval!("(1..7).collect{#%3}" : 20 => "[[1, [1, 4, 7]], [2, [2, 5]], [0, [3, 6]]]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("collect", eval_collect, r#"
Groups the elements `x` of `stream` agreeing in the value of `x.func`.
Returns a stream of pairs `[key, [x1, x2, ...]]` in order of appearance.
= stream.?(func)
> ["one", "two", "three"].?(?last) : 15 => [['e', ["one", "three"]], ['o', ["two"]]]
: group
: runs
: drep
"#);
}
