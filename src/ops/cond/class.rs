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
            Item::Stream(stm) => Ok(stm.is_empty()),
            Item::String(stm) => Ok(stm.is_empty()),
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
        "isupper" => Ok(match item {
            Item::Char(ch) => ch.case() == CharCase::Upper,
            Item::String(stm) => {
                let mut indet = true;
                for ch in stm.iter() {
                    check_stop!();
                    let ch = ch?;
                    match ch.case() {
                        CharCase::Lower | CharCase::Mixed => return Ok(false),
                        CharCase::Upper => indet = false,
                        CharCase::Indeterminate => ()
                    }
                }
                !indet
            },
            _ => return Err(format!("expected character or string, found {:?}", item).into())
        }),
        "islower" => Ok(match item {
            Item::Char(ch) => ch.case() == CharCase::Lower,
            Item::String(stm) => {
                let mut indet = true;
                for ch in stm.iter() {
                    check_stop!();
                    let ch = ch?;
                    match ch.case() {
                        CharCase::Upper | CharCase::Mixed => return Ok(false),
                        CharCase::Lower => indet = false,
                        CharCase::Indeterminate => ()
                    }
                }
                !indet
            },
            _ => return Err(format!("expected character or string, found {:?}", item).into())
        }),
        "isnumeric" => {
            if let Item::Char(ch) = item {
                return match ch {
                    Char::Single(ch) => Ok(ch.is_ascii_digit()),
                    _ => Ok(false)
                }
            }
            let Item::String(stm) = item else { return Err(format!("expected character or string, found {:?}", item).into()); };
            let mut iter = stm.iter();
            let mut nonempty = match iter.next().transpose()? {
                Some(ch) => match ch {
                    Char::Single('+'|'-') => false,
                    Char::Single(x) if x.is_ascii_digit() => true,
                    _ => return Ok(false)
                },
                None => return Ok(false)
            };
            for ch in iter {
                match ch? {
                    Char::Single(x) if x.is_ascii_digit() => nonempty = true,
                    _ => return Ok(false)
                }
            }
            Ok(nonempty)
        },
        _ => unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_class() {
        use super::*;

        test_eval!("[1,true,'2',\"3\",[]]:isnum" => "[true, false, false, false, false]");
        test_eval!("[1,true,'2',\"3\",[]]:isbool" => "[false, true, false, false, false]");
        test_eval!("[1,true,'2',\"3\",[]]:ischar" => "[false, false, true, false, false]");
        test_eval!("[1,true,'2',\"3\",[]]:isstream" => "[false, false, false, false, true]");
        test_eval!("[1,true,'2',\"3\",[]]:isstring" => "[false, false, false, true, false]");
        test_eval!("[0,1,2,3,'a']:isodd" => "[false, true, false, true, <!>");
        test_eval!("[0,1,2,3,'a']:iseven" => "[true, false, true, false, <!>");
        test_eval!("[\"\",[],\"a\",[0],0]:isempty" => "[true, true, false, false, <!>");
        test_eval!("['a','A','á',\"a\"]:isalpha" => "[true, true, false, <!>");
        test_eval!("alpha(['Á'],['a','A','á','Á',\"a\"]:isalpha)" => "[false, false, true, true, <!>");
        test_eval!("['a','á','ch',\"a\"]:isascii" => "[true, false, false, <!>");
        test_eval!("['0','00',\"0\"]:isdigit" => "[true, false, <!>");
        test_eval!("[' ','.','\r','\n']:iswhite" => "[true, false, true, true]");
        test_eval!("['á','Á','ch','Ch','CH']:isupper" => "[false, true, false, false, true]");
        test_eval!("['á','Á','ch','Ch','CH']:islower" => "[true, false, true, false, false]");
        test_eval!("[' ','  ',\"A B\",\"A b\",['Ch'].string]:isupper" => "[false, false, true, false, false]");
        test_eval!("[' ','  ',\"a b\",['Ch'].string,['ch'].string]:islower" => "[false, false, true, false, true]");
        test_eval!("[' ','  ',\"a b\",['Ch'].string,['ch'].string]:islower" => "[false, false, true, false, true]");
        test_eval!("['0', '+', \"0\", \"+\", 0]:isnumeric" => "[true, false, true, false, <!>");
        test_eval!("[\"-123\",\"+-1\",'a'.repeat,'00',[1].string]:isnumeric" => "[true, false, false, false, <!>");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("isnum", eval_class, r#"
Evaluates to `true` if `input` is a number, `false` otherwise.
= input.?
> 1.? => true
> [1, 2].? => false
> "1".? => false
: isnum
: isbool
: ischar
: isstream
: isstring
: isodd
: iseven
: isnumeric
"#);
    symbols.insert("isbool", eval_class, r#"
Evaluates to `true` if `input` is a boolean value (`true` or `false`), `false` otherwise.
= input.?
> 1.? => false
> true.? => true
> false.? => true
> "true".? => false
: isnum
: ischar
: isstream
: isstring
"#);
    symbols.insert("ischar", eval_class, r#"
Evaluates to `true` if `input` is a character, `false` otherwise.
= input.?
> 1.? => false
> 'x'.? => true
> "x".? => false
: isnum
: isbool
: isstream
: isstring
: isalpha
: isdigit
: iswhite
: isupper
: islower
: isnumeric
"#);
    symbols.insert("isstream", eval_class, r#"
Evaluates to `true` if `input` is a stream, `false` otherwise.
= input.?
> 1.? => false
> "string".? => false
> [1, 2].? => true
> [].? => true
: isnum
: isbool
: ischar
: isstring
: isempty
"#);
    symbols.insert("isstring", eval_class, r#"
Evaluates to `true` if `input` is a string, `false` otherwise.
= input.?
> 1.? => false
> "string".? => true
> ['a', 'b'].? => false
: isnum
: isbool
: ischar
: isstream
: isempty
"#);
    symbols.insert("isodd", eval_class, r#"
Evaluates to `true` if `number` is odd, `false` otherwise.
= number.?
> 1.? => true
> 0.? => false
> 'a'.? => !not a number
: isnum
: isnumeric
"#);
    symbols.insert("iseven", eval_class, r#"
Evaluates to `true` if `number` is even, `false` otherwise.
= number.?
> 1.? => false
> 0.? => true
> 'a'.? => !not a number
: isnum
: isnumeric
"#);
    symbols.insert("isempty", eval_class, r#"
Evaluates to `true` if `input` is an empty stream or an empty string, `false` otherwise.
= input.?
> [].? => true
> "".? => true
> [[]].? => false
> " ".? => false
: isstream
: isstring
: isnumeric
"#);
    symbols.insert("isalpha", eval_class, r#"
Evaluates to `true` if `char` is alphabetic (a member of `?alpha`), `false` otherwise.
= char.?
> 'a'.? => true
> 'A'.? => true
> 'á'.? => false
> "a".? => !not a character
: ischar
: isascii
: isdigit
: iswhite
: isupper
: islower
: isnumeric
"#);
    symbols.insert("isascii", eval_class, r#"
Evaluates to `true` if `char` is ASCII, `false` otherwise.
= char.?
> 'a'.? => true
> '#'.? => true
> 'á'.? => false
> "a".? => !not a character
: ischar
: isalpha
: isdigit
: iswhite
: isupper
: islower
: isnumeric
"#);
    symbols.insert("isdigit", eval_class, r#"
Evaluates to `true` if `char` is a digit (0 through 9), `false` otherwise.
= char.?
> 'a'.? => false
> '5'.? => true
> "5".? => !not a character
: ischar
: isalpha
: isascii
: iswhite
: isupper
: islower
: isnumeric
"#);
    symbols.insert("iswhite", eval_class, r#"
Evaluates to `true` if `char` is a whitespace character, `false` otherwise.
= char.?
> ' '.? => true
> '\n'.? => true
> '5'.? => false
> " ".? => !not a character
: ischar
: isalpha
: isascii
: isdigit
: isupper
: islower
: isnumeric
"#);
    symbols.insert("isupper", eval_class, r#"
For characters: evaluates to `true` if `char` is uppercase, `false` otherwise.
For strings: evaluates to `true` if all alphabetic characters are uppercase and there is at least one of them.
= char.?
= string.?
> ' '.? => false
> 'A'.? => true
> "ABC".? => true
> "Abc".? => false
> "A B".? => true
> " ".? => false
: ischar
: isstring
: isalpha
: isascii
: isdigit
: iswhite
: islower
: isnumeric
"#);
    symbols.insert("islower", eval_class, r#"
For characters: evaluates to `true` if `char` is lowercase, `false` otherwise.
For strings: evaluates to `true` if all alphabetic characters are lowercase and there is at least one of them.
= char.?
= string.?
> ' '.? => false
> 'a'.? => true
> "abc".? => true
> "Abc".? => false
> "a b".? => true
> " ".? => false
: ischar
: isstring
: isalpha
: isascii
: isdigit
: iswhite
: isupper
: isnumeric
"#);
    symbols.insert("isnumeric", eval_class, r#"
For characters: evaluates to `true` if `char` is a digit (0 through 9), `false` otherwise.
For strings: evaluates to `true` if the string represents a number. Apart from digits, it can start 
with a sign `+` or `-`.
= char.?
= string.?
> '1'.? => true
> '+'.? => false
> "123".? => true
> "+0".? => true
> "+".? => false
: ischar
: isstring
: isalpha
: isascii
: isdigit
: iswhite
: isupper
: islower
: strnum
"#);
}
