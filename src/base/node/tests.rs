use crate::base::*;

#[test]
fn test_block() {
    use crate::parser::parse;
    assert_eq!(parse("{#1}(3,4)").unwrap().eval_default().unwrap().to_string(), "3");
    assert_eq!(parse("{#2}(3,4)").unwrap().eval_default().unwrap().to_string(), "4");
    assert!(parse("{#3}(3,4)").unwrap().eval_default().is_err());
    assert_eq!(parse("{##}(3,4)").unwrap().eval_default().unwrap().to_string(), "[3, 4]");
    assert_eq!(parse("{##}(3)").unwrap().eval_default().unwrap().to_string(), "[3]");
    assert_eq!(parse("{##}()").unwrap().eval_default().unwrap().to_string(), "[]");
    assert_eq!(parse("{##}").unwrap().eval_default().unwrap().to_string(), "[]");
    assert!(parse("#1").unwrap().eval_default().is_err());
    assert!(parse("##").unwrap().eval_default().is_err());
    assert_eq!(parse("1.{2}(3)").unwrap().eval_default().unwrap().to_string(), "2");
    assert_eq!(parse("{#1+{#1}(2,3)}(4,5)").unwrap().eval_default().unwrap().to_string(), "6");
    assert_eq!(parse("{#1}({#2}(3,4),5)").unwrap().eval_default().unwrap().to_string(), "4");
    assert_eq!(parse("{range(#1,#2)}(3,4)").unwrap().eval_default().unwrap().describe(), "range(3, 4)");
}

#[test]
fn test_describe() {
    use crate::parser::parse;

    // chaining, args, block
    let orig = parse("a.b(c,d).{e}(f,g)").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // value types
    let orig = parse(r#"[1,true,'a"\'b\nc',"a'b\"c\n",[],""]"#).unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // character escaping
    let orig = parse("'a\\n\\r\\t\\'\\\"'").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // operators, precedence
    let orig = parse("1+(-2-3-4^(-5)*2)").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // lists, parts
    let orig = parse("[1,2][3,4] + [[1,2]][[3,4]]").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // map
    let orig = parse("a:b:c").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    // args
    let orig = parse("a@b@c(d)[e]").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);

    let orig = parse("a@b.c@(d.e)").unwrap();
    let copy = parse(&orig.describe()).unwrap();
    assert_eq!(orig, copy);
}

