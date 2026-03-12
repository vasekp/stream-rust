use crate::base::*;

#[test]
fn test_block() {
    test_eval!("{#1}(3,4)" => "3");
    test_eval!("{#2}(3,4)" => "4");
    test_eval!("{#3}(3,4)" => err);
    test_eval!("{##}(3,4)" => "[3, 4]");
    test_eval!("{##}(3)" => "[3]");
    test_eval!("{##}()" => "[]");
    test_eval!("{##}" => "[]");
    test_eval!("#1" => err);
    test_eval!("##" => err);
    test_eval!("1.{2}(3)" => "2");
    test_eval!("{#1+{#1}(2,3)}(4,5)" => "6");
    test_eval!("{#1}({#2}(3,4),5)" => "4");
    test_describe!("{range(#1,#2)}(3,4)" => "range(3, 4)");
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

