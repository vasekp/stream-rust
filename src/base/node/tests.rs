use crate::base::*;

#[test]
fn test_block() {
    use crate::parser::parse;
    assert_eq!(parse("{#1}(3,4)").unwrap().eval().unwrap().to_string(), "3");
    assert_eq!(parse("{#2}(3,4)").unwrap().eval().unwrap().to_string(), "4");
    assert!(parse("{#3}(3,4)").unwrap().eval().is_err());
    assert!(parse("#1").unwrap().eval().is_err());
    assert_eq!(parse("1.{2}(3)").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("{#1+{#1}(2,3)}(4,5)").unwrap().eval().unwrap().to_string(), "6");
    assert_eq!(parse("{#1}({#2}(3,4),5)").unwrap().eval().unwrap().to_string(), "4");
    assert_eq!(parse("{range(#1,#2)}(3,4)").unwrap().eval().unwrap().describe(0), "range(3, 4)");
}

#[test]
fn test_args() {
    use crate::parser::parse;
    assert_eq!(parse("range@[3]").unwrap().eval().unwrap().to_string(), "[1, 2, 3]");
    assert_eq!(parse("range@range(3)").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("range@range(3)").unwrap().eval().unwrap().to_string(), "[1]");
    assert_eq!(parse("range@[3][2]").unwrap().eval().unwrap().to_string(), "2");
    assert_eq!(parse("range@range(3)[1]").unwrap().eval().unwrap().to_string(), "1");
    assert!(parse("range@3").unwrap().eval().is_err());
    assert!(parse("range@seq").unwrap().eval().is_err());
    assert_eq!(parse("range@[3,4]").unwrap().eval().unwrap().describe(0), "range(3, 4)");
}

#[test]
fn test_describe() {
    use crate::parser::parse;

    // chaining, args, block
    let orig = parse("a.b(c,d).{e}(f,g)").unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);

    // value types
    let orig = parse(r#"[1,true,'a"\'b\nc',"a'b\"c\n",[],""]"#).unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);

    // character escaping
    let orig = parse("'a\\n\\r\\t\\'\\\"'").unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);

    // operators, precedence
    let orig = parse("1+(-2-3-4^(-5)*2)").unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);

    // lists, parts
    let orig = parse("[1,2][3,4] + [[1,2]][[3,4]]").unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);

    // map
    let orig = parse("a:b:c").unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);

    // args
    let orig = parse("a@b@c(d)[e]").unwrap();
    let copy = parse(&orig.describe(0)).unwrap();
    assert_eq!(orig, copy);
}

