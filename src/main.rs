use streamlang::*;

fn main() {
    /*let params = vec![Item::new_atomic(1), Item::new_atomic(3)];
    let s = RangeStream::construct(params).unwrap();
    println!("{:.80?}", s);*/

    let input = r#""stř".stř"#;
    println!("{input}");
    parse(input);

    let input = r#"str".stř"#;
    println!("{input}");
    parse(input);
}
