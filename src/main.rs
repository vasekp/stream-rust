use streamlang::*;

fn main() {
    /*let params = vec![Item::new_imm(1), Item::new_imm(3)];
    let s = RangeStream::construct(params).unwrap();
    println!("{:.80?}", s);*/
    let input = r#"aah"ooo"j"#;
    let tk = Tokenizer::new(input);
    for t in tk {
        if let Ok(slice) = t {
            let start = unsafe { slice.as_ptr().offset_from(input.as_ptr()) };
            let length = slice.len();
            println!("{slice}: {start} + {length}");
        }
    }
}
