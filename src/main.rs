use streamlang::*;

fn main() {
    let params = vec![Item::new_atomic(1), Item::new_atomic(3)];
    let s = RangeStream::construct(params).unwrap();
    println!("{:.80?}", s);
    /*let input = r#"123 + 45"#;
    let tk = Tokenizer::new(input);
    for t in tk {
        if let Ok(slice) = t {
            let start = unsafe { slice.as_ptr().offset_from(input.as_ptr()) } as usize;
            let length = slice.len();
            //println!("{slice}: {start} + {length}");
            println!("\x1b[8m{}\x1b[0m{}", &input[0..start], &input[start..(start + length)]);
        } else {
            println!("<!>");
        }
    }*/
}
