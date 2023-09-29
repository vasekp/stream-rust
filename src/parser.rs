use std::str::Chars;
use std::iter::Peekable;

pub struct Tokenizer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    index: usize
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer{input, chars: input.chars().peekable(), index: 0}
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    slice: &'a str,
    start: usize,
    length: usize
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.chars.next() {
            let start = self.index;
            let mut length = 1;
            while let Some(ch2) = self.chars.peek() {
                if *ch2 != ch {
                    break;
                }
                length = length + 1;
                self.chars.next();
            }
            self.index = self.index + length;
            Some(Token{slice: &self.input[start..self.index], start, length})
        } else {
            None
        }
    }
}
