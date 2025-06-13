mod chars;
mod split;
mod cat;
mod ulcase;
mod replace;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    chars::init(keywords);
    split::init(keywords);
    cat::init(keywords);
    ulcase::init(keywords);
    replace::init(keywords);
}
