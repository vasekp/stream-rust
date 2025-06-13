mod chars;
mod split;
mod cat;
mod ulcase;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    chars::init(keywords);
    split::init(keywords);
    cat::init(keywords);
    ulcase::init(keywords);
}
