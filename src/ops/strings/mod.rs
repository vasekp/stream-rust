mod chars;
mod split;
mod cat;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    chars::init(keywords);
    split::init(keywords);
    cat::init(keywords);
}
