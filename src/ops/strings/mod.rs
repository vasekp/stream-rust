mod chars;
mod split;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    chars::init(keywords);
    split::init(keywords);
}
