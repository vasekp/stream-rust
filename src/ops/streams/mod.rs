mod seq;
mod range;
mod len;
mod riffle;
mod repeat;
mod skip;

pub fn init(keywords: &mut crate::keywords::Keywords) {
    seq::init(keywords);
    range::init(keywords);
    len::init(keywords);
    riffle::init(keywords);
    repeat::init(keywords);
    skip::init(keywords);
}
