mod ord;
mod numstr;
mod numdig;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    ord::init(keywords);
    numstr::init(keywords);
    numdig::init(keywords);
}
