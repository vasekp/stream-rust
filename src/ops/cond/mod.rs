mod select;
mod count;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    select::init(keywords);
    count::init(keywords);
}
