mod select;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    select::init(keywords);
}
