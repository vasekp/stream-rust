mod select;
mod count;
mod ifelse;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    select::init(keywords);
    count::init(keywords);
    ifelse::init(keywords);
}
