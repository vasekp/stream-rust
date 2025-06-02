mod select;
mod count;
mod ifelse;
mod class;
mod r#while;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    select::init(keywords);
    count::init(keywords);
    ifelse::init(keywords);
    class::init(keywords);
    r#while::init(keywords);
}
