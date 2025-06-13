mod select;
mod countif;
mod ifelse;
mod class;
mod r#while;
mod splitby;

pub(crate) fn init(keywords: &mut crate::keywords::Keywords) {
    select::init(keywords);
    countif::init(keywords);
    ifelse::init(keywords);
    class::init(keywords);
    r#while::init(keywords);
    splitby::init(keywords);
}
