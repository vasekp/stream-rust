mod select;
mod countif;
mod ifelse;
mod class;
mod r#while;
mod splitby;
mod all;
mod consec;

pub(crate) fn init(symbols: &mut crate::symbols::Symbols) {
    select::init(symbols);
    countif::init(symbols);
    ifelse::init(symbols);
    class::init(symbols);
    r#while::init(symbols);
    splitby::init(symbols);
    all::init(symbols);
    consec::init(symbols);
}
