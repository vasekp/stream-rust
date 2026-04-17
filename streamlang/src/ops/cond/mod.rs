mod select;
mod countif;
mod ifelse;
mod class;
mod r#while;
mod all;
mod consec;
mod isin;

pub(crate) fn init(symbols: &mut crate::symbols::Symbols) {
    select::init(symbols);
    countif::init(symbols);
    ifelse::init(symbols);
    class::init(symbols);
    r#while::init(symbols);
    all::init(symbols);
    consec::init(symbols);
    isin::init(symbols);
}
