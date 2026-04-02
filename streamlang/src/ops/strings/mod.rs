mod chars;
mod split;
mod cat;
mod ulcase;
mod subst;
mod sewith;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    chars::init(symbols);
    split::init(symbols);
    cat::init(symbols);
    ulcase::init(symbols);
    subst::init(symbols);
    sewith::init(symbols);
}
