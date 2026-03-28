mod chars;
mod split;
mod cat;
mod ulcase;
mod replace;
mod sewith;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    chars::init(symbols);
    split::init(symbols);
    cat::init(symbols);
    ulcase::init(symbols);
    replace::init(symbols);
    sewith::init(symbols);
}
