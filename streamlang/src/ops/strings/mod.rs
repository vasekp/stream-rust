mod chars;
mod split;
mod cat;
mod ulcase;
mod replace;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    chars::init(symbols);
    split::init(symbols);
    cat::init(symbols);
    ulcase::init(symbols);
    replace::init(symbols);
}
