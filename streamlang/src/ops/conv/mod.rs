mod ord;
mod digits;
mod numstr;
mod numdig;
mod digstr;

pub(crate) fn init(symbols: &mut crate::symbols::Symbols) {
    ord::init(symbols);
    numstr::init(symbols);
    numdig::init(symbols);
    digstr::init(symbols);
}
