mod ord;
mod numstr;
mod numdig;

pub(crate) fn init(symbols: &mut crate::symbols::Symbols) {
    ord::init(symbols);
    numstr::init(symbols);
    numdig::init(symbols);
}
