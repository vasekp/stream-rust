mod counts;

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    counts::init(symbols);
}
