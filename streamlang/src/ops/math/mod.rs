mod pi;
mod gcd;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    pi::init(symbols);
    gcd::init(symbols);
}
