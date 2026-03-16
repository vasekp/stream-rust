mod pi;
mod gcd;
mod primes;
mod minmax;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    pi::init(symbols);
    gcd::init(symbols);
    primes::init(symbols);
    minmax::init(symbols);
}
