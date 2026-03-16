mod pi;
mod gcd;
mod primes;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    pi::init(symbols);
    gcd::init(symbols);
    primes::init(symbols);
}
