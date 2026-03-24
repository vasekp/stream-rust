mod pi;
mod gcd;
mod minmax;
mod primes;
mod factor;
mod divisors;
mod total;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    pi::init(symbols);
    gcd::init(symbols);
    minmax::init(symbols);
    primes::init(symbols);
    factor::init(symbols);
    divisors::init(symbols);
    total::init(symbols);
}
