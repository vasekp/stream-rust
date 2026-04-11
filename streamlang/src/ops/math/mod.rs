mod pi;
mod gcd;
mod minmax;
mod primes;
mod factor;
mod divisors;
mod total;
mod abs;
mod modular;
mod clamp;
mod sqrt;
mod greedy;
mod dot;

pub fn init(symbols: &mut crate::symbols::Symbols) {
    pi::init(symbols);
    gcd::init(symbols);
    minmax::init(symbols);
    primes::init(symbols);
    factor::init(symbols);
    divisors::init(symbols);
    total::init(symbols);
    abs::init(symbols);
    modular::init(symbols);
    clamp::init(symbols);
    sqrt::init(symbols);
    greedy::init(symbols);
    dot::init(symbols);
}
