/// Streams up to this length are sorted or reversed eagerly. Lowered under test so that the lazy
/// paths are reachable with small inputs.
#[cfg(not(test))]
pub(crate) const CACHE_LEN: usize = 100;
#[cfg(test)]
pub(crate) const CACHE_LEN: usize = 10;
pub(crate) const RESERVE_CAP: usize = 1000000;
