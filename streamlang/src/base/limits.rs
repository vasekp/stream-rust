/// Streams up to this length are sorted eagerly; `rev` reads its input in chunks of this size.
/// Lowered under test so that the lazy paths are reachable with small inputs.
#[cfg(not(test))]
pub(crate) const CACHE_LEN: usize = 100;
#[cfg(test)]
pub(crate) const CACHE_LEN: usize = 10;
/// Upper bound on preallocation; streams up to this length are reversed eagerly.
pub(crate) const RESERVE_CAP: usize = 1000000;
