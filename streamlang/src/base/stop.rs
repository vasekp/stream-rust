use std::sync::atomic::{AtomicBool, Ordering};

static STOP: AtomicBool = AtomicBool::new(false);

/// Clears the interrupt request. Called by [`Session::process()`](crate::session::Session::process)
/// before every evaluation.
pub fn reset_stop() {
    STOP.store(false, Ordering::SeqCst);
}

/// Requests the running evaluation to stop. The request stays in effect, making every subsequent
/// `check_stop!()` fail with [`StreamError::interrupt()`](crate::base::StreamError::interrupt),
/// until [`reset_stop()`] is called.
pub fn send_stop() {
    STOP.store(true, Ordering::SeqCst);
}

/// Returns whether an interrupt request is pending. Evaluation code uses this through
/// `check_stop!()`.
pub fn should_stop() -> bool {
    STOP.load(Ordering::SeqCst)
}
