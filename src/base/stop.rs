use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::LazyLock;

static STOP: LazyLock<AtomicBool> = LazyLock::new(|| AtomicBool::new(false));

pub fn reset_stop() {
    STOP.store(false, Ordering::SeqCst);
}

pub fn send_stop() {
    STOP.store(true, Ordering::SeqCst);
}

pub fn should_stop() -> bool {
    STOP.load(Ordering::SeqCst)
}
