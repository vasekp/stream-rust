use std::sync::atomic::{AtomicBool, Ordering};

static STOP: AtomicBool = AtomicBool::new(false);

pub fn reset_stop() {
    STOP.store(false, Ordering::SeqCst);
}

pub fn send_stop() {
    STOP.store(true, Ordering::SeqCst);
}

pub fn should_stop() -> bool {
    STOP.load(Ordering::SeqCst)
}
