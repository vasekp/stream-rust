use std::sync::{LazyLock, Mutex};
use std::collections::HashSet;

static STRINGS: LazyLock<Mutex<HashSet<&'static str>>> = LazyLock::new(Default::default);

pub fn intern(s: &str) -> &'static str {
    let mut set = STRINGS.lock().expect("mutex lock failure");
    if let Some(rec) = set.get(s) {
        rec
    } else {
        let leaked: &'static str = Box::leak(s.to_string().into_boxed_str());
        set.insert(leaked);
        leaked
    }
}

pub fn intern_static(s: &'static str) {
    let mut set = STRINGS.lock().expect("mutex lock failure");
    set.insert(s);
}

#[cfg(test)]
#[test]
fn test_internalization() {
    let s1 = intern("test");
    let s2 = intern(&"test".to_string());
    let st = "other";
    intern_static(st);
    let s3 = intern("other");
    assert!(std::ptr::eq(s1, s2));
    assert!(std::ptr::eq(s3, st));
    assert!(!std::ptr::eq(s1, s3));
}
