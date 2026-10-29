use crate::base::*;

use std::cell::RefCell;
use std::rc::Rc;

/// Receives evaluation events. Install an implementation with [`install()`].
pub trait Tracer {
    fn log(&mut self, ev: Event<'_>);
}

pub enum Event<'a> {
    Enter(&'a Node),
    Leave(&'a SResult<Item>),
}

impl Tracer for () {
    fn log(&mut self, _ev: Event<'_>) {}
}

thread_local! {
    static TRACER: RefCell<Option<Rc<RefCell<dyn Tracer>>>> = const { RefCell::new(None) };
}

/// Makes `tracer` receive the events of all evaluation on the current thread, including lazy
/// evaluation happening when streams are iterated, for as long as the returned guard lives.
/// Dropping the guard restores the tracer installed before, if any. Evaluation outside any such
/// scope is not traced.
///
/// The tracer is ambient state, like the interrupt flag ([`stop`](crate::base::stop)), not part of
/// an [`Env`]: it belongs to whoever drives the evaluation, not to the expressions being evaluated.
/// [`Session::process()`](crate::session::Session::process) installs the session's tracer for the
/// duration of each call.
pub fn install(tracer: Rc<RefCell<dyn Tracer>>) -> TracerGuard {
    let prev = TRACER.with(|slot| slot.replace(Some(tracer)));
    TracerGuard { prev }
}

/// Restores the previously installed tracer when dropped. See [`install()`].
#[must_use = "the tracer is uninstalled as soon as the guard is dropped"]
pub struct TracerGuard {
    prev: Option<Rc<RefCell<dyn Tracer>>>,
}

impl Drop for TracerGuard {
    fn drop(&mut self) {
        TRACER.with(|slot| *slot.borrow_mut() = self.prev.take());
    }
}

/// Sends `ev` to the installed tracer, if any.
pub(crate) fn log(ev: Event<'_>) {
    let tracer = TRACER.with(|slot| slot.borrow().clone());
    if let Some(tracer) = tracer {
        tracer.borrow_mut().log(ev);
    }
}
