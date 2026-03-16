use wasm_bindgen::prelude::*;
use streamlang as stream;
use stream::session::*;
use std::cell::RefCell;

thread_local!{
    static SESS: RefCell<Session> = RefCell::new(Session::new());
}

#[wasm_bindgen]
pub fn process(input: String) -> String {
    console_error_panic_hook::set_once();
    match stream::parse(&input) {
        Ok(expr) => SESS.with(|sess| match sess.borrow_mut().process(expr) {
            Ok(SessionUpdate::History(index, item)) => format!("%{index}: {item:.80}"),
            Ok(SessionUpdate::Globals(_list)) => "vars update".into(),
            Err(err) => format!("Err: {err}")
        }),
        Err(err) => format!("Err: {err}")
    }
}

#[wasm_bindgen]
pub fn stop() {
    stream::base::stop::send_stop()
}

#[wasm_bindgen]
pub fn validate(input: String) -> bool {
    !input.starts_with(':')
}
