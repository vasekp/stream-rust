use streamlang as stream;
use stream::session::*;
use std::cell::RefCell;

use wasm_bindgen::prelude::*;
use js_sys::{Object, Reflect};

thread_local!{
    static SESS: RefCell<Session> = RefCell::new(Session::new());
}

#[wasm_bindgen]
pub fn process(input: String) -> Result<JsValue, JsValue> {
    console_error_panic_hook::set_once();
    let obj = Object::new();
    match stream::parse(&input) {
        Ok(expr) => SESS.with(|sess| -> Result<(), JsValue> {
            match sess.borrow_mut().process(expr) {
                Ok(SessionUpdate::History(index, item)) => {
                    Reflect::set(&obj, &"type".into(), &"result".into())?;
                    Reflect::set(&obj, &"history".into(), &index.into())?;
                    Reflect::set(&obj, &"value".into(), &format!("{item:.80}").into())?;
                    Reflect::set(&obj, &"itemType".into(), &item.type_str().into())?;
                },
                Ok(SessionUpdate::Globals(list)) => {
                    Reflect::set(&obj, &"type".into(), &"globals".into())?;
                    Reflect::set(&obj, &"names".into(), &format!("{}", list.len()).into())?;
                },
                Err(err) => {
                    Reflect::set(&obj, &"type".into(), &"error".into())?;
                    Reflect::set(&obj, &"text".into(), &format!("{err}").into())?;
                }
            }
            Ok(())
        })?,
        Err(err) => {
            Reflect::set(&obj, &"type".into(), &"error".into())?;
            Reflect::set(&obj, &"text".into(), &format!("{err}").into())?;
        }
    }
    Ok(obj.into())
}

#[wasm_bindgen]
pub fn stop() {
    stream::base::stop::send_stop()
}

#[wasm_bindgen]
pub fn validate(input: String) -> bool {
    !input.starts_with(':')
}
