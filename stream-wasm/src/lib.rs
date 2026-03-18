use streamlang as stream;
use stream::session::*;
use stream::base::*;
use std::cell::RefCell;

use wasm_bindgen::prelude::*;
use js_sys::{Object, Reflect, Array};

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
                    Reflect::set(&obj, &"type".into(), &"item".into())?;
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
                    Reflect::set(&obj, &"trace".into(), &err.backtrace()
                        .iter().rev()
                        .map(|expr| JsValue::from(&expr.describe()))
                        .collect::<Array>()
                        .into())?;
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

#[wasm_bindgen]
pub struct BoxedIterator(Box<dyn SIterator<Item>>);

#[wasm_bindgen]
impl BoxedIterator {
    pub fn next(&mut self) -> Result<JsValue, JsValue> {
        let value = match self.0.next() {
            Ok(Some(item)) => {
                let obj = Object::new();
                Reflect::set(&obj, &"type".into(), &"item".into())?;
                Reflect::set(&obj, &"value".into(), &format!("{item:.80}").into())?;
                Reflect::set(&obj, &"itemType".into(), &item.type_str().into())?;
                obj
            },
            Ok(None) => {
                let obj = Object::new();
                Reflect::set(&obj, &"done".into(), &JsValue::TRUE)?;
                return Ok(obj.into());
            },
            Err(err) => {
                let obj = Object::new();
                Reflect::set(&obj, &"type".into(), &"error".into())?;
                Reflect::set(&obj, &"text".into(), &format!("{err}").into())?;
                Reflect::set(&obj, &"trace".into(), &err.backtrace()
                    .iter().rev()
                    .map(|expr| JsValue::from(&expr.describe()))
                    .collect::<Array>()
                    .into())?;
                obj
            },
        };
        let obj = Object::new();
        Reflect::set(&obj, &"value".into(), &value.into())?;
        Reflect::set(&obj, &"done".into(), &JsValue::FALSE)?;
        Ok(obj.into())
    }
}

#[wasm_bindgen]
pub fn get_iter(index: usize) -> Result<BoxedIterator, JsValue> {
    SESS.with(|sess| {
        let sess = sess.borrow();
        if index == 0 { return Err("invalid index".into()); }
        let Some(item) = sess.history().get(index - 1) else {
            return Err(format!("no such history index {index} len {}", sess.history().len()).into());
        };
        let Item::Stream(stm) = item else {
            return Err("not a stream".into());
        };
        Ok(BoxedIterator(stm.iter()))
    })
}
