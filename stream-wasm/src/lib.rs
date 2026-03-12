use wasm_bindgen::prelude::*;
use streamlang as stream;

#[wasm_bindgen]
pub fn process(input: String) -> String {
    console_error_panic_hook::set_once();
    match stream::parse(&input) {
        Ok(expr) => match expr.eval(&Default::default()) {
            Ok(item) => format!("{item:.80}"),
            Err(err) => format!("Err: {err}")
        },
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
