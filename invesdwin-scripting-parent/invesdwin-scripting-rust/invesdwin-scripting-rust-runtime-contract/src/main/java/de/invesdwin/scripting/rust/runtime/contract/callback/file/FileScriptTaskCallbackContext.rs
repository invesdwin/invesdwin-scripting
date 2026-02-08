use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::thread;
use std::time::Duration;
use rhai::{Engine, Scope, Dynamic};
use serde_json;

fn wait_for_file(file_path: &str) {
    loop {
        if fs::metadata(file_path).is_ok() {
            return;
        }
        thread::sleep(Duration::from_millis(1));
    }
}

fn write_request(method: &str, params: &[serde_json::Value]) -> String {
    let mut json_array = serde_json::json!([method]);
    if let Some(array) = json_array.as_array_mut() {
        for param in params {
            array.push(param.clone());
        }
    }
    json_array.to_string()
}

fn evaluate_response(expression: &str) -> Dynamic {
    let engine = Engine::new();
    let mut scope = Scope::new();
    engine.eval_with_scope::<Dynamic>(&mut scope, expression).unwrap()
}

pub fn param<T: serde::Serialize>(value: T) -> serde_json::Value {
    serde_json::to_value(value).unwrap()
}

pub fn callback(method_name: &str, parameters: &[serde_json::Value]) -> Dynamic {
	let requestPartFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE};
	let requestFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE};
	let responseFile = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE};
    let request = write_request(method_name, parameters);
    fs::write(requestPartFile, &request).unwrap();
    fs::rename(requestPartFile, requestFile).unwrap();
    wait_for_file(responseFile);
    let response = fs::read_to_string(responseFile).unwrap();
    fs::remove_file(responseFile).unwrap();
    evaluate_response(&response)
}