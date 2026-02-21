:{
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::thread;
use std::time::Duration;
use rhai::{Engine, Scope, Dynamic};
use serde_json;
:}

:{
pub fn param<T: serde::Serialize>(value: T) -> serde_json::Value {
    serde_json::to_value(value).unwrap()
}
:}

:{
pub fn callback_dynamic(method_name: &str, parameters: &[serde_json::Value]) -> Dynamic {
	let requestPartFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE};
	let requestFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE};
	let responseFile = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE};
	
	//write request
	let mut json_array = serde_json::json!([method_name]);
	if let Some(array) = json_array.as_array_mut() {
	    for param in parameters {
	        array.push(param.clone());
	    }
	}
	let request = json_array.to_string();
    fs::write(requestPartFile, &request).unwrap();
    fs::rename(requestPartFile, requestFile).unwrap();
	
	//wait for response
	while (!fs::metadata(responseFile).is_ok()) {
	    thread::sleep(Duration::from_millis(1));
	}
    let response = fs::read_to_string(responseFile).unwrap();
    fs::remove_file(responseFile).unwrap();
	
	//evaluate response
    let engine = Engine::new();
    let mut scope = Scope::new();
    engine.eval_with_scope::<Dynamic>(&mut scope, &response).unwrap()
}
:}

:{
pub fn callback_void(method_name: &str, parameters: &[serde_json::Value]) {
    callback_dynamic(method_name, parameters);
}
:}

:{
pub fn callback<T: serde::de::DeserializeOwned>(method_name: &str, parameters: &[serde_json::Value]) -> T {
    let dynamic = callback_dynamic(method_name, parameters);
    
    // Handle unit type () specially for void methods
    if dynamic.is_unit() {
        // For unit type, just return () without parsing
        unsafe { std::mem::transmute_copy(&()) }
    } else if dynamic.is_string() {
        // For string type, convert directly to String using JSON for safety
        let string_val = dynamic.into_string().unwrap();
        serde_json::from_str(&format!("\"{}\"", string_val)).unwrap()
    } else {
        // For other types, parse from JSON
        serde_json::from_str(&dynamic.to_string()).unwrap()
    }
}
:}
