:{
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::thread;
use std::time::Duration;
use rhai::{Engine, Scope, Dynamic, Array};
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
	
	println!("response: {:?}", response);
	
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
fn is_first_element_char(d: &Dynamic) -> bool {
    // 1. Try to cast current Dynamic to an Array
    if let Some(arr) = d.read_lock::<Array>() {
        // 2. Check if array is empty
        if let Some(first) = arr.first() {
            // 3. Recurse: if the first element is another array, keep going
            return is_first_element_char(first);
        }
        return false; // Empty array has no first element
    }

    // 4. Base Case: It's not an array, so check if it's a char
    d.is_char()
}

pub fn callback<T: serde::de::DeserializeOwned>(method_name: &str, parameters: &[serde_json::Value]) -> T {
    let dynamic = callback_dynamic(method_name, parameters);
    
	println!("dynamic: {:?}", dynamic);
	
	println!("dynamic type: {:?}", dynamic.type_name());
	
    // Handle unit type () specially for void methods
    if dynamic.is_unit() {
        // For unit type, just return () without parsing
        unsafe { std::mem::transmute_copy(&()) }
    } else if dynamic.is_string() {
        // For string type, convert directly to String using JSON for safety
        let string_val = dynamic.into_string().unwrap();
        serde_json::from_str(&format!("\"{}\"", string_val)).unwrap()
	} else if dynamic.is_char() {
        // For char type, return directly
		let char_val = dynamic.as_char().unwrap();
		unsafe { std::mem::transmute_copy(&char_val) }
    } else if dynamic.is_array() {
        // For array types, need to handle character arrays specially since JSON doesn't support chars
        // Check if the first element is a character to determine if this is a character array/matrix
        if is_first_element_char(&dynamic) {
            // This is a character array or matrix, convert by replacing ' with "
            let dynamic_str = dynamic.to_string();
            let json_str = dynamic_str.replace('\'', "\"");
            serde_json::from_str(&json_str).unwrap()
        } else {
            // For other arrays, parse normally
            let dynamic_str = dynamic.to_string();
            serde_json::from_str(&dynamic_str).unwrap()
        }
    } else {
        // For other types, parse from JSON
        serde_json::from_str(&dynamic.to_string()).unwrap()
    }
}
:}
