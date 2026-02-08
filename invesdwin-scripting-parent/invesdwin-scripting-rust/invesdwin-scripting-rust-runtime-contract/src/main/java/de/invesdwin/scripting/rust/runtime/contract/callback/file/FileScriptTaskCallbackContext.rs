use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::thread;
use std::time::Duration;
use rhai::{Engine, Scope, Dynamic};
use serde_json;

// Constants for file-based communication (these will be replaced by the build system)
const SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE: &str = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE};
const SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE: &str = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE};
const SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE: &str = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE};

/// Wait for a file to exist (endless polling with 1ms intervals)
fn wait_for_file(file_path: &str) {
    loop {
        if fs::metadata(file_path).is_ok() {
            return;
        }
        thread::sleep(Duration::from_millis(1));
    }
}

/// Write a request in JSON format [method, param1, param2, ...] for Java
fn write_request(method: &str, params: &[serde_json::Value]) -> String {
    let mut json_array = serde_json::json!([method]);
    if let Some(array) = json_array.as_array_mut() {
        for param in params {
            array.push(param.clone());
        }
    }
    json_array.to_string()
}

/// Evaluate a response expression using Rhai and return the result as Dynamic
fn evaluate_response(expression: &str) -> Dynamic {
    let engine = Engine::new();
    let mut scope = Scope::new();
    
    // Evaluate the entire expression (Rhai handles comments and whitespace naturally)
    engine.eval_with_scope::<Dynamic>(&mut scope, expression)
        .unwrap_or_else(|e| panic!("Rhai evaluation error: {}", e))
}

/// Helper function to convert any serializable value to JSON Value
pub fn param<T: serde::Serialize>(value: T) -> serde_json::Value {
    serde_json::to_value(value).unwrap_or_else(|e| panic!("Failed to serialize parameter: {}", e))
}

/// Main callback function - accepts JSON values as parameters, returns Dynamic for casting
pub fn callback(method_name: &str, parameters: &[serde_json::Value]) -> Dynamic {
    // Write the request to a temporary file first (atomic operation)
    let request = write_request(method_name, parameters);
    fs::write(SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE, &request)
        .unwrap_or_else(|e| panic!("Failed to write request file: {}", e));
    
    // Rename to make the request visible (atomic operation)
    fs::rename(
        SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE,
        SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE
    ).unwrap_or_else(|e| panic!("Failed to rename request file: {}", e));
    
    // Wait for the response file (endless polling)
    wait_for_file(SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE);
    
    // Read the response
    let response = fs::read_to_string(SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE)
        .unwrap_or_else(|e| panic!("Failed to read response file: {}", e));
    
    // Clean up the response file
    fs::remove_file(SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE)
        .unwrap_or_else(|e| panic!("Failed to remove response file: {}", e));
    
    // Evaluate and return the response using Rhai
    evaluate_response(&response)
}