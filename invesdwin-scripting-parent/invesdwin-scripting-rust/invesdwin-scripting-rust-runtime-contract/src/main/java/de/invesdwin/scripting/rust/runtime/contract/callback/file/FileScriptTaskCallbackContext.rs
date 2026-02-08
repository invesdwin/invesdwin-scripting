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

/// Custom error types for our callback system
#[derive(Debug)]
pub enum CallbackError {
    IoError(io::Error),
    RhaiError(rhai::EvalAltResult),
    JsonError(serde_json::Error),
    ExecutionError(String),
}

impl From<io::Error> for CallbackError {
    fn from(err: io::Error) -> Self {
        CallbackError::IoError(err)
    }
}

impl From<rhai::EvalAltResult> for CallbackError {
    fn from(err: rhai::EvalAltResult) -> Self {
        CallbackError::RhaiError(err)
    }
}

impl From<serde_json::Error> for CallbackError {
    fn from(err: serde_json::Error) -> Self {
        CallbackError::JsonError(err)
    }
}

impl std::fmt::Display for CallbackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallbackError::IoError(e) => write!(f, "IO Error: {}", e),
            CallbackError::RhaiError(e) => write!(f, "Rhai Error: {}", e),
            CallbackError::JsonError(e) => write!(f, "JSON Error: {}", e),
            CallbackError::ExecutionError(e) => write!(f, "Execution Error: {}", e),
        }
    }
}

impl std::error::Error for CallbackError {}

/// Wait for a file to exist (endless polling with 1ms intervals)
fn wait_for_file(file_path: &str) -> Result<(), CallbackError> {
    loop {
        if fs::metadata(file_path).is_ok() {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(1));
    }
}

/// Write a request in JSON format [method, param1, param2, ...] for Java
fn write_request(method: &str, params: &[serde_json::Value]) -> Result<String, CallbackError> {
    let mut json_array = serde_json::json!([method]);
    if let Some(array) = json_array.as_array_mut() {
        for param in params {
            array.push(param.clone());
        }
    }
    Ok(json_array.to_string())
}

/// Evaluate a response expression using Rhai and return the result as Dynamic
fn evaluate_response(expression: &str) -> Result<Dynamic, CallbackError> {
    let engine = Engine::new();
    let mut scope = Scope::new();
    
    // Evaluate the entire expression (Rhai handles comments and whitespace naturally)
    let result = engine.eval_with_scope::<Dynamic>(&mut scope, expression)?;
    Ok(result)
}

/// Main callback function - accepts any serializable parameters via varargs, returns Dynamic for casting
pub fn callback(method_name: &str, parameters: &[&dyn serde::Serialize]) -> Result<Dynamic, CallbackError> {
    // Convert parameters to JSON
    let json_params: Result<Vec<serde_json::Value>, _> = parameters
        .iter()
        .map(|&param| serde_json::to_value(param))
        .collect();
    
    // Write the request to a temporary file first (atomic operation)
    let request = write_request(method_name, &json_params?)?;
    fs::write(SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE, &request)?;
    
    // Rename to make the request visible (atomic operation)
    fs::rename(
        SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE,
        SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE
    )?;
    
    // Wait for the response file (endless polling)
    wait_for_file(SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE)?;
    
    // Read the response
    let response = fs::read_to_string(SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE)?;
    
    // Clean up the response file
    fs::remove_file(SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE)?;
    
    // Evaluate and return the response using Rhai
    evaluate_response(&response)
}

/// Macro for convenient varargs callback usage
#[macro_export]
macro_rules! callback {
    ($method:expr) => {
        $crate::callback($method, &[])
    };
    ($method:expr, $($param:expr),+ $(,)?) => {
        $crate::callback($method, &[$(&$param),+])
    };
}