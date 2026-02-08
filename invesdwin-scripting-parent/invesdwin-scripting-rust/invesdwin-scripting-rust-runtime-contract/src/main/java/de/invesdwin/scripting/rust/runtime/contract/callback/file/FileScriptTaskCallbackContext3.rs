pub fn callback(method_name: &str, parameters: &[serde_json::Value]) -> Dynamic {
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
