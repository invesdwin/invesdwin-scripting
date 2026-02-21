println!("getString");
let getString: String = callback("getString", &[]);
println!("getString: {}", getString);
callback_void("setString", &[param(getString)]);

println!("getStringWithNull");
let getStringWithNull: Option<String> = callback("getStringWithNull", &[]);
println!("getStringWithNull: {:?}", getStringWithNull);
callback_void("setStringWithNull", &[param(getStringWithNull)]);

println!("getStringVector");
let getStringVector: Vec<String> = callback("getStringVector", &[]);
println!("getStringVector: {:?}", getStringVector);
callback_void("setStringVector", &[param(getStringVector)]);

println!("getStringVectorWithNull");
let getStringVectorWithNull: Vec<Option<String>> = callback("getStringVectorWithNull", &[]);
println!("getStringVectorWithNull: {:?}", getStringVectorWithNull);
if getStringVectorWithNull[1].is_some() {
    panic!("getStringVectorWithNull[1] not None!");
}
callback_void("setStringVectorWithNull", &[param(getStringVectorWithNull)]);

println!("getStringVectorAsList");
let getStringVectorAsList: Vec<String> = callback("getStringVectorAsList", &[]);
println!("getStringVectorAsList: {:?}", getStringVectorAsList);
callback_void("setStringVectorAsList", &[param(getStringVectorAsList)]);

println!("getStringVectorAsListWithNull");
let getStringVectorAsListWithNull: Vec<Option<String>> = callback("getStringVectorAsListWithNull", &[]);
println!("getStringVectorAsListWithNull: {:?}", getStringVectorAsListWithNull);
if getStringVectorAsListWithNull[1].is_some() {
    panic!("getStringVectorAsListWithNull[1] not None!");
}
callback_void("setStringVectorAsListWithNull", &[param(getStringVectorAsListWithNull)]);

println!("getStringMatrix");
let getStringMatrix: Vec<Vec<String>> = callback("getStringMatrix", &[]);
println!("getStringMatrix: {:?}", getStringMatrix);
callback_void("setStringMatrix", &[param(getStringMatrix)]);

println!("getStringMatrixWithNull");
let getStringMatrixWithNull: Vec<Vec<Option<String>>> = callback("getStringMatrixWithNull", &[]);
println!("getStringMatrixWithNull: {:?}", getStringMatrixWithNull);
if getStringMatrixWithNull[0][0].is_some() {
    panic!("getStringMatrixWithNull[0][0] not None!");
}
if getStringMatrixWithNull[1][1].is_some() {
    panic!("getStringMatrixWithNull[1][1] not None!");
}
if getStringMatrixWithNull[2][2].is_some() {
    panic!("getStringMatrixWithNull[2][2] not None!");
}
callback_void("setStringMatrixWithNull", &[param(getStringMatrixWithNull)]);

println!("getStringMatrixAsList");
let getStringMatrixAsList: Vec<Vec<String>> = callback("getStringMatrixAsList", &[]);
println!("getStringMatrixAsList: {:?}", getStringMatrixAsList);
callback_void("setStringMatrixAsList", &[param(getStringMatrixAsList)]);

println!("getStringMatrixAsListWithNull");
let getStringMatrixAsListWithNull: Vec<Vec<Option<String>>> = callback("getStringMatrixAsListWithNull", &[]);
println!("getStringMatrixAsListWithNull: {:?}", getStringMatrixAsListWithNull);
if getStringMatrixAsListWithNull[0][0].is_some() {
    panic!("getStringMatrixAsListWithNull[0][0] not None!");
}
if getStringMatrixAsListWithNull[1][1].is_some() {
    panic!("getStringMatrixAsListWithNull[1][1] not None!");
}
if getStringMatrixAsListWithNull[2][2].is_some() {
    panic!("getStringMatrixAsListWithNull[2][2] not None!");
}
callback_void("setStringMatrixAsListWithNull", &[param(getStringMatrixAsListWithNull)]);
