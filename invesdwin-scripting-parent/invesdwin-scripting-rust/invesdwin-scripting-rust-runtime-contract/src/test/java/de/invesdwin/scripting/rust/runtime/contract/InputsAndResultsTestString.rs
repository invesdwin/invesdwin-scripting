println!("getString");
let getString = putString;
println!("{:?}", getString);

println!("getStringWithNull");
let getStringWithNull = putStringWithNull;
println!("{:?}", getStringWithNull);

println!("getStringVector");
let getStringVector = putStringVector;
println!("{:?}", getStringVector);

println!("getStringVectorWithNull");
let getStringVectorWithNull = putStringVectorWithNull;
println!("{:?}", getStringVectorWithNull);
if(!getStringVectorWithNull[1].is_empty()) {
	panic!("getStringVectorWithNull[2] not empty!");
};

println!("getStringVectorAsList");
let getStringVectorAsList = putStringVectorAsList;
println!("{:?}", getStringVectorAsList);

println!("getStringVectorAsListWithNull");
let getStringVectorAsListWithNull = putStringVectorAsListWithNull;
println!("{:?}", getStringVectorAsListWithNull);
if(!getStringVectorAsListWithNull[1].is_empty()) {
	panic!("getStringVectorAsListWithNull[1] not empty!");
};

println!("getStringMatrix");
let getStringMatrix = putStringMatrix;
println!("{:?}", getStringMatrix);

println!("getStringMatrixWithNull");
let getStringMatrixWithNull = putStringMatrixWithNull;
println!("{:?}", getStringMatrixWithNull);
if(!getStringMatrixWithNull[0][0].is_empty()) {
	panic!("getStringMatrixWithNull[0][0] not empty!");
};
if(!getStringMatrixWithNull[1][1].is_empty()) {
	panic!("getStringMatrixWithNull[1][1] not empty!");
};
if(!getStringMatrixWithNull[2][2].is_empty()) {
	panic!("getStringMatrixWithNull[2][2] not empty!");
};

println!("getStringMatrixAsList");
let getStringMatrixAsList = putStringMatrixAsList;
println!("{:?}", getStringMatrixAsList);

println!("getStringMatrixAsListWithNull");
let getStringMatrixAsListWithNull = putStringMatrixAsListWithNull;
println!("{:?}", getStringMatrixAsListWithNull);
if(!getStringMatrixAsListWithNull[0][0].is_empty()) {
	panic!("getStringMatrixAsListWithNull[0][0] not empty!");
};
if(!getStringMatrixAsListWithNull[1][1].is_empty()) {
	panic!("getStringMatrixAsListWithNull[1][1] not empty!");
};
if(!getStringMatrixAsListWithNull[2][2].is_empty()) {
	panic!("getStringMatrixAsListWithNull[2][2] not empty!");
};
