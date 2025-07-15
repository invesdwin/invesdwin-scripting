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
if(getStringVectorWithNull[1] is not None):
	raise Exception("getStringVectorWithNull[2] not None!");

println!("getStringVectorAsList");
let let getStringVectorAsList = putStringVectorAsList;
println!("{:?}", getStringVectorAsList);

println!("getStringVectorAsListWithNull");
getStringVectorAsListWithNull = putStringVectorAsListWithNull;
println!("{:?}", getStringVectorAsListWithNull);
if(getStringVectorAsListWithNull[1] is not None):
	raise Exception("getStringVectorAsListWithNull[1] not None!");

println!("getStringMatrix");
let getStringMatrix = putStringMatrix;
println!("{:?}", getStringMatrix);

println!("getStringMatrixWithNull");
let getStringMatrixWithNull = putStringMatrixWithNull;
println!("{:?}", getStringMatrixWithNull);
if(getStringMatrixWithNull[0][0] is not None):
	raise Exception("getStringMatrixWithNull[0][0] not None!");
if(getStringMatrixWithNull[1][1] is not None):
	raise Exception("getStringMatrixWithNull[1][1] not None!");
if(getStringMatrixWithNull[2][2] is not None):
	raise Exception("getStringMatrixWithNull[2][2] not None!");

println!("getStringMatrixAsList");
let getStringMatrixAsList = putStringMatrixAsList;
println!("{:?}", getStringMatrixAsList);

println!("getStringMatrixAsListWithNull");
let getStringMatrixAsListWithNull = putStringMatrixAsListWithNull;
println!("{:?}", getStringMatrixAsListWithNull);
if(getStringMatrixAsListWithNull[0][0] is not None):
	raise Exception("getStringMatrixAsListWithNull[0][0] not None!");
if(getStringMatrixAsListWithNull[1][1] is not None):
	raise Exception("getStringMatrixAsListWithNull[1][1] not None!");
if(getStringMatrixAsListWithNull[2][2] is not None):
	raise Exception("getStringMatrixAsListWithNull[2][2] not None!");
