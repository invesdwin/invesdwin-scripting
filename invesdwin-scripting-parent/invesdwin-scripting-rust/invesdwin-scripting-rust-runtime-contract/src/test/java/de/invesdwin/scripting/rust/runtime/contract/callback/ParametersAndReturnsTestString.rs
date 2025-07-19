println!("getString")
if 'getString' in locals():
	panic!("getString already defined!")
getString = callback("getString")
getStringType = type(getString)
println!(getStringType)
println!(getString)
if(getStringType is not unicode):
	panic!("getString not unicode!")
callback("setString", getString)

println!("getStringWithNull")
if 'getStringWithNull' in locals():
	panic!("getStringWithNull already defined!")
getStringWithNull = callback("getStringWithNull")
getStringWithNullType = type(getStringWithNull)
println!(getStringWithNullType)
println!(getStringWithNull)
if(getStringWithNull is not None):
	panic!("getStringWithNull not None!")
callback("setStringWithNull", getStringWithNull)

println!("getStringVector")
if 'getStringVector' in locals():
	panic!("getStringVector already defined!")
getStringVector = callback("getStringVector")
getStringVectorType = type(getStringVector[0])
println!(getStringVectorType)
println!(getStringVector)
if(getStringVectorType is not unicode):
	panic!("getStringVector not unicode!")
callback("setStringVector", getStringVector)

println!("getStringVectorWithNull")
if 'getStringVectorWithNull' in locals():
	panic!("getStringVectorWithNull already defined!")
getStringVectorWithNull = callback("getStringVectorWithNull")
getStringVectorWithNullType = type(getStringVectorWithNull[0])
println!(getStringVectorWithNullType)
println!(getStringVectorWithNull)
if(getStringVectorWithNullType is not unicode):
	panic!("getStringVectorWithNull not unicode!")
if(getStringVectorWithNull[1] is not None):
	panic!("getStringVectorWithNull[2] not None!")
callback("setStringVectorWithNull", getStringVectorWithNull)

println!("getStringVectorAsList")
if 'getStringVectorAsList' in locals():
	panic!("getStringVectorAsList already defined!")
getStringVectorAsList = callback("getStringVectorAsList")
getStringVectorAsListType = type(getStringVectorAsList[0])
println!(getStringVectorAsListType)
println!(getStringVectorAsList)
if(getStringVectorAsListType is not unicode):
	panic!("getStringVectorAsList not unicode!")
callback("setStringVectorAsList", getStringVectorAsList)

println!("getStringVectorAsListWithNull")
if 'getStringVectorAsListWithNull' in locals():
	panic!("getStringVectorAsListWithNull already defined!")
getStringVectorAsListWithNull = callback("getStringVectorAsListWithNull")
getStringVectorAsListWithNullType = type(getStringVectorAsListWithNull[0])
println!(getStringVectorAsListWithNullType)
println!(getStringVectorAsListWithNull)
if(getStringVectorAsListWithNullType is not unicode):
	panic!("getStringVectorAsListWithNull not unicode!")
if(getStringVectorAsListWithNull[1] is not None):
	panic!("getStringVectorAsListWithNull[1] not None!")
callback("setStringVectorAsListWithNull", getStringVectorAsListWithNull)

println!("getStringMatrix")
if 'getStringMatrix' in locals():
	panic!("getStringMatrix already defined!")
getStringMatrix = callback("getStringMatrix")
getStringMatrixType = type(getStringMatrix[0][0])
println!(getStringMatrixType)
println!(getStringMatrix)
if(getStringMatrixType is not unicode):
	panic!("getStringMatrix not unicode!")
callback("setStringMatrix", getStringMatrix)

println!("getStringMatrixWithNull")
if 'getStringMatrixWithNull' in locals():
	panic!("getStringMatrixWithNull already defined!")
getStringMatrixWithNull = callback("getStringMatrixWithNull")
getStringMatrixWithNullType = type(getStringMatrixWithNull[0][1])
println!(getStringMatrixWithNullType)
println!(getStringMatrixWithNull)
if(getStringMatrixWithNullType is not unicode):
	panic!("getStringMatrixWithNull not unicode!")
if(getStringMatrixWithNull[0][0] is not None):
	panic!("getStringMatrixWithNull[0][0] not None!")
if(getStringMatrixWithNull[1][1] is not None):
	panic!("getStringMatrixWithNull[1][1] not None!")
if(getStringMatrixWithNull[2][2] is not None):
	panic!("getStringMatrixWithNull[2][2] not None!")
callback("setStringMatrixWithNull", getStringMatrixWithNull)

println!("getStringMatrixAsList")
if 'getStringMatrixAsList' in locals():
	panic!("getStringMatrixAsList already defined!")
getStringMatrixAsList = callback("getStringMatrixAsList")
getStringMatrixAsListType = type(getStringMatrixAsList[0][0])
println!(getStringMatrixAsListType)
println!(getStringMatrixAsList)
if(getStringMatrixAsListType is not unicode):
	panic!("getStringMatrixAsList not unicode!")
callback("setStringMatrixAsList", getStringMatrixAsList)

println!("getStringMatrixAsListWithNull")
if 'getStringMatrixAsListWithNull' in locals():
	panic!("getStringMatrixAsListWithNull already defined!")
getStringMatrixAsListWithNull = callback("getStringMatrixAsListWithNull")
getStringMatrixAsListWithNullType = type(getStringMatrixAsListWithNull[0][1])
println!(getStringMatrixAsListWithNullType)
println!(getStringMatrixAsListWithNull)
if(getStringMatrixAsListWithNullType is not unicode):
	panic!("getStringMatrixAsListWithNull not unicode!")
if(getStringMatrixAsListWithNull[0][0] is not None):
	panic!("getStringMatrixAsListWithNull[0][0] not None!")
if(getStringMatrixAsListWithNull[1][1] is not None):
	panic!("getStringMatrixAsListWithNull[1][1] not None!")
if(getStringMatrixAsListWithNull[2][2] is not None):
	panic!("getStringMatrixAsListWithNull[2][2] not None!")
callback("setStringMatrixAsListWithNull", getStringMatrixAsListWithNull)
