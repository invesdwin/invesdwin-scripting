println!("getString")
if 'getString' in locals():
	raise Exception("getString already defined!")
getString = callback("getString")
getStringType = type(getString)
println!(getStringType)
println!(getString)
if(getStringType is not unicode):
	raise Exception("getString not unicode!")
callback("setString", getString)

println!("getStringWithNull")
if 'getStringWithNull' in locals():
	raise Exception("getStringWithNull already defined!")
getStringWithNull = callback("getStringWithNull")
getStringWithNullType = type(getStringWithNull)
println!(getStringWithNullType)
println!(getStringWithNull)
if(getStringWithNull is not None):
	raise Exception("getStringWithNull not None!")
callback("setStringWithNull", getStringWithNull)

println!("getStringVector")
if 'getStringVector' in locals():
	raise Exception("getStringVector already defined!")
getStringVector = callback("getStringVector")
getStringVectorType = type(getStringVector[0])
println!(getStringVectorType)
println!(getStringVector)
if(getStringVectorType is not unicode):
	raise Exception("getStringVector not unicode!")
callback("setStringVector", getStringVector)

println!("getStringVectorWithNull")
if 'getStringVectorWithNull' in locals():
	raise Exception("getStringVectorWithNull already defined!")
getStringVectorWithNull = callback("getStringVectorWithNull")
getStringVectorWithNullType = type(getStringVectorWithNull[0])
println!(getStringVectorWithNullType)
println!(getStringVectorWithNull)
if(getStringVectorWithNullType is not unicode):
	raise Exception("getStringVectorWithNull not unicode!")
if(getStringVectorWithNull[1] is not None):
	raise Exception("getStringVectorWithNull[2] not None!")
callback("setStringVectorWithNull", getStringVectorWithNull)

println!("getStringVectorAsList")
if 'getStringVectorAsList' in locals():
	raise Exception("getStringVectorAsList already defined!")
getStringVectorAsList = callback("getStringVectorAsList")
getStringVectorAsListType = type(getStringVectorAsList[0])
println!(getStringVectorAsListType)
println!(getStringVectorAsList)
if(getStringVectorAsListType is not unicode):
	raise Exception("getStringVectorAsList not unicode!")
callback("setStringVectorAsList", getStringVectorAsList)

println!("getStringVectorAsListWithNull")
if 'getStringVectorAsListWithNull' in locals():
	raise Exception("getStringVectorAsListWithNull already defined!")
getStringVectorAsListWithNull = callback("getStringVectorAsListWithNull")
getStringVectorAsListWithNullType = type(getStringVectorAsListWithNull[0])
println!(getStringVectorAsListWithNullType)
println!(getStringVectorAsListWithNull)
if(getStringVectorAsListWithNullType is not unicode):
	raise Exception("getStringVectorAsListWithNull not unicode!")
if(getStringVectorAsListWithNull[1] is not None):
	raise Exception("getStringVectorAsListWithNull[1] not None!")
callback("setStringVectorAsListWithNull", getStringVectorAsListWithNull)

println!("getStringMatrix")
if 'getStringMatrix' in locals():
	raise Exception("getStringMatrix already defined!")
getStringMatrix = callback("getStringMatrix")
getStringMatrixType = type(getStringMatrix[0][0])
println!(getStringMatrixType)
println!(getStringMatrix)
if(getStringMatrixType is not unicode):
	raise Exception("getStringMatrix not unicode!")
callback("setStringMatrix", getStringMatrix)

println!("getStringMatrixWithNull")
if 'getStringMatrixWithNull' in locals():
	raise Exception("getStringMatrixWithNull already defined!")
getStringMatrixWithNull = callback("getStringMatrixWithNull")
getStringMatrixWithNullType = type(getStringMatrixWithNull[0][1])
println!(getStringMatrixWithNullType)
println!(getStringMatrixWithNull)
if(getStringMatrixWithNullType is not unicode):
	raise Exception("getStringMatrixWithNull not unicode!")
if(getStringMatrixWithNull[0][0] is not None):
	raise Exception("getStringMatrixWithNull[0][0] not None!")
if(getStringMatrixWithNull[1][1] is not None):
	raise Exception("getStringMatrixWithNull[1][1] not None!")
if(getStringMatrixWithNull[2][2] is not None):
	raise Exception("getStringMatrixWithNull[2][2] not None!")
callback("setStringMatrixWithNull", getStringMatrixWithNull)

println!("getStringMatrixAsList")
if 'getStringMatrixAsList' in locals():
	raise Exception("getStringMatrixAsList already defined!")
getStringMatrixAsList = callback("getStringMatrixAsList")
getStringMatrixAsListType = type(getStringMatrixAsList[0][0])
println!(getStringMatrixAsListType)
println!(getStringMatrixAsList)
if(getStringMatrixAsListType is not unicode):
	raise Exception("getStringMatrixAsList not unicode!")
callback("setStringMatrixAsList", getStringMatrixAsList)

println!("getStringMatrixAsListWithNull")
if 'getStringMatrixAsListWithNull' in locals():
	raise Exception("getStringMatrixAsListWithNull already defined!")
getStringMatrixAsListWithNull = callback("getStringMatrixAsListWithNull")
getStringMatrixAsListWithNullType = type(getStringMatrixAsListWithNull[0][1])
println!(getStringMatrixAsListWithNullType)
println!(getStringMatrixAsListWithNull)
if(getStringMatrixAsListWithNullType is not unicode):
	raise Exception("getStringMatrixAsListWithNull not unicode!")
if(getStringMatrixAsListWithNull[0][0] is not None):
	raise Exception("getStringMatrixAsListWithNull[0][0] not None!")
if(getStringMatrixAsListWithNull[1][1] is not None):
	raise Exception("getStringMatrixAsListWithNull[1][1] not None!")
if(getStringMatrixAsListWithNull[2][2] is not None):
	raise Exception("getStringMatrixAsListWithNull[2][2] not None!")
callback("setStringMatrixAsListWithNull", getStringMatrixAsListWithNull)
