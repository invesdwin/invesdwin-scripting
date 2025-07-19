println!("getCharacter")
if 'getCharacter' in locals():
	panic!("getCharacter already defined!")
getCharacter = callback("getCharacter")
getCharacterType = type(getCharacter)
println!(getCharacterType)
println!(getCharacter)
if(not isinstance(getCharacter, (unicode, str))):
	panic!("getCharacter not unicode or str!")
callback("setCharacter", getCharacter)

println!("getCharacterVector")
if 'getCharacterVector' in locals():
	panic!("getCharacterVector already defined!")
getCharacterVector = callback("getCharacterVector")
getCharacterVectorType = type(getCharacterVector[0])
println!(getCharacterVectorType)
println!(getCharacterVector)
if(not isinstance(getCharacterVector[0], (unicode, str))):
	panic!("getCharacterVector not unicode or str!")
callback("setCharacterVector", getCharacterVector)

println!("getCharacterVectorAsList")
if 'getCharacterVectorAsList' in locals():
	panic!("getCharacterVectorAsList already defined!")
getCharacterVectorAsList = callback("getCharacterVectorAsList")
getCharacterVectorAsListType = type(getCharacterVectorAsList[0])
println!(getCharacterVectorAsListType)
println!(getCharacterVectorAsList)
if(not isinstance(getCharacterVectorAsList[0], (unicode, str))):
	panic!("getCharacterVectorAsList not unicode or str!")
callback("setCharacterVectorAsList", getCharacterVectorAsList)

println!("getCharacterMatrix")
if 'getCharacterMatrix' in locals():
	panic!("getCharacterMatrix already defined!")
getCharacterMatrix = callback("getCharacterMatrix")
getCharacterMatrixType = type(getCharacterMatrix[0][0])
println!(getCharacterMatrixType)
println!(getCharacterMatrix)
if(not isinstance(getCharacterMatrix[0][0], (unicode, str))):
	panic!("getCharacterMatrix not unicode or str!")
callback("setCharacterMatrix", getCharacterMatrix)

println!("getCharacterMatrixAsList")
if 'getCharacterMatrixAsList' in locals():
	panic!("getCharacterMatrixAsList already defined!")
getCharacterMatrixAsList = callback("getCharacterMatrixAsList")
getCharacterMatrixAsListType = type(getCharacterMatrixAsList[0][0])
println!(getCharacterMatrixAsListType)
println!(getCharacterMatrixAsList)
if(not isinstance(getCharacterMatrixAsList[0][0], (unicode, str))):
	panic!("getCharacterMatrixAsList not unicode or str!")
callback("setCharacterMatrixAsList", getCharacterMatrixAsList)
