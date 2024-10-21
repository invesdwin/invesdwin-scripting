println("getCharacter")
if isdefined(Main, :getCharacter) && !isnothing(getCharacter)
	error("getCharacter already defined!")
end
getCharacter = putCharacter
println(typeof(getCharacter))
println(getCharacter)
if typeof(getCharacter) != Char
	error("getCharacter not Char!")
end

println("getCharacterVector")
if isdefined(Main, :getCharacterVector) && !isnothing(getCharacterVector)
	error("getCharacterVector already defined!")
end
getCharacterVector = putCharacterVector
println(eltype(getCharacterVector))
println(getCharacterVector)
if eltype(getCharacterVector) != Char
	error("getCharacterVector not Char!")
end

println("getCharacterVectorAsList")
if isdefined(Main, :getCharacterVectorAsList) && !isnothing(getCharacterVectorAsList)
	error("getCharacterVectorAsList already defined!")
end
getCharacterVectorAsList = putCharacterVectorAsList
println(eltype(getCharacterVectorAsList))
println(getCharacterVectorAsList)
if eltype(getCharacterVectorAsList) != Char
	error("getCharacterVectorAsList not Char!")
end

println("getCharacterMatrix")
if isdefined(Main, :getCharacterMatrix) && !isnothing(getCharacterMatrix)
	error("getCharacterMatrix already defined!")
end
getCharacterMatrix = putCharacterMatrix
println(eltype(getCharacterMatrix))
println(getCharacterMatrix)
if eltype(getCharacterMatrix) != Char
	error("getCharacterMatrix not Char!")
end

println("getCharacterMatrixAsList")
if isdefined(Main, :getCharacterMatrixAsList) && !isnothing(getCharacterMatrixAsList)
	error("getCharacterMatrixAsList already defined!")
end
getCharacterMatrixAsList = putCharacterMatrixAsList
println(eltype(getCharacterMatrixAsList))
println(getCharacterMatrixAsList)
if eltype(getCharacterMatrixAsList) != Char
	error("getCharacterMatrixAsList not Char!")
end