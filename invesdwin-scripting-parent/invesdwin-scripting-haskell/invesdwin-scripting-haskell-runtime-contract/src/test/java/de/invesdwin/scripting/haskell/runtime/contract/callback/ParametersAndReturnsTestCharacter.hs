println("getCharacter")
if isdefined(Main, :getCharacter) && !isnothing(getCharacter)
	error("getCharacter already defined!")
end
getCharacter = callback("getCharacter")
println(typeof(getCharacter))
println(getCharacter)
if typeof(getCharacter) != Char
	error("getCharacter not Char!")
end
callback("setCharacter",getCharacter)

println("getCharacterVector")
if isdefined(Main, :getCharacterVector) && !isnothing(getCharacterVector)
	error("getCharacterVector already defined!")
end
getCharacterVector = callback("getCharacterVector")
println(eltype(getCharacterVector))
println(getCharacterVector)
if eltype(getCharacterVector) != Char
	error("getCharacterVector not Char!")
end
callback("setCharacterVector",getCharacterVector)

println("getCharacterVectorAsList")
if isdefined(Main, :getCharacterVectorAsList) && !isnothing(getCharacterVectorAsList)
	error("getCharacterVectorAsList already defined!")
end
getCharacterVectorAsList = callback("getCharacterVectorAsList")
println(eltype(getCharacterVectorAsList))
println(getCharacterVectorAsList)
if eltype(getCharacterVectorAsList) != Char
	error("getCharacterVectorAsList not Char!")
end
callback("setCharacterVectorAsList",getCharacterVectorAsList)

println("getCharacterMatrix")
if isdefined(Main, :getCharacterMatrix) && !isnothing(getCharacterMatrix)
	error("getCharacterMatrix already defined!")
end
getCharacterMatrix = callback("getCharacterMatrix")
println(eltype(getCharacterMatrix))
println(getCharacterMatrix)
if eltype(getCharacterMatrix) != Char
	error("getCharacterMatrix not Char!")
end
callback("setCharacterMatrix",getCharacterMatrix)

println("getCharacterMatrixAsList")
if isdefined(Main, :getCharacterMatrixAsList) && !isnothing(getCharacterMatrixAsList)
	error("getCharacterMatrixAsList already defined!")
end
getCharacterMatrixAsList = callback("getCharacterMatrixAsList")
println(eltype(getCharacterMatrixAsList))
println(getCharacterMatrixAsList)
if eltype(getCharacterMatrixAsList) != Char
	error("getCharacterMatrixAsList not Char!")
end
callback("setCharacterMatrixAsList",getCharacterMatrixAsList)
