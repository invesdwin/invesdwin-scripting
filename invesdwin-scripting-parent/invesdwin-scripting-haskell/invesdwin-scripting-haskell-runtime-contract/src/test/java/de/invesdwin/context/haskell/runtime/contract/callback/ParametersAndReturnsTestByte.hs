println("getByte")
if isdefined(Main, :getByte) && !isnothing(getByte)
	error("getByte already defined!")
end
getByte = callback("getByte")
println(typeof(getByte))
println(getByte)
if typeof(getByte) != Int8
	error("getByte not Int8!")
end
callback("setByte",getByte)

println("getByteVector")
if isdefined(Main, :getByteVector) && !isnothing(getByteVector)
	error("getByteVector already defined!")
end
getByteVector = callback("getByteVector")
println(eltype(getByteVector))
println(getByteVector)
if eltype(getByteVector) != Int8
	error("getByteVector not Int8!")
end
callback("setByteVector",getByteVector)

println("getByteVectorAsList")
if isdefined(Main, :getByteVectorAsList) && !isnothing(getByteVectorAsList)
	error("getByteVectorAsList already defined!")
end
getByteVectorAsList = callback("getByteVectorAsList")
println(eltype(getByteVectorAsList))
println(getByteVectorAsList)
if eltype(getByteVectorAsList) != Int8
	error("getByteVectorAsList not Int8!")
end
callback("setByteVectorAsList",getByteVectorAsList)

println("getByteMatrix")
if isdefined(Main, :getByteMatrix) && !isnothing(getByteMatrix)
	error("getByteMatrix already defined!")
end
getByteMatrix = callback("getByteMatrix")
println(eltype(getByteMatrix))
println(getByteMatrix)
if eltype(getByteMatrix) != Int8
	error("getByteMatrix not Int8!")
end
callback("setByteMatrix",getByteMatrix)

println("getByteMatrixAsList")
if isdefined(Main, :getByteMatrixAsList) && !isnothing(getByteMatrixAsList)
	error("getByteMatrixAsList already defined!")
end
getByteMatrixAsList = callback("getByteMatrixAsList")
println(eltype(getByteMatrixAsList))
println(getByteMatrixAsList)
if eltype(getByteMatrixAsList) != Int8
	error("getByteMatrixAsList not Int8!")
end
callback("setByteMatrixAsList",getByteMatrixAsList)
