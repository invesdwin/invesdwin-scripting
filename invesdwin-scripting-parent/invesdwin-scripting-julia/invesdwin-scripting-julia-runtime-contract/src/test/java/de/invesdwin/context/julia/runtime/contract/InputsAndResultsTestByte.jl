println("getByte")
if isdefined(Main, :getByte) && !isnothing(getByte)
	error("getByte already defined!")
end
getByte = putByte
println(typeof(getByte))
println(getByte)
if typeof(getByte) != Int8
	error("getByte not Int8!")
end

println("getByteVector")
if isdefined(Main, :getByteVector) && !isnothing(getByteVector)
	error("getByteVector already defined!")
end
getByteVector = putByteVector
println(eltype(getByteVector))
println(getByteVector)
if eltype(getByteVector) != Int8
	error("getByteVector not Int8!")
end

println("getByteVectorAsList")
if isdefined(Main, :getByteVectorAsList) && !isnothing(getByteVectorAsList)
	error("getByteVectorAsList already defined!")
end
getByteVectorAsList = putByteVectorAsList
println(eltype(getByteVectorAsList))
println(getByteVectorAsList)
if eltype(getByteVectorAsList) != Int8
	error("getByteVectorAsList not Int8!")
end

println("getByteMatrix")
if isdefined(Main, :getByteMatrix) && !isnothing(getByteMatrix)
	error("getByteMatrix already defined!")
end
getByteMatrix = putByteMatrix
println(eltype(getByteMatrix))
println(getByteMatrix)
if eltype(getByteMatrix) != Int8
	error("getByteMatrix not Int8!")
end

println("getByteMatrixAsList")
if isdefined(Main, :getByteMatrixAsList) && !isnothing(getByteMatrixAsList)
	error("getByteMatrixAsList already defined!")
end
getByteMatrixAsList = putByteMatrixAsList
println(eltype(getByteMatrixAsList))
println(getByteMatrixAsList)
if eltype(getByteMatrixAsList) != Int8
	error("getByteMatrixAsList not Int8!")
end
