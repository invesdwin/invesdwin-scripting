println("getDouble")
if isdefined(Main, :getDouble) && !isnothing(getDouble)
	error("getDouble already defined!")
end
getDouble = callback("getDouble")
println(typeof(getDouble))
println(getDouble)
if typeof(getDouble) != Float64
	error("getDouble not Float64!")
end
callback("setDouble",getDouble)

println("getDoubleVector")
if isdefined(Main, :getDoubleVector) && !isnothing(getDoubleVector)
	error("getDoubleVector already defined!")
end
getDoubleVector = callback("getDoubleVector")
println(eltype(getDoubleVector))
println(getDoubleVector)
if eltype(getDoubleVector) != Float64
	error("getDoubleVector not Float64!")
end
callback("setDoubleVector",getDoubleVector)

println("getDoubleVectorAsList")
if isdefined(Main, :getDoubleVectorAsList) && !isnothing(getDoubleVectorAsList)
	error("getDoubleVectorAsList already defined!")
end
getDoubleVectorAsList = callback("getDoubleVectorAsList")
println(eltype(getDoubleVectorAsList))
println(getDoubleVectorAsList)
if eltype(getDoubleVectorAsList) != Float64
	error("getDoubleVectorAsList not Float64!")
end
callback("setDoubleVectorAsList",getDoubleVectorAsList)

println("getDoubleMatrix")
if isdefined(Main, :getDoubleMatrix) && !isnothing(getDoubleMatrix)
	error("getDoubleMatrix already defined!")
end
getDoubleMatrix = callback("getDoubleMatrix")
println(eltype(getDoubleMatrix))
println(getDoubleMatrix)
if eltype(getDoubleMatrix) != Float64
	error("getDoubleMatrix not Float64!")
end
callback("setDoubleMatrix",getDoubleMatrix)

println("getDoubleMatrixAsList")
if isdefined(Main, :getDoubleMatrixAsList) && !isnothing(getDoubleMatrixAsList)
	error("getDoubleMatrixAsList already defined!")
end
getDoubleMatrixAsList = callback("getDoubleMatrixAsList")
println(eltype(getDoubleMatrixAsList))
println(getDoubleMatrixAsList)
if eltype(getDoubleMatrixAsList) != Float64
	error("getDoubleMatrixAsList not Float64!")
end
callback("setDoubleMatrixAsList",getDoubleMatrixAsList)
