println("getDecimal")
if isdefined(Main, :getDecimal) && !isnothing(getDecimal)
	error("getDecimal already defined!")
end
getDecimal = callback("getDecimal")
println(typeof(getDecimal))
println(getDecimal)
if typeof(getDecimal) != Float64
	error("getDecimal not Float64!")
end
callback("setDecimal",getDecimal)

println("getDecimalVector")
if isdefined(Main, :getDecimalVector) && !isnothing(getDecimalVector)
	error("getDecimalVector already defined!")
end
getDecimalVector = callback("getDecimalVector")
println(eltype(getDecimalVector))
println(getDecimalVector)
if eltype(getDecimalVector) != Float64
	error("getDecimalVector not Float64!")
end
callback("setDecimalVector",getDecimalVector)

println("getDecimalVectorAsList")
if isdefined(Main, :getDecimalVectorAsList) && !isnothing(getDecimalVectorAsList)
	error("getDecimalVectorAsList already defined!")
end
getDecimalVectorAsList = callback("getDecimalVectorAsList")
println(eltype(getDecimalVectorAsList))
println(getDecimalVectorAsList)
if eltype(getDecimalVectorAsList) != Float64
	error("getDecimalVectorAsList not Float64!")
end
callback("setDecimalVectorAsList",getDecimalVectorAsList)

println("getDecimalMatrix")
if isdefined(Main, :getDecimalMatrix) && !isnothing(getDecimalMatrix)
	error("getDecimalMatrix already defined!")
end
getDecimalMatrix = callback("getDecimalMatrix")
println(eltype(getDecimalMatrix))
println(getDecimalMatrix)
if eltype(getDecimalMatrix) != Float64
	error("getDecimalMatrix not Float64!")
end
callback("setDecimalMatrix",getDecimalMatrix)

println("getDecimalMatrixAsList")
if isdefined(Main, :getDecimalMatrixAsList) && !isnothing(getDecimalMatrixAsList)
	error("getDecimalMatrixAsList already defined!")
end
getDecimalMatrixAsList = callback("getDecimalMatrixAsList")
println(eltype(getDecimalMatrixAsList))
println(getDecimalMatrixAsList)
if eltype(getDecimalMatrixAsList) != Float64
	error("getDecimalMatrixAsList not Float64!")
end
callback("setDecimalMatrixAsList",getDecimalMatrixAsList)
