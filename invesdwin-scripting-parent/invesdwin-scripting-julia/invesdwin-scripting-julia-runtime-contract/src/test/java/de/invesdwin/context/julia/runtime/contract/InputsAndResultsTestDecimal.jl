println("getDecimal")
if isdefined(Main, :getDecimal) && !isnothing(getDecimal)
	error("getDecimal already defined!")
end
getDecimal = putDecimal
println(typeof(getDecimal))
println(getDecimal)
if typeof(getDecimal) != Float64
	error("getDecimal not Float64!")
end

println("getDecimalVector")
if isdefined(Main, :getDecimalVector) && !isnothing(getDecimalVector)
	error("getDecimalVector already defined!")
end
getDecimalVector = putDecimalVector
println(eltype(getDecimalVector))
println(getDecimalVector)
if eltype(getDecimalVector) != Float64
	error("getDecimalVector not Float64!")
end

println("getDecimalVectorAsList")
if isdefined(Main, :getDecimalVectorAsList) && !isnothing(getDecimalVectorAsList)
	error("getDecimalVectorAsList already defined!")
end
getDecimalVectorAsList = putDecimalVectorAsList
println(eltype(getDecimalVectorAsList))
println(getDecimalVectorAsList)
if eltype(getDecimalVectorAsList) != Float64
	error("getDecimalVectorAsList not Float64!")
end

println("getDecimalMatrix")
if isdefined(Main, :getDecimalMatrix) && !isnothing(getDecimalMatrix)
	error("getDecimalMatrix already defined!")
end
getDecimalMatrix = putDecimalMatrix
println(eltype(getDecimalMatrix))
println(getDecimalMatrix)
if eltype(getDecimalMatrix) != Float64
	error("getDecimalMatrix not Float64!")
end

println("getDecimalMatrixAsList")
if isdefined(Main, :getDecimalMatrixAsList) && !isnothing(getDecimalMatrixAsList)
	error("getDecimalMatrixAsList already defined!")
end
getDecimalMatrixAsList = putDecimalMatrixAsList
println(eltype(getDecimalMatrixAsList))
println(getDecimalMatrixAsList)
if eltype(getDecimalMatrixAsList) != Float64
	error("getDecimalMatrixAsList not Float64!")
end
