println("getLong")
if isdefined(Main, :getLong) && !isnothing(getLong)
	error("getLong already defined!")
end
getLong = putLong
println(typeof(getLong))
println(getLong)
if typeof(getLong) != Int64
	error("getLong not Int64!")
end

println("getLongVector")
if isdefined(Main, :getLongVector) && !isnothing(getLongVector)
	error("getLongVector already defined!")
end
getLongVector = putLongVector
println(eltype(getLongVector))
println(getLongVector)
if eltype(getLongVector) != Int64
	error("getLongVector not Int64!")
end

println("getLongVectorAsList")
if isdefined(Main, :getLongVectorAsList) && !isnothing(getLongVectorAsList)
	error("getLongVectorAsList already defined!")
end
getLongVectorAsList = putLongVectorAsList
println(eltype(getLongVectorAsList))
println(getLongVectorAsList)
if eltype(getLongVectorAsList) != Int64
	error("getLongVectorAsList not Int64!")
end

println("getLongMatrix")
if isdefined(Main, :getLongMatrix) && !isnothing(getLongMatrix)
	error("getLongMatrix already defined!")
end
getLongMatrix = putLongMatrix
println(eltype(getLongMatrix))
println(getLongMatrix)
if eltype(getLongMatrix) != Int64
	error("getLongMatrix not Int64!")
end

println("getLongMatrixAsList")
if isdefined(Main, :getLongMatrixAsList) && !isnothing(getLongMatrixAsList)
	error("getLongMatrixAsList already defined!")
end
getLongMatrixAsList = putLongMatrixAsList
println(eltype(getLongMatrixAsList))
println(getLongMatrixAsList)
if eltype(getLongMatrixAsList) != Int64
	error("getLongMatrixAsList not Int64!")
end
