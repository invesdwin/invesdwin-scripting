println("getBoolean")
if isdefined(Main, :getBoolean) && !isnothing(getBoolean)
	error("getBoolean already defined!")
end
getBoolean = putBoolean
println(typeof(getBoolean))
println(getBoolean)
if typeof(getBoolean) != Bool
	error("getBoolean not Bool!")
end

println("getBooleanVector")
if isdefined(Main, :getBooleanVector) && !isnothing(getBooleanVector)
	error("getBooleanVector already defined!")
end
getBooleanVector = putBooleanVector
println(eltype(getBooleanVector))
println(getBooleanVector)
if eltype(getBooleanVector) != Bool 
	error("getBooleanVector not Bool!")
end

println("getBooleanVectorAsList")
if isdefined(Main, :getBooleanVectorAsList) && !isnothing(getBooleanVectorAsList)
	error("getBooleanVectorAsList already defined!")
end
getBooleanVectorAsList = putBooleanVectorAsList
println(eltype(getBooleanVectorAsList))
println(getBooleanVectorAsList)
if eltype(getBooleanVectorAsList) != Bool
	error("getBooleanVectorAsList not Bool!")
end

println("getBooleanMatrix")
if isdefined(Main, :getBooleanMatrix) && !isnothing(getBooleanMatrix)
	error("getBooleanMatrix already defined!")
end
getBooleanMatrix = putBooleanMatrix
println(eltype(getBooleanMatrix))
println(getBooleanMatrix)
if eltype(getBooleanMatrix) != Bool
	error("getBooleanMatrix not Bool!")
end

println("getBooleanMatrixAsList")
if isdefined(Main, :getBooleanMatrixAsList) && !isnothing(getBooleanMatrixAsList)
	error("getBooleanMatrixAsList already defined!")
end
getBooleanMatrixAsList = putBooleanMatrixAsList
println(eltype(getBooleanMatrixAsList))
println(getBooleanMatrixAsList)
if eltype(getBooleanMatrixAsList) != Bool
	error("getBooleanMatrixAsList not Bool!")
end
