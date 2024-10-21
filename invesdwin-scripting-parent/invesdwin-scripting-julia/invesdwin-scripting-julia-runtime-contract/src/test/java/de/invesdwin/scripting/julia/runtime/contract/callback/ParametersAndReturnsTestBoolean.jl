println("getBoolean")
if isdefined(Main, :getBoolean) && !isnothing(getBoolean)
	error("getBoolean already defined!")
end
getBoolean = callback("getBoolean")
println(typeof(getBoolean))
println(getBoolean)
if typeof(getBoolean) != Bool
	error("getBoolean not Bool!")
end
callback("setBoolean",getBoolean)

println("getBooleanVector")
if isdefined(Main, :getBooleanVector) && !isnothing(getBooleanVector)
	error("getBooleanVector already defined!")
end
getBooleanVector = callback("getBooleanVector")
println(eltype(getBooleanVector))
println(getBooleanVector)
if eltype(getBooleanVector) != Bool 
	error("getBooleanVector not Bool!")
end
callback("setBooleanVector",getBooleanVector)

println("getBooleanVectorAsList")
if isdefined(Main, :getBooleanVectorAsList) && !isnothing(getBooleanVectorAsList)
	error("getBooleanVectorAsList already defined!")
end
getBooleanVectorAsList = callback("getBooleanVectorAsList")
println(eltype(getBooleanVectorAsList))
println(getBooleanVectorAsList)
if eltype(getBooleanVectorAsList) != Bool
	error("getBooleanVectorAsList not Bool!")
end
callback("setBooleanVectorAsList",getBooleanVectorAsList)

println("getBooleanMatrix")
if isdefined(Main, :getBooleanMatrix) && !isnothing(getBooleanMatrix)
	error("getBooleanMatrix already defined!")
end
getBooleanMatrix = callback("getBooleanMatrix")
println(eltype(getBooleanMatrix))
println(getBooleanMatrix)
if eltype(getBooleanMatrix) != Bool
	error("getBooleanMatrix not Bool!")
end
callback("setBooleanMatrix",getBooleanMatrix)

println("getBooleanMatrixAsList")
if isdefined(Main, :getBooleanMatrixAsList) && !isnothing(getBooleanMatrixAsList)
	error("getBooleanMatrixAsList already defined!")
end
getBooleanMatrixAsList = callback("getBooleanMatrixAsList")
println(eltype(getBooleanMatrixAsList))
println(getBooleanMatrixAsList)
if eltype(getBooleanMatrixAsList) != Bool
	error("getBooleanMatrixAsList not Bool!")
end
callback("setBooleanMatrixAsList",getBooleanMatrixAsList)
