println("getShort")
if isdefined(Main, :getShort) && !isnothing(getShort)
	error("getShort already defined!")
end
getShort = callback("getShort")
println(typeof(getShort))
println(getShort)
if typeof(getShort) != Int16
	error("getShort not Int16!")
end
callback("setShort",getShort)

println("getShortVector")
if isdefined(Main, :getShortVector) && !isnothing(getShortVector)
	error("getShortVector already defined!")
end
getShortVector = callback("getShortVector")
println(eltype(getShortVector))
println(getShortVector)
if eltype(getShortVector) != Int16
	error("getShortVector not Int16!")
end
callback("setShortVector",getShortVector)

println("getShortVectorAsList")
if isdefined(Main, :getShortVectorAsList) && !isnothing(getShortVectorAsList)
	error("getShortVectorAsList already defined!")
end
getShortVectorAsList = callback("getShortVectorAsList")
println(eltype(getShortVectorAsList))
println(getShortVectorAsList)
if eltype(getShortVectorAsList) != Int16
	error("getShortVectorAsList not Int16!")
end
callback("setShortVectorAsList",getShortVectorAsList)

println("getShortMatrix")
if isdefined(Main, :getShortMatrix) && !isnothing(getShortMatrix)
	error("getShortMatrix already defined!")
end
getShortMatrix = callback("getShortMatrix")
println(eltype(getShortMatrix))
println(getShortMatrix)
if eltype(getShortMatrix) != Int16
	error("getShortMatrix not Int16!")
end
callback("setShortMatrix",getShortMatrix)

println("getShortMatrixAsList")
if isdefined(Main, :getShortMatrixAsList) && !isnothing(getShortMatrixAsList)
	error("getShortMatrixAsList already defined!")
end
getShortMatrixAsList = callback("getShortMatrixAsList")
println(eltype(getShortMatrixAsList))
println(getShortMatrixAsList)
if eltype(getShortMatrixAsList) != Int16
	error("getShortMatrixAsList not Int16!")
end
callback("setShortMatrixAsList",getShortMatrixAsList)
