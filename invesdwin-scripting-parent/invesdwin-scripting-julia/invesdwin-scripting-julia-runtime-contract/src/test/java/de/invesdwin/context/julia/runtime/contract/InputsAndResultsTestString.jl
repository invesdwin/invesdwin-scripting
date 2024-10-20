println("getString")
if isdefined(Main, :getString) && !isnothing(getString)
	error("getString already defined!")
end
getString = putString
println(typeof(getString))
println(getString)
if typeof(getString) != String
	error("getString not String!")
end

println("getStringWithNull")
if isdefined(Main, :getStringWithNull) && !isnothing(getStringWithNull)
	error("getStringWithNull already defined!")
end
getStringWithNull = putStringWithNull
println(typeof(getStringWithNull))
println(getStringWithNull)
if typeof(getStringWithNull) != Nothing
	error("getStringWithNull not Nothing!")
end
if !isnothing(getStringWithNull)
	error("getStringWithNull not nothing!")
end

println("getStringVector")
if isdefined(Main, :getStringVector) && !isnothing(getStringVector)
	error("getStringVector already defined!")
end
getStringVector = putStringVector
println(eltype(getStringVector))
println(getStringVector)
if eltype(getStringVector) != String
	error("getStringVector not String!")
end


println("getStringVectorWithNull")
if isdefined(Main, :getStringVectorWithNull) && !isnothing(getStringVectorWithNull)
	error("getStringVectorWithNull already defined!")
end
getStringVectorWithNull = putStringVectorWithNull
println(eltype(getStringVectorWithNull))
println(getStringVectorWithNull)
if eltype(getStringVectorWithNull) != String
	error("getStringVectorWithNull not String!")
end
if !isempty(getStringVectorWithNull[2])
	error("getStringVectorWithNull[2] not empty!")
end

println("getStringVectorAsList")
if isdefined(Main, :getStringVectorAsList) && !isnothing(getStringVectorAsList)
	error("getStringVectorAsList already defined!")
end
getStringVectorAsList = putStringVectorAsList
println(eltype(getStringVectorAsList))
println(getStringVectorAsList)
if eltype(getStringVectorAsList) != String
	error("getStringVectorAsList not String!")
end

println("getStringVectorAsListWithNull")
if isdefined(Main, :getStringVectorAsListWithNull) && !isnothing(getStringVectorAsListWithNull)
	error("getStringVectorAsListWithNull already defined!")
end
getStringVectorAsListWithNull = putStringVectorAsListWithNull
println(eltype(getStringVectorAsListWithNull))
println(getStringVectorAsListWithNull)
if eltype(getStringVectorAsListWithNull) != String
	error("getStringVectorAsListWithNull not String!")
end
if !isempty(getStringVectorAsListWithNull[2])
	error("getStringVectorAsListWithNull[2] not empty!")
end

println("getStringMatrix")
if isdefined(Main, :getStringMatrix) && !isnothing(getStringMatrix)
	error("getStringMatrix already defined!")
end
getStringMatrix = putStringMatrix
println(eltype(getStringMatrix))
println(getStringMatrix)
if eltype(getStringMatrix) != String
	error("getStringMatrix not String!")
end


println("getStringMatrixWithNull")
if isdefined(Main, :getStringMatrixWithNull) && !isnothing(getStringMatrixWithNull)
	error("getStringMatrixWithNull already defined!")
end
getStringMatrixWithNull = putStringMatrixWithNull
println(eltype(getStringMatrixWithNull))
println(getStringMatrixWithNull)
if eltype(getStringMatrixWithNull) != String
	error("getStringMatrixWithNull not String!")
end
if !isempty(getStringMatrixWithNull[1,1])
	error("getStringMatrixWithNull[1,1] not empty!")
end
if !isempty(getStringMatrixWithNull[2,2])
	error("getStringMatrixWithNull[2,2] not empty!")
end
if !isempty(getStringMatrixWithNull[3,3])
	error("getStringMatrixWithNull[3,3] not empty!")
end

println("getStringMatrixAsList")
if isdefined(Main, :getStringMatrixAsList) && !isnothing(getStringMatrixAsList)
	error("getStringMatrixAsList already defined!")
end
getStringMatrixAsList = putStringMatrixAsList
println(eltype(getStringMatrixAsList))
println(getStringMatrixAsList)
if eltype(getStringMatrixAsList) != String
	error("getStringMatrixAsList not String!")
end

println("getStringMatrixAsListWithNull")
if isdefined(Main, :getStringMatrixAsListWithNull) && !isnothing(getStringMatrixAsListWithNull)
	error("getStringMatrixAsListWithNull already defined!")
end
getStringMatrixAsListWithNull = putStringMatrixAsListWithNull
println(eltype(getStringMatrixAsListWithNull))
println(getStringMatrixAsListWithNull)
if eltype(getStringMatrixAsListWithNull) != String
	error("getStringMatrixAsListWithNull not String!")
end
if !isempty(getStringMatrixAsListWithNull[1,1])
	error("getStringMatrixAsListWithNull[1,1] not empty!")
end
if !isempty(getStringMatrixAsListWithNull[2,2])
	error("getStringMatrixAsListWithNull[2,2] not empty!")
end
if !isempty(getStringMatrixAsListWithNull[3,3])
	error("getStringMatrixAsListWithNull[3,3] not empty!")
end
