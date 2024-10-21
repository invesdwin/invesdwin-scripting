println("getPercent")
if isdefined(Main, :getPercent) && !isnothing(getPercent)
	error("getPercent already defined!")
end
getPercent = putPercent
println(typeof(getPercent))
println(getPercent)
if typeof(getPercent) != Float64
	error("getPercent not Float64!")
end

println("getPercentVector")
if isdefined(Main, :getPercentVector) && !isnothing(getPercentVector)
	error("getPercentVector already defined!")
end
getPercentVector = putPercentVector
println(eltype(getPercentVector))
println(getPercentVector)
if eltype(getPercentVector) != Float64
	error("getPercentVector not Float64!")
end

println("getPercentVectorAsList")
if isdefined(Main, :getPercentVectorAsList) && !isnothing(getPercentVectorAsList)
	error("getPercentVectorAsList already defined!")
end
getPercentVectorAsList = putPercentVectorAsList
println(eltype(getPercentVectorAsList))
println(getPercentVectorAsList)
if eltype(getPercentVectorAsList) != Float64
	error("getPercentVectorAsList not Float64!")
end

println("getPercentMatrix")
if isdefined(Main, :getPercentMatrix) && !isnothing(getPercentMatrix)
	error("getPercentMatrix already defined!")
end
getPercentMatrix = putPercentMatrix
println(eltype(getPercentMatrix))
println(getPercentMatrix)
if eltype(getPercentMatrix) != Float64
	error("getPercentMatrix not Float64!")
end

println("getPercentMatrixAsList")
if isdefined(Main, :getPercentMatrixAsList) && !isnothing(getPercentMatrixAsList)
	error("getPercentMatrixAsList already defined!")
end
getPercentMatrixAsList = putPercentMatrixAsList
println(eltype(getPercentMatrixAsList))
println(getPercentMatrixAsList)
if eltype(getPercentMatrixAsList) != Float64
	error("getPercentMatrixAsList not Float64!")
end
