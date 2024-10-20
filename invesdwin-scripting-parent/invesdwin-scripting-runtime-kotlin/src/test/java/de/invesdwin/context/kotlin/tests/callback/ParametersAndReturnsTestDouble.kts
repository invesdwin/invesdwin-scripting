println("getDouble")
if(binding.containsKey("getDouble"))
	throw Exception("getDouble already defined!")
val getDouble: Double = callback("getDouble")
val getDoubleType = getDouble::class
println(getDoubleType)
println(getDouble)
if(getDoubleType != Double::class)
	throw Exception("getDouble not Double!")
callback<Unit>("setDouble",getDouble)

println("getDoubleVector")
if(binding.containsKey("getDoubleVector"))
	throw Exception("getDoubleVector already defined!")
val getDoubleVector: DoubleArray = callback("getDoubleVector")
val getDoubleVectorType = getDoubleVector[0]::class
println(getDoubleVectorType)
println(getDoubleVector)
if(getDoubleVectorType != Double::class)
	throw Exception("getDoubleVector not Double!")
callback<Unit>("setDoubleVector",getDoubleVector)

println("getDoubleVectorAsList")
if(binding.containsKey("getDoubleVectorAsList"))
	throw Exception("getDoubleVectorAsList already defined!")
val getDoubleVectorAsList: DoubleArray = callback("getDoubleVectorAsList")
val getDoubleVectorAsListType = getDoubleVectorAsList[0]::class
println(getDoubleVectorAsListType)
println(getDoubleVectorAsList)
if(getDoubleVectorAsListType != Double::class)
	throw Exception("getDoubleVectorAsList not Double!")
callback<Unit>("setDoubleVectorAsList",getDoubleVectorAsList)

println("getDoubleMatrix")
if(binding.containsKey("getDoubleMatrix"))
	throw Exception("getDoubleMatrix already defined!")
val getDoubleMatrix: Array<DoubleArray> = callback("getDoubleMatrix")
val getDoubleMatrixType = getDoubleMatrix[0][0]::class
println(getDoubleMatrixType)
println(getDoubleMatrix)
if(getDoubleMatrixType != Double::class)
	throw Exception("getDoubleMatrix not Double!")
callback<Unit>("setDoubleMatrix",getDoubleMatrix)

println("getDoubleMatrixAsList")
if(binding.containsKey("getDoubleMatrixAsList"))
	throw Exception("getDoubleMatrixAsList already defined!")
val getDoubleMatrixAsList: Array<DoubleArray> = callback("getDoubleMatrixAsList")
val getDoubleMatrixAsListType = getDoubleMatrixAsList[0][0]::class
println(getDoubleMatrixAsListType)
println(getDoubleMatrixAsList)
if(getDoubleMatrixAsListType != Double::class)
	throw Exception("getDoubleMatrixAsList not Double!")
callback<Unit>("setDoubleMatrixAsList",getDoubleMatrixAsList)
