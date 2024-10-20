System.out.println("getByte")
if(getByte != null)
	throw new Exception("getByte already defined!")
val getByte: java.lang.Byte = callback("getByte")
val getByteType = getByte.getClass()
System.out.println(getByteType)
System.out.println(getByte)
if(getByteType != classOf[java.lang.Byte])
	throw new Exception("getByte not Byte!")
callback("setByte",getByte)

System.out.println("getByteVector")
if(getByteVector != null)
	throw new Exception("getByteVector already defined!")
val getByteVector: Array[Byte] = callback("getByteVector")
val getByteVectorType = getByteVector(0).getClass()
System.out.println(getByteVectorType)
System.out.println(getByteVector)
if(getByteVectorType != classOf[Byte])
	throw new Exception("getByteVector not Byte!")
callback("setByteVector",getByteVector)

System.out.println("getByteVectorAsList")
if(getByteVectorAsList != null)
	throw new Exception("getByteVectorAsList already defined!")
val getByteVectorAsList: Array[Byte] = callback("getByteVectorAsList")
val getByteVectorAsListType = getByteVectorAsList(0).getClass()
System.out.println(getByteVectorAsListType)
System.out.println(getByteVectorAsList)
if(getByteVectorAsListType != classOf[Byte])
	throw new Exception("getByteVectorAsList not Byte!")
callback("setByteVectorAsList",getByteVectorAsList)

System.out.println("getByteMatrix")
if(getByteMatrix != null)
	throw new Exception("getByteMatrix already defined!")
val getByteMatrix: Array[Array[Byte]] = callback("getByteMatrix")
val getByteMatrixType = getByteMatrix(0)(0).getClass()
System.out.println(getByteMatrixType)
System.out.println(getByteMatrix)
if(getByteMatrixType != classOf[Byte])
	throw new Exception("getByteMatrix not Byte!")
callback("setByteMatrix",getByteMatrix)

System.out.println("getByteMatrixAsList")
if(getByteMatrixAsList != null)
	throw new Exception("getByteMatrixAsList already defined!")
val getByteMatrixAsList: Array[Array[Byte]] = callback("getByteMatrixAsList")
val getByteMatrixAsListType = getByteMatrixAsList(0)(0).getClass()
System.out.println(getByteMatrixAsListType)
System.out.println(getByteMatrixAsList)
if(getByteMatrixAsListType != classOf[Byte])
	throw new Exception("getByteMatrixAsList not Byte!")
callback("setByteMatrixAsList",getByteMatrixAsList)
