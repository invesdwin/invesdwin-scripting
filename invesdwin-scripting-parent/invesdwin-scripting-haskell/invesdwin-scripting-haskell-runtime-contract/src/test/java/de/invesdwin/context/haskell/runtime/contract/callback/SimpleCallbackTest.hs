println("putUuid")
println(putUuid)

getSecretStaticCallback = callback("getSecretStatic", putUuid)
println("getSecretStaticCallback")
println(getSecretStaticCallback)

getSecretCallback = callback("getSecret", putUuid)
println("getSecretCallback")
println(getSecretCallback)

getSecretExpressionCallback = callback("getSecretExpression", putUuid)
println("getSecretExpressionCallback")
println(getSecretExpressionCallback)

callback("voidMethod")