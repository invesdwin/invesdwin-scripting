(println "putUuid")
(println putUuid)

(import de.invesdwin.scripting.runtime.clojure.tests.callback.SimpleCallbackTest)
(def getSecretStaticImport (. SimpleCallbackTest getSecretStatic putUuid))
(println "getSecretStaticImport")
(println getSecretStaticImport)

(def getSecretStaticCallback (callback "getSecretStatic" putUuid))
(println "getSecretStaticCallback")
(println getSecretStaticCallback)

(def getSecretCallback (callback "getSecret" putUuid))
(println "getSecretCallback")
(println getSecretCallback)

(def getSecretExpressionCallback (callback "getSecretExpression" putUuid))
(println "getSecretExpressionCallback")
(println getSecretExpressionCallback)

(callback "voidMethod")

(def callManyParams (callback "callManyParams" true 2 3 "4" 5 6 7.0 8.0 "123456789" 10.0))
(if-not (= callManyParams 55.0)
	(throw (Exception. (str "callManyParams unexpected result: " callManyParams)))
)
(def callManyParamsExpression (callback "callManyParamsExpression" true 2 3 "4" 5 6 7.0 8.0 "123456789" 10.0))
(if-not (= callManyParamsExpression 55.0)
	(throw (Exception. (str "callManyParamsExpression unexpected result: " callManyParamsExpression)))
)
(def callManyParamsExpressionMultiline (callback "callManyParamsExpressionMultiline" true 2 3 "4" 5 6 7.0 8.0 "123456789" 10.0))
(if-not (= callManyParamsExpressionMultiline 55.0)
	(throw (Exception. (str "callManyParamsExpressionMultiline unexpected result: " callManyParamsExpressionMultiline)))
)

(def getManyParamsExpression putManyParamsExpression)
(println "getManyParamsExpression")
(println getManyParamsExpression)
(def getManyParamsExpressionMultilineWrong putManyParamsExpressionMultilineWrong)
(println "getManyParamsExpressionMultilineWrong")
(println getManyParamsExpressionMultilineWrong)
(def getManyParamsExpressionMultiline putManyParamsExpressionMultiline)
(println "getManyParamsExpressionMultiline")
(println getManyParamsExpressionMultiline)