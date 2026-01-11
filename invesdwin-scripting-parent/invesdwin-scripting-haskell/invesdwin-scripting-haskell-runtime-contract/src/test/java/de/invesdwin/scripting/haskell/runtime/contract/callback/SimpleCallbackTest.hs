getSecretStaticCallback = callback "getSecretStatic" [putUuid] :: IO String

getSecretCallback = callback "getSecret" [putUuid] :: IO String

getSecretExpressionCallback = callback "getSecretExpression" [putUuid] :: IO String

callback "voidMethod" []:: IO ()
