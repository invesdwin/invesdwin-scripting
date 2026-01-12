putStrLn ( show ( "putUuid" ) )
putStrLn ( show ( putUuid ) )

getSecretStaticCallback :: String <- callback "getSecretStatic" [param putUuid]
putStrLn ( show ( "getSecretStaticCallback" ) )
putStrLn ( show ( getSecretStaticCallback ) )

getSecretCallback :: String <- callback "getSecret" [param putUuid]
putStrLn ( show ( "getSecretCallback" ) )
putStrLn ( show ( getSecretCallback ) )

getSecretExpressionCallback :: String <- callback "getSecretExpression" [param putUuid]
putStrLn ( show ( "getSecretExpressionCallback" ) )
putStrLn ( show ( getSecretExpressionCallback ) )

callback "voidMethod" [] :: IO ()