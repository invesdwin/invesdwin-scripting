putStrLn ( show ( "getString" ) )
getString :: String <- callback "getString" []
putStrLn ( show ( getString ) )
callback "setString" [param getString] :: IO ()

putStrLn ( show ( "getStringWithNull" ) )
getStringWithNull :: Maybe String <- callback "getStringWithNull" []
putStrLn ( show ( getStringWithNull ) )
callback "setStringWithNull" [param getStringWithNull] :: IO ()

putStrLn ( show ( "getStringVector" ) )
getStringVector :: [String] <- callback "getStringVector" []
putStrLn ( show ( getStringVector ) )
callback "setStringVector" [param getStringVector] :: IO ()


putStrLn ( show ( "getStringVectorWithNull" ) )
getStringVectorWithNull :: [String] <- callback "getStringVectorWithNull" []
putStrLn ( show ( getStringVectorWithNull ) )
callback "setStringVectorWithNull" [param getStringVectorWithNull] :: IO ()

putStrLn ( show ( "getStringVectorAsList" ) )
getStringVectorAsList :: [String] <- callback "getStringVectorAsList" []
putStrLn ( show ( getStringVectorAsList ) )
callback "setStringVectorAsList" [param getStringVectorAsList] :: IO ()

putStrLn ( show ( "getStringVectorAsListWithNull" ) )
getStringVectorAsListWithNull :: [String] <- callback "getStringVectorAsListWithNull" []
putStrLn ( show ( getStringVectorAsListWithNull ) )
callback "setStringVectorAsListWithNull" [param getStringVectorAsListWithNull] :: IO ()

putStrLn ( show ( "getStringMatrix" ) )
getStringMatrix :: [[String]] <- callback "getStringMatrix" []
putStrLn ( show ( getStringMatrix ) )
callback "setStringMatrix" [param getStringMatrix] :: IO ()


putStrLn ( show ( "getStringMatrixWithNull" ) )
getStringMatrixWithNull :: [[String]] <- callback "getStringMatrixWithNull" []
putStrLn ( show ( getStringMatrixWithNull ) )
callback "setStringMatrixWithNull" [param getStringMatrixWithNull] :: IO ()

putStrLn ( show ( "getStringMatrixAsList" ) )
getStringMatrixAsList :: [[String]] <- callback "getStringMatrixAsList" []
putStrLn ( show ( getStringMatrixAsList ) )
callback "setStringMatrixAsList" [param getStringMatrixAsList] :: IO ()

putStrLn ( show ( "getStringMatrixAsListWithNull" ) )
getStringMatrixAsListWithNull :: [[String]] <- callback "getStringMatrixAsListWithNull" []
putStrLn ( show ( getStringMatrixAsListWithNull ) )
callback "setStringMatrixAsListWithNull" [param getStringMatrixAsListWithNull] :: IO ()
