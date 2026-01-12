putStrLn ( show ( "getFloat" ) )
getFloat :: Float <- callback "getFloat" []
putStrLn ( show ( getFloat ) )
callback "setFloat" [param getFloat] :: IO ()

putStrLn ( show ( "getFloatVector" ) )
getFloatVector :: [Float] <- callback "getFloatVector" []
putStrLn ( show ( getFloatVector ) )
callback "setFloatVector" [param getFloatVector] :: IO ()

putStrLn ( show ( "getFloatVectorAsList" ) )
getFloatVectorAsList :: [Float] <- callback "getFloatVectorAsList" []
putStrLn ( show ( getFloatVectorAsList ) )
callback "setFloatVectorAsList" [param getFloatVectorAsList] :: IO ()

putStrLn ( show ( "getFloatMatrix" ) )
getFloatMatrix :: [[Float]] <- callback "getFloatMatrix" []
putStrLn ( show ( getFloatMatrix ) )
callback "setFloatMatrix" [param getFloatMatrix] :: IO ()

putStrLn ( show ( "getFloatMatrixAsList" ) )
getFloatMatrixAsList :: [[Float]] <- callback "getFloatMatrixAsList" []
putStrLn ( show ( getFloatMatrixAsList ) )
callback "setFloatMatrixAsList" [param getFloatMatrixAsList] :: IO ()
