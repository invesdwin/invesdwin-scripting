putStrLn ( show ( "getBoolean" ) )
getBoolean :: Bool <- callback "getBoolean" []
putStrLn ( show ( getBoolean ) )
callback "setBoolean" [param getBoolean] :: IO ()

putStrLn ( show ( "getBooleanVector" ) )
getBooleanVector :: [Bool] <- callback "getBooleanVector" []
putStrLn ( show ( getBooleanVector ) )
callback "setBooleanVector" [param getBooleanVector] :: IO ()

putStrLn ( show ( "getBooleanVectorAsList" ) )
getBooleanVectorAsList :: [Bool] <- callback "getBooleanVectorAsList" []
putStrLn ( show ( getBooleanVectorAsList ) )
callback "setBooleanVectorAsList" [param getBooleanVectorAsList] :: IO ()

putStrLn ( show ( "getBooleanMatrix" ) )
getBooleanMatrix :: [[Bool]] <- callback "getBooleanMatrix" []
putStrLn ( show ( getBooleanMatrix ) )
callback "setBooleanMatrix" [param getBooleanMatrix] :: IO ()

putStrLn ( show ( "getBooleanMatrixAsList" ) )
getBooleanMatrixAsList :: [[Bool]] <- callback "getBooleanMatrixAsList" []
putStrLn ( show ( getBooleanMatrixAsList ) )
callback "setBooleanMatrixAsList" [param getBooleanMatrixAsList] :: IO ()
