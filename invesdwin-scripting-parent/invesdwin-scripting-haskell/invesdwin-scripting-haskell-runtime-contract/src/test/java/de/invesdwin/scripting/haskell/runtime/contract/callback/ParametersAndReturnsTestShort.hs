putStrLn ( show ( "getShort" ) )
getShort :: Int <- callback "getShort" []
putStrLn ( show ( getShort ) )
callback "setShort" [param getShort] :: IO ()

putStrLn ( show ( "getShortVector" ) )
getShortVector :: [Int] <- callback "getShortVector" []
putStrLn ( show ( getShortVector ) )
callback "setShortVector" [param getShortVector] :: IO ()

putStrLn ( show ( "getShortVectorAsList" ) )
getShortVectorAsList :: [Int] <- callback "getShortVectorAsList" []
putStrLn ( show ( getShortVectorAsList ) )
callback "setShortVectorAsList" [param getShortVectorAsList] :: IO ()

putStrLn ( show ( "getShortMatrix" ) )
getShortMatrix :: [[Int]] <- callback "getShortMatrix" []
putStrLn ( show ( getShortMatrix ) )
callback "setShortMatrix" [param getShortMatrix] :: IO ()

putStrLn ( show ( "getShortMatrixAsList" ) )
getShortMatrixAsList :: [[Int]] <- callback "getShortMatrixAsList" []
putStrLn ( show ( getShortMatrixAsList ) )
callback "setShortMatrixAsList" [param getShortMatrixAsList] :: IO ()
