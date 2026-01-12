putStrLn ( show ( "getInteger" ) )
getInteger :: Int <- callback "getInteger" []
putStrLn ( show ( getInteger ) )
callback "setInteger" [param getInteger] :: IO ()

putStrLn ( show ( "getIntegerVector" ) )
getIntegerVector :: [Int] <- callback "getIntegerVector" []
putStrLn ( show ( getIntegerVector ) )
callback "setIntegerVector" [param getIntegerVector] :: IO ()

putStrLn ( show ( "getIntegerVectorAsList" ) )
getIntegerVectorAsList :: [Int] <- callback "getIntegerVectorAsList" []
putStrLn ( show ( getIntegerVectorAsList ) )
callback "setIntegerVectorAsList" [param getIntegerVectorAsList] :: IO ()

putStrLn ( show ( "getIntegerMatrix" ) )
getIntegerMatrix :: [[Int]] <- callback "getIntegerMatrix" []
putStrLn ( show ( getIntegerMatrix ) )
callback "setIntegerMatrix" [param getIntegerMatrix] :: IO ()

putStrLn ( show ( "getIntegerMatrixAsList" ) )
getIntegerMatrixAsList :: [[Int]] <- callback "getIntegerMatrixAsList" []
putStrLn ( show ( getIntegerMatrixAsList ) )
callback "setIntegerMatrixAsList" [param getIntegerMatrixAsList] :: IO ()
