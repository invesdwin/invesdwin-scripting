putStrLn ( show ( "getByte" ) )
getByte :: Int <- callback "getByte" []
putStrLn ( show ( getByte ) )
callback "setByte" [param getByte] :: IO ()

putStrLn ( show ( "getByteVector" ) )
getByteVector :: [Int] <- callback "getByteVector" []
putStrLn ( show ( getByteVector ) )
callback "setByteVector" [param getByteVector] :: IO ()

putStrLn ( show ( "getByteVectorAsList" ) )
getByteVectorAsList :: [Int] <- callback "getByteVectorAsList" []
putStrLn ( show ( getByteVectorAsList ) )
callback "setByteVectorAsList" [param getByteVectorAsList] :: IO ()

putStrLn ( show ( "getByteMatrix" ) )
getByteMatrix :: [[Int]] <- callback "getByteMatrix" []
putStrLn ( show ( getByteMatrix ) )
callback "setByteMatrix" [param getByteMatrix] :: IO ()

putStrLn ( show ( "getByteMatrixAsList" ) )
getByteMatrixAsList :: [[Int]] <- callback "getByteMatrixAsList" []
putStrLn ( show ( getByteMatrixAsList ) )
callback "setByteMatrixAsList" [param getByteMatrixAsList] :: IO ()
