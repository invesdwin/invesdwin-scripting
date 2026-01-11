putStrLn ( show ( "getDecimal" ) )
getDecimal :: Double <- callback "getDecimal" []
putStrLn ( show ( getDecimal ) )
callback "setDecimal" [param getDecimal] :: IO ()

putStrLn ( show ( "getDecimalVector" ) )
getDecimalVector :: [Double] <- callback "getDecimalVector" []
putStrLn ( show ( getDecimalVector ) )
callback "setDecimalVector" [param getDecimalVector] :: IO ()

putStrLn ( show ( "getDecimalVectorAsList" ) )
getDecimalVectorAsList :: [Double] <- callback "getDecimalVectorAsList" []
putStrLn ( show ( getDecimalVectorAsList ) )
callback "setDecimalVectorAsList" [param getDecimalVectorAsList] :: IO ()

putStrLn ( show ( "getDecimalMatrix" ) )
getDecimalMatrix :: [[Double]] <- callback "getDecimalMatrix" []
putStrLn ( show ( getDecimalMatrix ) )
callback "setDecimalMatrix" [param getDecimalMatrix] :: IO ()

putStrLn ( show ( "getDecimalMatrixAsList" ) )
getDecimalMatrixAsList :: [[Double]] <- callback "getDecimalMatrixAsList" []
putStrLn ( show ( getDecimalMatrixAsList ) )
callback "setDecimalMatrixAsList" [param getDecimalMatrixAsList] :: IO ()
