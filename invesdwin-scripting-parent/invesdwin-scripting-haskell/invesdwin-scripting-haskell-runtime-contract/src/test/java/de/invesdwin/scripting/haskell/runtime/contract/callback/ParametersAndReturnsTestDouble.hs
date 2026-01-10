putStrLn ( show ( "getDouble" ) )
getDouble :: Double <- callback "getDouble" []
putStrLn ( show ( getDouble ) )
callback "setDouble" [param getDouble] :: IO ()

putStrLn ( show ( "getDoubleVector" ) )
getDoubleVector :: [Double] <- callback "getDoubleVector" []
putStrLn ( show ( getDoubleVector ) )
callback "setDoubleVector" [param getDoubleVector] :: IO ()

putStrLn ( show ( "getDoubleVectorAsList" ) )
getDoubleVectorAsList :: [Double] <- callback "getDoubleVectorAsList" []
putStrLn ( show ( getDoubleVectorAsList ) )
callback "setDoubleVectorAsList" [param getDoubleVectorAsList] :: IO ()

putStrLn ( show ( "getDoubleMatrix" ) )
getDoubleMatrix :: [[Double]] <- callback "getDoubleMatrix" []
putStrLn ( show ( getDoubleMatrix ) )
callback "setDoubleMatrix" [param getDoubleMatrix] :: IO ()

putStrLn ( show ( "getDoubleMatrixAsList" ) )
getDoubleMatrixAsList :: [[Double]] <- callback "getDoubleMatrixAsList" []
putStrLn ( show ( getDoubleMatrixAsList ) )
callback "setDoubleMatrixAsList" [param getDoubleMatrixAsList] :: IO ()
