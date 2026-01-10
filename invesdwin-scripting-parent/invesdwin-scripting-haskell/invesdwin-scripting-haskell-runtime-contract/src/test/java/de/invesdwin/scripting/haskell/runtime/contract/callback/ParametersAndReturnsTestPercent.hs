putStrLn ( show ( "getPercent" ) )
getPercent :: Double <- callback "getPercent" []
putStrLn ( show ( getPercent ) )
callback "setPercent" [param getPercent] :: IO ()

putStrLn ( show ( "getPercentVector" ) )
getPercentVector :: [Double] <- callback "getPercentVector" []
putStrLn ( show ( getPercentVector ) )
callback "setPercentVector" [param getPercentVector] :: IO ()

putStrLn ( show ( "getPercentVectorAsList" ) )
getPercentVectorAsList :: [Double] <- callback "getPercentVectorAsList" []
putStrLn ( show ( getPercentVectorAsList ) )
callback "setPercentVectorAsList" [param getPercentVectorAsList] :: IO ()

putStrLn ( show ( "getPercentMatrix" ) )
getPercentMatrix :: [[Double]] <- callback "getPercentMatrix" []
putStrLn ( show ( getPercentMatrix ) )
callback "setPercentMatrix" [param getPercentMatrix] :: IO ()

putStrLn ( show ( "getPercentMatrixAsList" ) )
getPercentMatrixAsList :: [[Double]] <- callback "getPercentMatrixAsList" []
putStrLn ( show ( getPercentMatrixAsList ) )
callback "setPercentMatrixAsList" [param getPercentMatrixAsList] :: IO ()
