putStrLn ( show ( "getLong" ) )
getLong :: Integer <- callback "getLong" []
putStrLn ( show ( getLong ) )
callback "setLong" [param getLong] :: IO ()

putStrLn ( show ( "getLongVector" ) )
getLongVector :: [Integer] <- callback "getLongVector" []
putStrLn ( show ( getLongVector ) )
callback "setLongVector" [param getLongVector] :: IO ()

putStrLn ( show ( "getLongVectorAsList" ) )
getLongVectorAsList :: [Integer] <- callback "getLongVectorAsList" []
putStrLn ( show ( getLongVectorAsList ) )
callback "setLongVectorAsList" [param getLongVectorAsList] :: IO ()

putStrLn ( show ( "getLongMatrix" ) )
getLongMatrix :: [[Integer]] <- callback "getLongMatrix" []
putStrLn ( show ( getLongMatrix ) )
callback "setLongMatrix" [param getLongMatrix] :: IO ()

putStrLn ( show ( "getLongMatrixAsList" ) )
getLongMatrixAsList :: [[Integer]] <- callback "getLongMatrixAsList" []
putStrLn ( show ( getLongMatrixAsList ) )
callback "setLongMatrixAsList" [param getLongMatrixAsList] :: IO ()
