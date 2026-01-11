putStrLn ( show ( "getCharacter" ) )
getCharacter :: Char <- callback "getCharacter" []
putStrLn ( show ( getCharacter ) )
callback "setCharacter" [param getCharacter] :: IO ()

putStrLn ( show ( "getCharacterVector" ) )
getCharacterVector :: [Char] <- callback "getCharacterVector" []
putStrLn ( show ( getCharacterVector ) )
callback "setCharacterVector" [param getCharacterVector] :: IO ()

putStrLn ( show ( "getCharacterVectorAsList" ) )
getCharacterVectorAsList :: [Char] <- callback "getCharacterVectorAsList" []
putStrLn ( show ( getCharacterVectorAsList ) )
callback "setCharacterVectorAsList" [param getCharacterVectorAsList] :: IO ()

putStrLn ( show ( "getCharacterMatrix" ) )
getCharacterMatrix :: [[Char]] <- callback "getCharacterMatrix" []
putStrLn ( show ( getCharacterMatrix ) )
callback "setCharacterMatrix" [param getCharacterMatrix] :: IO ()

putStrLn ( show ( "getCharacterMatrixAsList" ) )
getCharacterMatrixAsList :: [[Char]] <- callback "getCharacterMatrixAsList" []
putStrLn ( show ( getCharacterMatrixAsList ) )
callback "setCharacterMatrixAsList" [param getCharacterMatrixAsList] :: IO ()
