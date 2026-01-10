{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- cabal update && cabal install aeson OR pacman -S haskell-aseon
import Data.Aeson
import qualified Data.Vector as V
import Data.Text (pack)
import qualified Data.ByteString.Lazy as BL
-- cabal update && cabal install hint OR pacman -S haskell-hint
import Language.Haskell.Interpreter
import System.Directory
import Control.Concurrent (threadDelay)
import Data.Dynamic
import Data.Typeable

param :: ToJSON a => a -> Dynamic
param x = toDyn (toJSON x)

dynamicToJSON :: Dynamic -> Value
dynamicToJSON d =
    case fromDynamic d of
        Just (v :: Value) -> v
        Nothing ->
            error ("Dynamic does not contain a JSON Value: " ++ show (dynTypeRep d))

writeRequest :: String -> [Dynamic] -> BL.ByteString
writeRequest method params =
    let jsonParams = map dynamicToJSON params
        jsonArray  = Array (V.fromList (String (pack method) : jsonParams))
    in encode jsonArray


waitForFile :: FilePath -> IO ()
waitForFile path = do
    exists <- doesFileExist path
    if exists
        then return ()
        else threadDelay 1 >> waitForFile path

evaluateResponse :: forall a. Typeable a => String -> IO a
evaluateResponse expr = do
    result <- runInterpreter $ do
        setImports ["Prelude"]
        interpret expr (as :: a)
    case result of
        Left err -> error ("Interpreter error: " ++ show err)
        Right val -> return val

callback :: Typeable a => String -> [Dynamic] -> IO a
callback method params = do
    let json = writeRequest method params
    BL.writeFile scriptTaskCallbackContextRequestPartFile json
    renameFile scriptTaskCallbackContextRequestPartFile scriptTaskCallbackContextRequestFile
    waitForFile scriptTaskCallbackContextResponseFile
    expr <- readFile scriptTaskCallbackContextResponseFile
    removeFile scriptTaskCallbackContextResponseFile
    evaluateResponse expr
  where
    scriptTaskCallbackContextRequestPartFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE}
    scriptTaskCallbackContextRequestFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE}
    scriptTaskCallbackContextResponseFile = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE}
