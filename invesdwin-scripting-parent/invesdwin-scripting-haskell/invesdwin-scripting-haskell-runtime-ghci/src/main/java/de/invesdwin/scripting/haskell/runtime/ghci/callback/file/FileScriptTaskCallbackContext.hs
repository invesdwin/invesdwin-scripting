import Data.Aeson
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
-- cabal update && cabal install aeson OR pacman -S haskell-aseon
-- cabal update && cabal install hint OR pacman -S haskell-hint
import Language.Haskell.Interpreter

:{
readFileSafely :: String -> IO (Maybe String)
readFileSafely file = catch (Just <$> readFile file) handleError
  where
    handleError :: SomeException -> IO (Maybe String -> String)
    handleError e = do
        threadDelay 1000 -- 1 ms sleep
        return readFileSafely file
:}

:{
callback :: (Show args) => [args] -> result
callback args = do
    let message = show ( toJSON ( args ) )
    writeFile scriptTaskCallbackContextRequestPartFile message
    renameFile scriptTaskCallbackContextRequestPartFile scriptTaskCallbackContextRequestFile
    let returnExpression = readFileSafely scriptTaskCallbackContextResponseFile
    runInterpreter $ setImports ["Prelude"] >> eval returnExpression
where 
	scriptTaskCallbackContextRequestPartFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE}
    scriptTaskCallbackContextRequestFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE}
    scriptTaskCallbackContextResponseFile = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE}
:}