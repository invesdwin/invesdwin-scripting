-- cabal update && cabal install aeson OR pacman -S haskell-aseon
import Data.Aeson
-- cabal update && cabal install hint OR pacman -S haskell-hint
import Language.Haskell.Interpreter
import Control.Monad.Catch(catchAll)
import Control.Concurrent(threadDelay)
import System.Directory(renameFile)
import Data.Typeable (Typeable)

-- :{
-- https://stackoverflow.com/questions/57633136/how-to-write-a-retryforever-function-in-haskell-using-exception-handling
-- retryForever :: IO a -> IO a
-- retryForever prog = catchAll prog retry
--     where retry ex = do
--         threadDelay 1000
--         retryForever prog
-- :}

-- eval @Int "1 + 1"
:{
eval :: forall t. Typeable t => String -> IO (t)
eval s = do 
       x <- runInterpreter $ do
           setImports ["Prelude"]
           interpret s (as :: t)
       case x of
           Right y -> return y
:}

:{
callback :: (Show args, Typeable t) => [args] -> IO t
callback args = do
    let message = show ( toJSON ( args ) )
    writeFile scriptTaskCallbackContextRequestPartFile message
    renameFile scriptTaskCallbackContextRequestPartFile scriptTaskCallbackContextRequestFile
    returnExpression <- readFile scriptTaskCallbackContextResponseFile
    eval returnExpression
    where scriptTaskCallbackContextRequestPartFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE}
          scriptTaskCallbackContextRequestFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE}
          scriptTaskCallbackContextResponseFile = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE}
:}