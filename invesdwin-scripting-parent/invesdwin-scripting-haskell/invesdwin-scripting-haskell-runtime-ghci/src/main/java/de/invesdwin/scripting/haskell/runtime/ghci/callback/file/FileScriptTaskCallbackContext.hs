import Data.JSON
import Language.Haskell.Interpreter

:{
callback :: (Show args) => [args] -> result
callback args = do
	let message = show ( toJSON ( args ) )
	writeFile scriptTaskCallbackContextRequestPartFile message
	renameFile scriptTaskCallbackContextRequestPartFile scriptTaskCallbackContextRequestFile
	-- TODO: retry if file does not exist yet
	let returnExpression = readFile scriptTaskCallbackContextResponseFile
	runInterpreter $ setImports ["Prelude"] >> eval returnExpression
where 	scriptTaskCallbackContextRequestPartFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_PART_FILE}
		scriptTaskCallbackContextRequestFile = {SCRIPT_TASK_CALLBACK_CONTEXT_REQUEST_FILE}
		scriptTaskCallbackContextResponseFile = {SCRIPT_TASK_CALLBACK_CONTEXT_RESPONSE_FILE}
:}