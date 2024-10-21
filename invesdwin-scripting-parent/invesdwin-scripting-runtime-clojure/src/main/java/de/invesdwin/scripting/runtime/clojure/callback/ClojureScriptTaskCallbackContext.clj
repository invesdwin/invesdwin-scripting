(defn callback [^String methodName & parameters]
	(if-not (. binding containsKey "clojureScriptTaskCallbackContextUuid") 
        (throw (Exception. "IScriptTaskCallback not available"))
    )
    (let [^de.invesdwin.scripting.runtime.clojure.callback.ClojureScriptTaskCallbackContext context (. de.invesdwin.scripting.runtime.clojure.callback.ClojureScriptTaskCallbackContext (getContext clojureScriptTaskCallbackContextUuid))]
	    (. context invoke methodName (into-array Object parameters))
    )
)