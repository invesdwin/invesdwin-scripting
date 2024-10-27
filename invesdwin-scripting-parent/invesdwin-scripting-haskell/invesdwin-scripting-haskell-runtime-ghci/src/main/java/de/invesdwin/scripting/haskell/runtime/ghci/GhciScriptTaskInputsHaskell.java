package de.invesdwin.scripting.haskell.runtime.ghci;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.haskell.runtime.contract.AScriptTaskInputsHaskellToExpression;

@NotThreadSafe
public class GhciScriptTaskInputsHaskell extends AScriptTaskInputsHaskellToExpression {

    private final GhciScriptTaskEngineHaskell engine;

    public GhciScriptTaskInputsHaskell(final GhciScriptTaskEngineHaskell engine) {
        this.engine = engine;
    }

    @Override
    public GhciScriptTaskEngineHaskell getEngine() {
        return engine;
    }

}
