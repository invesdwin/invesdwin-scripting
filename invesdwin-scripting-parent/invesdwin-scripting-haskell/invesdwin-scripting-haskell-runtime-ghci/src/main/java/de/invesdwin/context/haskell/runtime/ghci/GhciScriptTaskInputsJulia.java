package de.invesdwin.context.haskell.runtime.ghci;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.haskell.runtime.contract.AScriptTaskInputsHaskellToExpression;

@NotThreadSafe
public class GhciScriptTaskInputsJulia extends AScriptTaskInputsHaskellToExpression {

    private final GhciScriptTaskEngineJulia engine;

    public GhciScriptTaskInputsJulia(final GhciScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public GhciScriptTaskEngineJulia getEngine() {
        return engine;
    }

}
