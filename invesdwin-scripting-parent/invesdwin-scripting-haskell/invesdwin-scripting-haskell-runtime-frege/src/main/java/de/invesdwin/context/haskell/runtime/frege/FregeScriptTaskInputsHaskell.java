package de.invesdwin.context.haskell.runtime.frege;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.haskell.runtime.contract.AScriptTaskInputsHaskellToExpression;

@NotThreadSafe
public class FregeScriptTaskInputsHaskell extends AScriptTaskInputsHaskellToExpression {

    private final FregeScriptTaskEngineHaskell engine;

    public FregeScriptTaskInputsHaskell(final FregeScriptTaskEngineHaskell engine) {
        this.engine = engine;
    }

    @Override
    public FregeScriptTaskEngineHaskell getEngine() {
        return engine;
    }

    @Override
    protected String booleanToString(final boolean value) {
        return String.valueOf(value);
    }

}
