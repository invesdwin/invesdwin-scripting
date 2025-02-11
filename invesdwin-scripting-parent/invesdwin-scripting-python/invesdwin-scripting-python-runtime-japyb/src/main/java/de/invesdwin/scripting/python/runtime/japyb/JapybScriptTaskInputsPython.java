package de.invesdwin.scripting.python.runtime.japyb;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.python.runtime.contract.AScriptTaskInputsPythonToExpression;

@NotThreadSafe
public class JapybScriptTaskInputsPython extends AScriptTaskInputsPythonToExpression {

    private final JapybScriptTaskEnginePython engine;

    public JapybScriptTaskInputsPython(final JapybScriptTaskEnginePython engine) {
        this.engine = engine;
    }

    @Override
    public JapybScriptTaskEnginePython getEngine() {
        return engine;
    }

}
