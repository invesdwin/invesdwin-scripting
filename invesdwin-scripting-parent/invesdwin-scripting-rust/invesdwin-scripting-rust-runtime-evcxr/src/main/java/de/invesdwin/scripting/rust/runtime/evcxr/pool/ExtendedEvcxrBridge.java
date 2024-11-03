package de.invesdwin.scripting.rust.runtime.evcxr.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.rust.runtime.evcxr.EvcxrScriptTaskEngineRust;

@NotThreadSafe
public class ExtendedEvcxrBridge extends ModifiedEvcxrBridge {

    private final EvcxrResetContext resetContext;

    public ExtendedEvcxrBridge() {
        super();
        this.resetContext = new EvcxrResetContext(new EvcxrScriptTaskEngineRust(this));
    }

    @Override
    public void open() throws IOException {
        super.open();
        resetContext.init();
    }

    public void reset() throws IOException {
        getErrWatcher().clearLog();
        resetContext.reset();
        getErrWatcher().clearLog();
    }

}
