package de.invesdwin.scripting.julia.runtime.jajub.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.julia.runtime.contract.JuliaResetContext;
import de.invesdwin.scripting.julia.runtime.jajub.JajubScriptTaskEngineJulia;

@NotThreadSafe
public class ExtendedJuliaBridge extends ModifiedJuliaBridge {

    private final JuliaResetContext resetContext;

    public ExtendedJuliaBridge() {
        super();
        this.resetContext = new JuliaResetContext(new JajubScriptTaskEngineJulia(this));
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
