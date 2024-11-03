package de.invesdwin.scripting.rust.runtime.rhai.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.rust.runtime.rhai.RhaiScriptTaskEngineRust;

@NotThreadSafe
public class ExtendedRhaiBridge extends ModifiedRhaiBridge {

    private final RhaiResetContext resetContext;

    public ExtendedRhaiBridge() {
        super();
        this.resetContext = new RhaiResetContext(new RhaiScriptTaskEngineRust(this));
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
