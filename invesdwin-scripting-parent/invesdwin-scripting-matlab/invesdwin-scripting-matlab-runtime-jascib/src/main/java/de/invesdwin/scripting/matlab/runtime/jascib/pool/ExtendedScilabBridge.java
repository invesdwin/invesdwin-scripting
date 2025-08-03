package de.invesdwin.scripting.matlab.runtime.jascib.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class ExtendedScilabBridge extends ModifiedScilabBridge {

    public ExtendedScilabBridge() {
        super();
    }

    @Override
    public void open() throws IOException {
        super.open();
    }

    public void reset() throws IOException {
        getErrWatcher().clearLog();
    }

}
