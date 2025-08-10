package de.invesdwin.scripting.rust.runtime.irust.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class ExtendedIrustBridge extends ModifiedIrustBridge {

    public ExtendedIrustBridge() {
        super();
    }

    public void reset() throws IOException {
        getErrWatcher().clearLog();
        eval(":reset");
        eval(ADD_JSON);
        getErrWatcher().clearLog();
    }

}
