package de.invesdwin.scripting.rust.runtime.evcxr.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class ExtendedEvcxrBridge extends ModifiedEvcxrBridge {

    public ExtendedEvcxrBridge() {
        super();
    }

    public void reset() throws IOException {
        getErrWatcher().clearLog();
        eval(":clear");
        eval(DEP_JSON);
        getErrWatcher().clearLog();
    }

}
