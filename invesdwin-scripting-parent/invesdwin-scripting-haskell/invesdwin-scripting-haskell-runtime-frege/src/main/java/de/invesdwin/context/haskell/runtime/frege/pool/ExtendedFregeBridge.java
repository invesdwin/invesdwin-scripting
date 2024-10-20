package de.invesdwin.context.haskell.runtime.frege.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class ExtendedFregeBridge extends ModifiedFregeBridge {

    public ExtendedFregeBridge() {
        super();
    }

    public void reset() throws IOException {
        getErrWatcher().clearLog();
        eval(":reset");
        eval(ModifiedFregeBridge.STARTUP_SCRIPT);
        getErrWatcher().clearLog();
    }

}
