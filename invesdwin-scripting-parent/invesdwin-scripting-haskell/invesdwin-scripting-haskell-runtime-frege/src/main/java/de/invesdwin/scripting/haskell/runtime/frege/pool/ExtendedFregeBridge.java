package de.invesdwin.scripting.haskell.runtime.frege.pool;

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
        eval(":l " + getStartupScript().getAbsolutePath());
        getErrWatcher().clearLog();
    }

}
