package de.invesdwin.scripting.haskell.runtime.frege.callback.file;

import java.io.IOException;
import java.nio.charset.Charset;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.concurrent.loop.ASpinWait;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.time.date.FTimeUnit;

@NotThreadSafe
public class FileScriptTaskCallbackServerHandler implements Runnable {

    private final FileScriptTaskCallbackContext callbackContext;
    private final ASpinWait requestSpinWait;

    public FileScriptTaskCallbackServerHandler(final FileScriptTaskCallbackContext callbackContext) {
        this.callbackContext = callbackContext;
        this.requestSpinWait = new ASpinWait() {

            @Override
            public boolean isConditionFulfilled() throws Exception {
                return callbackContext.getRequestFile().exists();
            }
        };
    }

    @Override
    public void run() {
        try {
            while (true) {
                requestSpinWait.awaitFulfill(System.nanoTime());
                final String request = readRequest();
                Files.deleteQuietly(callbackContext.getRequestFile());
                final String response = handle(request);
                Files.writeStringToFile(callbackContext.getResponsePartFile(), response, Charset.defaultCharset());
                callbackContext.getResponsePartFile().renameTo(callbackContext.getResponseFile());
            }
        } catch (final Throwable t) {
            if (!Throwables.isCausedByInterrupt(t)) {
                throw Throwables.propagate(t);
            }
        }
    }

    private String readRequest() throws InterruptedException {
        while (true) {
            try {
                return Files.readFileToString(callbackContext.getRequestFile(), Charset.defaultCharset());
            } catch (final IOException e) {
                //windows file lock might still be active
                FTimeUnit.MILLISECONDS.sleep(1);
                continue;
            }
        }
    }

    private String handle(final String input) throws IOException {
        return callbackContext.invoke(input);
    }

}
