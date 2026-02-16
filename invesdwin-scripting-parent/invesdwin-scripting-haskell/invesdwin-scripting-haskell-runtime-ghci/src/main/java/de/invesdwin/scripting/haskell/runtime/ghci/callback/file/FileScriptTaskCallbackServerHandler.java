package de.invesdwin.scripting.haskell.runtime.ghci.callback.file;

import java.io.IOException;
import java.nio.charset.Charset;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.util.concurrent.loop.spinwait.ASpinWait;
import de.invesdwin.util.error.Throwables;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.time.date.FTimeUnit;

@NotThreadSafe
public class FileScriptTaskCallbackServerHandler implements Runnable {

    private final FileScriptTaskCallbackContext callbackContext;
    private final ASpinWait requestSpinWait;

    public FileScriptTaskCallbackServerHandler(final FileScriptTaskCallbackContext callbackContext) {
        this.callbackContext = callbackContext;
        this.requestSpinWait = new ASpinWait() {

            @Override
            protected boolean determineSpinAllowed() {
                return false;
            }

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
                final String request = Files.readFileToString(callbackContext.getRequestFile(),
                        Charset.defaultCharset());
                if (Strings.isBlank(request)) {
                    FTimeUnit.MILLISECONDS.sleep(1);
                    continue;
                }
                return request;
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
