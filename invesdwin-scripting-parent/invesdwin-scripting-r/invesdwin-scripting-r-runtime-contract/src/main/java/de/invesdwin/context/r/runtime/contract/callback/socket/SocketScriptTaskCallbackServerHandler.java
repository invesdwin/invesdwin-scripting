package de.invesdwin.context.r.runtime.contract.callback.socket;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.integration.channel.async.IAsynchronousHandler;
import de.invesdwin.context.integration.channel.async.IAsynchronousHandlerContext;
import de.invesdwin.util.concurrent.Executors;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class SocketScriptTaskCallbackServerHandler implements IAsynchronousHandler<String, String> {

    private static final WrappedExecutorService EXECUTOR = Executors
            .newCachedThreadPool(SocketScriptTaskCallbackServerHandler.class.getSimpleName());
    private SocketScriptTaskCallbackContext callbackContext;

    @Override
    public String open(final IAsynchronousHandlerContext<String> context) throws IOException {
        return null;
    }

    @Override
    public String idle(final IAsynchronousHandlerContext<String> context) throws IOException {
        return null;
    }

    @Override
    public String handle(final IAsynchronousHandlerContext<String> context, final String input) throws IOException {
        if (callbackContext == null) {
            callbackContext = SocketScriptTaskCallbackContext.getContext(input);
            if (callbackContext == null) {
                throw new IllegalArgumentException(
                        SocketScriptTaskCallbackContext.class.getSimpleName() + " not found for uuid: " + input);
            }
            return null;
        }
        final int methodNameEndIndex = Strings.indexOf(input, ";");
        if (methodNameEndIndex <= 0) {
            throw new IllegalArgumentException("requiring request format [<methodName>;<dims>;<args>]: " + input);
        }
        final String methodName = input.substring(0, methodNameEndIndex);
        final String dimsAndArgs = input.substring(methodNameEndIndex + 1, input.length());
        final int dimsEndIndex = Strings.indexOf(dimsAndArgs, ";");
        if (dimsEndIndex <= 0) {
            throw new IllegalArgumentException("requiring request format [<methodName>;<dims>;<args>]: " + input);
        }
        final String dims = dimsAndArgs.substring(0, dimsEndIndex);
        final String args = dimsAndArgs.substring(dimsEndIndex + 1, dimsAndArgs.length());
        //use executor to prevent blocking requests in a shared netty handler thread
        EXECUTOR.execute(new Runnable() {
            @Override
            public void run() {
                final String result = callbackContext.invoke(methodName, dims, args);
                final String resultReplaced = Strings.replace(result, "\n", "__##R@NL@C##__");
                context.write(resultReplaced);
            }
        });
        return null;
    }

    @Override
    public void outputFinished(final IAsynchronousHandlerContext<String> context) throws IOException {}

    @Override
    public void close() throws IOException {}

}
