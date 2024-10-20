package de.invesdwin.context.r.runtime.rserve.pool.internal;

import java.util.List;

import javax.annotation.concurrent.GuardedBy;
import javax.annotation.concurrent.ThreadSafe;

import org.math.R.RLog;
import org.math.R.Rsession;

import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.util.lang.reflection.Reflections;
import de.invesdwin.util.lang.string.Strings;

@ThreadSafe
public final class RsessionLogger implements RLog {

    @GuardedBy("this")
    private final StringBuilder errorMessage = new StringBuilder();

    public RsessionLogger() {}

    @Override
    public synchronized void log(final String text, final Level level) {
        if (Strings.isNotBlank(text)) {
            switch (level) {
            case OUTPUT:
                IScriptTaskRunnerR.LOG.debug(text);
                errorMessage.setLength(0);
                break;
            case INFO:
                IScriptTaskRunnerR.LOG.info(text);
                errorMessage.setLength(0);
                break;
            case WARNING:
                IScriptTaskRunnerR.LOG.warn(text);
                errorMessage.append(text);
                errorMessage.append("\n");
                break;
            case ERROR:
                IScriptTaskRunnerR.LOG.error(text);
                errorMessage.append(text);
                errorMessage.append("\n");
                break;
            default:
                IScriptTaskRunnerR.LOG.trace(text);
                errorMessage.setLength(0);
                break;
            }
        }
    }

    public synchronized String getErrorMessage() {
        if (errorMessage.length() == 0) {
            return null;
        }
        return String.valueOf(errorMessage).trim();
    }

    @Override
    public synchronized void closeLog() {
        errorMessage.setLength(0);
    }

    public static RsessionLogger get(final Rsession rsession) {
        return (RsessionLogger) Reflections.field("loggers").ofType(List.class).in(rsession).get().get(0);
    }

}
