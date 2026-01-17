package de.invesdwin.scripting.haskell.runtime.frege;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;
import io.netty.util.concurrent.FastThreadLocal;

@ThreadSafe
public final class FregeProperties {

    private static final SystemProperties SYSTEM_PROPERTIES = new SystemProperties(FregeProperties.class);
    private static final String KEY_FORKED_REPL_PROCESS = "FORKED_REPL_PROCESS";
    private static final FastThreadLocal<Boolean> FORKED_REPL_PROCESS_HOLDER = new FastThreadLocal<>();

    private FregeProperties() {}

    public static boolean isDefaultForkedReplProcess() {
        return SYSTEM_PROPERTIES.getBooleanOptional(KEY_FORKED_REPL_PROCESS, true);
    }

    public static void setDefaultForkedReplProcess(final boolean forkedReplProcess) {
        SYSTEM_PROPERTIES.setBoolean(KEY_FORKED_REPL_PROCESS, forkedReplProcess);
    }

    public static boolean isForkedReplProcess() {
        final Boolean forkedReplProcess = FORKED_REPL_PROCESS_HOLDER.get();
        if (forkedReplProcess != null) {
            return forkedReplProcess;
        } else {
            return isDefaultForkedReplProcess();
        }
    }

    public static void setForkedReplProcess(final Boolean forkedReplProcess) {
        if (forkedReplProcess == null) {
            FORKED_REPL_PROCESS_HOLDER.remove();
        } else {
            FORKED_REPL_PROCESS_HOLDER.set(forkedReplProcess);
        }
    }

}
