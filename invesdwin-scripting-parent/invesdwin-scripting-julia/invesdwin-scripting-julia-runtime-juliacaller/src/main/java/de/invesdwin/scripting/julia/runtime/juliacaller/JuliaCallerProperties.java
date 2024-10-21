package de.invesdwin.scripting.julia.runtime.juliacaller;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class JuliaCallerProperties {

    public static final String JULIA_COMMAND;

    static {

        final SystemProperties systemProperties = new SystemProperties(JuliaCallerProperties.class);
        if (systemProperties.containsValue("JULIA_COMMAND")) {
            JULIA_COMMAND = systemProperties.getString("JULIA_COMMAND");
        } else {
            JULIA_COMMAND = null;
        }
    }

    private JuliaCallerProperties() {
    }

}
