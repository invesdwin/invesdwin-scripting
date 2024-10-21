package de.invesdwin.scripting.julia.runtime.jajub;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class JajubProperties {

    public static final String JULIA_COMMAND;

    static {

        final SystemProperties systemProperties = new SystemProperties(JajubProperties.class);
        if (systemProperties.containsValue("JULIA_COMMAND")) {
            JULIA_COMMAND = systemProperties.getString("JULIA_COMMAND");
        } else {
            JULIA_COMMAND = null;
        }
    }

    private JajubProperties() {
    }

}
