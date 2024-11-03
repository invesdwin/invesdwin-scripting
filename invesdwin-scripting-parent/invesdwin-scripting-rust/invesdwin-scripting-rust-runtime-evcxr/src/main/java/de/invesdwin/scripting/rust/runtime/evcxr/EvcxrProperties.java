package de.invesdwin.scripting.rust.runtime.evcxr;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class EvcxrProperties {

    public static final String EVCXR_COMMAND;

    static {
        final SystemProperties systemProperties = new SystemProperties(EvcxrProperties.class);
        if (systemProperties.containsValue("EVCXR_COMMAND")) {
            EVCXR_COMMAND = systemProperties.getString("EVCXR_COMMAND");
        } else {
            EVCXR_COMMAND = null;
        }
    }

    private EvcxrProperties() {}

}
