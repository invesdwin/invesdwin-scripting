package de.invesdwin.scripting.rust.runtime.rhai;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class RhaiProperties {

    public static final String RHAI_COMMAND;

    static {
        final SystemProperties systemProperties = new SystemProperties(RhaiProperties.class);
        if (systemProperties.containsValue("RHAI_COMMAND")) {
            RHAI_COMMAND = systemProperties.getString("RHAI_COMMAND");
        } else {
            RHAI_COMMAND = null;
        }
    }

    private RhaiProperties() {}

}
