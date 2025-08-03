package de.invesdwin.scripting.matlab.runtime.jascib;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class JascibProperties {

    public static final String SCILAB_COMMAND;

    static {

        final SystemProperties systemProperties = new SystemProperties(JascibProperties.class);
        if (systemProperties.containsValue("SCILAB_COMMAND")) {
            SCILAB_COMMAND = systemProperties.getString("SCILAB_COMMAND");
        } else {
            SCILAB_COMMAND = null;
        }
    }

    private JascibProperties() {}

}
