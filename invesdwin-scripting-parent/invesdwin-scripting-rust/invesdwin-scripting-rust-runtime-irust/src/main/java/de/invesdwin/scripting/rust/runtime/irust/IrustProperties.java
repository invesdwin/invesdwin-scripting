package de.invesdwin.scripting.rust.runtime.irust;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class IrustProperties {

    public static final String IRUST_COMMAND;

    static {
        final SystemProperties systemProperties = new SystemProperties(IrustProperties.class);
        if (systemProperties.containsValue("IRUST_COMMAND")) {
            IRUST_COMMAND = systemProperties.getString("IRUST_COMMAND");
        } else {
            IRUST_COMMAND = null;
        }
    }

    private IrustProperties() {}

}
