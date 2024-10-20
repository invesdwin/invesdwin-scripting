package de.invesdwin.context.haskell.runtime.ghci;

import javax.annotation.concurrent.ThreadSafe;

import de.invesdwin.context.system.properties.SystemProperties;

@ThreadSafe
public final class GhciProperties {

    public static final String GHCI_COMMAND;

    static {

        final SystemProperties systemProperties = new SystemProperties(GhciProperties.class);
        if (systemProperties.containsValue("GHCI_COMMAND")) {
            GHCI_COMMAND = systemProperties.getString("GHCI_COMMAND");
        } else {
            GHCI_COMMAND = null;
        }
    }

    private GhciProperties() {}

}
