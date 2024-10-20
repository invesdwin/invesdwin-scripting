package de.invesdwin.context.matlab.runtime.matconsolectl;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;

@Immutable
public final class MatConsoleCtlProperties {

    public static final String MATLAB_COMMAND;

    static {
        final SystemProperties systemProperties = new SystemProperties(MatConsoleCtlProperties.class);
        if (systemProperties.containsValue("MATLAB_COMMAND")) {
            MATLAB_COMMAND = systemProperties.getString("MATLAB_COMMAND");
        } else {
            MATLAB_COMMAND = null;
        }
    }

    private MatConsoleCtlProperties() {}

}
