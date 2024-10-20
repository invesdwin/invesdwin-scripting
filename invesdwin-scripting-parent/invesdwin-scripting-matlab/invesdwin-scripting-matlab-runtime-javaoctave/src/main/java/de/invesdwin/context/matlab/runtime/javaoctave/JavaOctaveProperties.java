package de.invesdwin.context.matlab.runtime.javaoctave;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;

@Immutable
public final class JavaOctaveProperties {

    public static final String OCTAVE_COMMAND;

    static {
        final SystemProperties systemProperties = new SystemProperties(JavaOctaveProperties.class);
        if (systemProperties.containsValue("OCTAVE_COMMAND")) {
            OCTAVE_COMMAND = systemProperties.getString("OCTAVE_COMMAND");
        } else {
            OCTAVE_COMMAND = null;
        }
    }

    private JavaOctaveProperties() {}

}
