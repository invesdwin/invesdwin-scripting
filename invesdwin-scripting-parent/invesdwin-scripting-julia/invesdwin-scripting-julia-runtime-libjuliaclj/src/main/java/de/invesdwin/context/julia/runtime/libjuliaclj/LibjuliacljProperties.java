package de.invesdwin.context.julia.runtime.libjuliaclj;

import java.io.File;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;

@Immutable
public final class LibjuliacljProperties {

    public static final File JULIA_HOME;

    static {
        final SystemProperties systemProperties = new SystemProperties(LibjuliacljProperties.class);
        if (systemProperties.containsValue("JULIA_HOME")) {
            JULIA_HOME = systemProperties.getFile("JULIA_HOME");
        } else {
            JULIA_HOME = null;
        }
    }

    private LibjuliacljProperties() {
    }

}
