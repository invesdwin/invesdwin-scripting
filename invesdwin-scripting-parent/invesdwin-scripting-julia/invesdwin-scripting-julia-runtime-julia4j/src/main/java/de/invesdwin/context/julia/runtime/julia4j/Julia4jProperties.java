package de.invesdwin.context.julia.runtime.julia4j;

import java.util.List;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;

@Immutable
public final class Julia4jProperties {

    public static final List<String> JULIA_LIBRARY_PATH;

    static {
        final SystemProperties systemProperties = new SystemProperties(Julia4jProperties.class);
        if (systemProperties.containsValue("JULIA_LIBRARY_PATH")) {
            JULIA_LIBRARY_PATH = systemProperties.getList("JULIA_LIBRARY_PATH");
        } else {
            JULIA_LIBRARY_PATH = null;
        }
    }

    private Julia4jProperties() {}

}
