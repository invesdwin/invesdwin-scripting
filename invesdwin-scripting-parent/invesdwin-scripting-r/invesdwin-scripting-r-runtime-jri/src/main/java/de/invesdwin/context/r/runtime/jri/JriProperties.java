package de.invesdwin.context.r.runtime.jri;

import java.io.File;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;

@Immutable
public final class JriProperties {

    public static final File JRI_LIBRARY_PATH;

    static {
        final SystemProperties systemProperties = new SystemProperties(JriProperties.class);
        if (systemProperties.containsValue("JRI_LIBRARY_PATH")) {
            JRI_LIBRARY_PATH = systemProperties.getFile("JRI_LIBRARY_PATH");
        } else {
            JRI_LIBRARY_PATH = null;
        }
    }

    private JriProperties() {}

}
