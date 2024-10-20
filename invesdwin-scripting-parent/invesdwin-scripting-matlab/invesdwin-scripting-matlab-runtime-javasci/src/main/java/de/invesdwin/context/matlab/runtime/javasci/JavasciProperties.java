package de.invesdwin.context.matlab.runtime.javasci;

import java.io.File;
import java.util.List;

import javax.annotation.concurrent.Immutable;

import de.invesdwin.context.system.properties.SystemProperties;
import de.invesdwin.instrument.DynamicInstrumentationReflections;
import de.invesdwin.util.collections.Collections;

@Immutable
public final class JavasciProperties {

    public static final List<String> JAVASCI_LIBRARY_PATHS;
    public static final String SCILAB_PATH;

    static {
        final SystemProperties systemProperties = new SystemProperties(JavasciProperties.class);
        if (systemProperties.containsValue("JAVASCI_LIBRARY_PATHS")) {
            JAVASCI_LIBRARY_PATHS = systemProperties.getList("JAVASCI_LIBRARY_PATHS");
        } else {
            JAVASCI_LIBRARY_PATHS = Collections.emptyList();
        }
        for (final String path : JavasciProperties.JAVASCI_LIBRARY_PATHS) {
            DynamicInstrumentationReflections.addPathToJavaLibraryPath(new File(path));
        }
        if (systemProperties.containsValue("SCILAB_PATH")) {
            SCILAB_PATH = detectScilabPath(systemProperties);
        } else {
            SCILAB_PATH = null;
        }
    }

    private JavasciProperties() {}

    private static String detectScilabPath(final SystemProperties systemProperties) {
        final List<String> scilabPaths = systemProperties.getList("SCILAB_PATH");
        for (int i = 0; i < scilabPaths.size(); i++) {
            final String scilabPath = scilabPaths.get(i);
            if (new File(scilabPath).exists()) {
                return scilabPath;
            }
        }
        return null;
    }

}
