package de.invesdwin.context.r.runtime.rserve;

import javax.annotation.concurrent.Immutable;

import org.math.R.RserverConf;

import de.invesdwin.context.system.properties.IProperties;
import de.invesdwin.context.system.properties.SystemProperties;
import de.invesdwin.util.lang.string.Strings;

@Immutable
public final class RserveProperties {

    public static final RserverConf RSERVER_CONF;
    public static final RserverConfMode RSERVER_CONF_MODE;
    public static final String RSERVER_REPOSITORY;

    static {
        final SystemProperties systemProperties = new SystemProperties(RserveProperties.class);
        if (systemProperties.containsValue("RSERVER_CONF_MODE")) {
            RSERVER_CONF_MODE = systemProperties.getEnum(RserverConfMode.class, "RSERVER_CONF_MODE");
        } else {
            RSERVER_CONF_MODE = RserverConfMode.LOCAL_SPAWN;
        }
        if (systemProperties.containsValue("RSERVER_CONF")) {
            final String rserverConf = systemProperties.getString("RSERVER_CONF");
            RSERVER_CONF = RserverConf.parse(rserverConf);
            if (RSERVER_CONF_MODE == RserverConfMode.LOCAL) {
                //override port for local instance
                if (RserveProperties.RSERVER_CONF != null) {
                    RSERVER_CONF.port = RserverConf.DEFAULT_RSERVE_PORT;
                }
                systemProperties.setString("RSERVER_CONF", Strings.asString(RSERVER_CONF));
            }
        } else {
            if (RSERVER_CONF_MODE == RserverConfMode.LOCAL) {
                RSERVER_CONF = new RserverConf(RserverConf.DEFAULT_RSERVE_HOST, RserverConf.DEFAULT_RSERVE_PORT, null,
                        null);
            } else {
                RSERVER_CONF = null;
            }
        }
        if (RSERVER_CONF != null) {
            systemProperties.maybeLogSecurityWarning("RSERVER_CONF (username)", RSERVER_CONF.login,
                    IProperties.INVESDWIN_DEFAULT_PASSWORD);
            systemProperties.maybeLogSecurityWarning("RSERVER_CONF (password)", RSERVER_CONF.password,
                    IProperties.INVESDWIN_DEFAULT_PASSWORD);
        }
        RSERVER_REPOSITORY = systemProperties.getString("RSERVER_REPOSITORY");
    }

    private RserveProperties() {}

}
