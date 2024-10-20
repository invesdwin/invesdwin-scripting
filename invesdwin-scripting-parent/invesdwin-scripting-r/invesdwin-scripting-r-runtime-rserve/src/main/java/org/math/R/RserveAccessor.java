// CHECKSTYLE:OFF
package org.math.R;
// CHECKSTYLE:ON

import javax.annotation.concurrent.Immutable;

@Immutable
public final class RserveAccessor {

    private RserveAccessor() {}

    public static RserverConf getConf(final RserveDaemon daemon) {
        return daemon.conf;
    }

    public static void setLocalRServe(final RserveSession session, final RserveDaemon daemon) {
        session.localRserve = daemon;
    }

}
