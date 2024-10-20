package de.invesdwin.context.r.runtime.rserve;

import javax.annotation.concurrent.Immutable;

@Immutable
public enum RserverConfMode {
    LOCAL,
    LOCAL_SPAWN,
    REMOTE;
}
