package de.invesdwin.scripting.julia.runtime.julia4j.internal;

import com.fasterxml.jackson.databind.JsonNode;

import de.invesdwin.util.concurrent.lock.IReentrantLock;

public interface IJuliaEngineWrapper {

    void exec(String command);

    JsonNode getAsJsonNode(String variable);

    void reset();

    IReentrantLock getLock();

}
