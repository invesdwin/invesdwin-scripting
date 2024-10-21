package de.invesdwin.scripting.julia.runtime.julia4j.internal;

import java.io.File;
import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

import org.julia.jni.NativeUtils;
import org.julia.jni.swig.Julia4J;
import org.julia.jni.swig.SWIGTYPE_p_jl_value_t;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.NullNode;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.julia.runtime.contract.IScriptTaskRunnerJulia;
import de.invesdwin.scripting.julia.runtime.contract.JuliaResetContext;
import de.invesdwin.scripting.julia.runtime.julia4j.Julia4jProperties;
import de.invesdwin.scripting.julia.runtime.julia4j.Julia4jScriptTaskEngineJulia;
import de.invesdwin.util.concurrent.Executors;
import de.invesdwin.util.concurrent.WrappedExecutorService;
import de.invesdwin.util.concurrent.future.Futures;
import de.invesdwin.util.concurrent.lock.IReentrantLock;
import de.invesdwin.util.concurrent.lock.Locks;
import de.invesdwin.util.lang.string.Strings;
import de.invesdwin.util.math.Booleans;

/**
 * Always acquire the lock first before accessing the julia engine instance. Also make sure commands are only executed
 * from inside the EXECUTOR thread. Otherwise julia will throw errors due to being thread bound.
 * 
 * https://cnuernber.github.io/libjulia-clj/signals.html
 */
@NotThreadSafe
public final class UnsafeJuliaEngineWrapper implements IJuliaEngineWrapper {

    public static final WrappedExecutorService EXECUTOR = Executors
            .newFixedThreadPool(UnsafeJuliaEngineWrapper.class.getSimpleName(), 1);
    public static final UnsafeJuliaEngineWrapper INSTANCE = new UnsafeJuliaEngineWrapper();

    private final IReentrantLock lock;
    private final JuliaResetContext resetContext;
    private final ObjectMapper mapper;
    private boolean initialized = false;

    private UnsafeJuliaEngineWrapper() {
        for (final String dir : Julia4jProperties.JULIA_LIBRARY_PATH) {
            final File file = new File(dir, "libjulia.so");
            if (file.exists()) {
                System.load(file.getAbsolutePath());
                break;
            }
        }
        try {
            ModifiedNativeUtils.loadLibraryFromJar(NativeUtils.libnameToPlatform("libjulia4j"));
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
        this.lock = Locks.newReentrantLock(UnsafeJuliaEngineWrapper.class.getSimpleName() + "_lock");
        this.resetContext = new JuliaResetContext(new Julia4jScriptTaskEngineJulia(this));
        Futures.waitNoInterrupt(EXECUTOR.submit(() -> init()));
    }

    private void init() {
        if (initialized) {
            return;
        }
        if (Julia4J.jl_is_initialized() == 0) {
            Julia4J.jl_init();
        }
        evalUnchecked(
                "using InteractiveUtils; using Pkg; isinstalled(pkg::String) = any(x -> x.name == pkg && x.is_direct_dep, values(Pkg.dependencies())); if !isinstalled(\"JSON\"); Pkg.add(\"JSON\"); end; using JSON;");
        evalUnchecked(
                "function j4j_exec(cmd) try eval(Meta.parse(cmd)); return nothing catch err @error err; return sprint(showerror, err, backtrace()); end; end");
        evalUnchecked(
                "function j4j_get(cmd) try return JSON.json(eval(Meta.parse(cmd))); catch err @error err; return sprint(showerror, err, backtrace()); end; end;");
        this.resetContext.init();
        initialized = true;
    }

    @Override
    public void exec(final String eval) {
        final String command = "j4j_exec(\"begin "
                + Strings.normalizeNewlines(eval.trim()).replace("\n", "\\n").replace("\"", "\\\"") + "\\nend\")";
        IScriptTaskRunnerJulia.LOG.debug("> exec %s", eval);
        final SWIGTYPE_p_jl_value_t value = Julia4J.jl_eval_string(command);
        try {
            assertResponseNotNull(eval, value);
            final String error = Julia4J.jl_unbox_string(value);
            assertResponseSuccess(eval, error);
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void evalUnchecked(final String eval) {
        final String command = eval + "; true";
        IScriptTaskRunnerJulia.LOG.debug("> eval %s", eval);
        final SWIGTYPE_p_jl_value_t value = Julia4J.jl_eval_string(command);
        try {
            assertResponseNotNull(eval, value);
            final boolean success = Booleans.checkedCast(Julia4J.jl_unbox_bool(value));
            assertResponseSuccess(eval, success);
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public JsonNode getAsJsonNode(final String variable) {
        final String command = "j4j_get(\"" + variable + "\")";
        IScriptTaskRunnerJulia.LOG.debug("> get %s", variable);
        final SWIGTYPE_p_jl_value_t value = Julia4J.jl_eval_string(command);
        try {
            assertResponseNotNull(variable, value);
            final String result = Julia4J.jl_unbox_string(value);
            final JsonNode node = mapper.readTree(result);
            if (node instanceof NullNode) {
                return null;
            } else {
                return node;
            }
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void assertResponseNotNull(final String command, final SWIGTYPE_p_jl_value_t value) throws IOException {
        if (value == null) {
            throw new IllegalStateException("Command [" + Strings.truncate(command, 100).replace("\n", "\\n")
                    + "] returned null response which might be caused by a parser error");
        }
    }

    private void assertResponseSuccess(final String command, final boolean success) throws IOException {
        if (!success) {
            throw new IllegalStateException(
                    "Command [" + Strings.truncate(command, 100).replace("\n", "\\n") + "] returned a false response");
        }
    }

    private void assertResponseSuccess(final String command, final String error) throws IOException {
        if (Strings.isNotBlank(error)) {
            throw new IllegalStateException("Command [" + Strings.truncate(command, 100).replace("\n", "\\n")
                    + "] returned an error: " + error);
        }
    }

    @Override
    public void reset() {
        resetContext.reset();
    }

    @Override
    public IReentrantLock getLock() {
        return lock;
    }

}
