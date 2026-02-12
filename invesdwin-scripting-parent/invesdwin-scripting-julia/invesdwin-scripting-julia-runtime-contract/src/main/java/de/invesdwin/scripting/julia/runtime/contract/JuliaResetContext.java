package de.invesdwin.scripting.julia.runtime.contract;

import java.util.Map;
import java.util.Set;

import javax.annotation.concurrent.NotThreadSafe;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import de.invesdwin.context.integration.marshaller.MarshallerJsonJackson;
import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.util.collections.factory.ILockCollectionFactory;
import de.invesdwin.util.collections.factory.pool.set.ICloseableSet;
import de.invesdwin.util.collections.factory.pool.set.PooledSet;
import de.invesdwin.util.lang.Objects;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class JuliaResetContext {

    private final IScriptTaskEngine engine;

    private final Set<String> protectedVariables = ILockCollectionFactory.getInstance(false).newSet();
    private final Map<String, String> variable_size = ILockCollectionFactory.getInstance(false).newMap();

    private final ObjectMapper mapper;

    public JuliaResetContext(final IScriptTaskEngine engine) {
        this.engine = engine;
        this.mapper = MarshallerJsonJackson.getInstance().getJsonMapper(false);
    }

    public void init() {
        final String[] array = engine.getResults().getStringVector("names(Main)");
        for (int i = 0; i < array.length; i++) {
            final String str = array[i];
            protectedVariables.add(Strings.removeStart(str.trim(), ":"));
        }
    }

    public void reset() {
        final JsonNode varinfo = varinfo();
        ICloseableSet<String> changed = null;
        for (int i = 0; i < varinfo.size(); i++) {
            final JsonNode row = varinfo.get(i);
            final String name = row.get(0).asText();
            if (protectedVariables.contains(name)) {
                continue;
            }
            final String summary = row.get(2).asText();
            if ("DataType".equals(summary)) {
                //can not redefine data types
                continue;
            }
            if (summary.startsWith("typeof(") || summary.contains(" function ")) {
                /*
                 * we don't know the arguments to functions here so we can not redifne them properly. They should not
                 * take too much memory, so it should be fine to just leave them, new function definitions will
                 * overwrite them
                 */
                continue;
            }
            final String size = row.get(1).asText();
            final String existingSize = variable_size.get(size);
            if (Objects.equals(size, existingSize)) {
                continue;
            }
            if ("Module".equals(summary)) {
                try {
                    engine.eval("module " + name + " end");
                } catch (final Throwable t) {
                    //ignore, might be an error due to a previous command
                }
            } else {
                try {
                    //Variables, Arrays, Methods
                    engine.eval(name + " = nothing");
                } catch (final Throwable t) {
                    //ignore, might be an error due to a previous command
                }
            }
            if (changed == null) {
                changed = PooledSet.getInstance();
            }
            changed.add(name);
        }
        if (changed != null) {
            updateSizeMap(changed);
            changed.close();
        }
    }

    private void updateSizeMap(final Set<String> changed) {
        final JsonNode varinfo = varinfo();
        for (int i = 0; i < varinfo.size(); i++) {
            final JsonNode row = varinfo.get(i);
            final String name = row.get(0).asText();
            if (changed.contains(name)) {
                final String summary = row.get(2).asText();
                if (!"Nothing".equals(summary) && !"Module".equals(summary)) {
                    variable_size.remove(name);
                    protectedVariables.add(name);
                } else {
                    final String size = row.get(1).asText();
                    variable_size.put(name, size);
                }
            }
        }
    }

    /**
     * returns a two dimensional array with columns at index 0: name, size, summary
     * 
     * Thus iterate from index 1 to access the actual values.
     */
    public JsonNode varinfo() {
        String json;
        try {
            json = getVarinfo();
        } catch (final Throwable t) {
            //retry because an asynchronous error from a previous command might have been found which we want to ignore
            json = getVarinfo();
        }
        try {
            final JsonNode node = mapper.readTree(json);
            final JsonNode rows = node.get("content").get(0).get("rows");
            return rows;
        } catch (final JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    private String getVarinfo() {
        return engine.getResults().getString("JSON.json(varinfo(); allownan=true)");
    }

}
