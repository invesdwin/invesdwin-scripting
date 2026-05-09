package de.invesdwin.scripting.runtime.lua.callback;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.callback.ObjectScriptTaskReturns;
import de.invesdwin.util.lang.string.Strings;

@NotThreadSafe
public class LuaObjectScriptTaskReturns extends ObjectScriptTaskReturns {

    @Override
    public void returnCharacter(final char value) {
        returnString(Strings.checkedCast(value));
    }

    @Override
    public void returnCharacterVector(final char[] value) {
        returnStringVector(Strings.checkedCastVector(value));
    }

    @Override
    public void returnCharacterMatrix(final char[][] value) {
        returnStringMatrix(Strings.checkedCastMatrix(value));
    }

}
