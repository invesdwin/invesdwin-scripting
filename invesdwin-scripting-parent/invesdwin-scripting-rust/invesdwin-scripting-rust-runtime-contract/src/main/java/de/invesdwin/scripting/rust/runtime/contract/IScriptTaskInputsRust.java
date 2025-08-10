package de.invesdwin.scripting.rust.runtime.contract;

import de.invesdwin.scripting.IScriptTaskInputs;

public interface IScriptTaskInputsRust extends IScriptTaskInputs {

    String TYPE_BYTE = "i8";
    String TYPE_CHARACTER = "char";
    String TYPE_SHORT = "i16";
    String TYPE_INTEGER = "i32";
    String TYPE_LONG = "i64";
    String TYPE_FLOAT = "f32";
    String TYPE_DOUBLE = "f64";
    String TYPE_STRING = "&str";
    String TYPE_BOOLEAN = "bool";

    String NAN = "f64::NAN";

    void cargoAdd(String module);

    @Override
    default void putExpression(final String variable, final String expression) {
        getEngine().eval("let " + variable + " = " + expression + ";");
    }

    @Override
    default void putNull(final String variable) {
        putExpression(variable, NAN);
    }

    @Override
    default void remove(final String variable) {
        putNull(variable);
    }

    default String newVariable(final String variable, final String type) {
        return variable + ": " + type;
    }

    default String newVectorVariable(final String variable, final String type, final int cols) {
        return variable + ": [" + type + "; " + cols + "]";
    }

    default String newMatrixVariable(final String variable, final String type, final int cols, final int rows) {
        return variable + ": [[" + type + "; " + cols + "]; " + rows + "]";
    }

}
