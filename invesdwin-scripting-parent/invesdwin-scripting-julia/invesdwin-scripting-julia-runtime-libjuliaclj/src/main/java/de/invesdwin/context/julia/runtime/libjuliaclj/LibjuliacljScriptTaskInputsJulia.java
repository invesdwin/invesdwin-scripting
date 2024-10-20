package de.invesdwin.context.julia.runtime.libjuliaclj;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.context.julia.runtime.contract.IScriptTaskInputsJulia;

@NotThreadSafe
public class LibjuliacljScriptTaskInputsJulia implements IScriptTaskInputsJulia {

    private final LibjuliacljScriptTaskEngineJulia engine;

    public LibjuliacljScriptTaskInputsJulia(final LibjuliacljScriptTaskEngineJulia engine) {
        this.engine = engine;
    }

    @Override
    public LibjuliacljScriptTaskEngineJulia getEngine() {
        return engine;
    }

    @Override
    public void putCharacter(final String variable, final char value) {
        putExpression(variable, "Char('" + value + "')");
    }

    @Override
    public void putCharacterVector(final String variable, final char[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putCharacterVectorAsString(variable, value);
        }
    }

    @Override
    public void putCharacterMatrix(final String variable, final char[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Char}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putCharacterMatrixAsString(variable, value);
        }
    }

    @Override
    public void putString(final String variable, final String value) {
        if (value == null) {
            putNull(variable);
        } else {
            putExpression(variable, "String(\"" + value + "\")");
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putStringVectorAsString(variable, value);
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{String}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putStringMatrixAsString(variable, value);
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        putExpression(variable, "Bool(" + String.valueOf(value) + ")");
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putBooleanVectorAsString(variable, value);
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Bool}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putBooleanMatrixAsString(variable, value);
        }
    }

    @Override
    public void putByte(final String variable, final byte value) {
        putExpression(variable, "Int8(" + String.valueOf(value) + ")");
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putByteVector(variable, value);
        }
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Int8}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putByteMatrix(variable, value);
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        putExpression(variable, "Int16(" + String.valueOf(value) + ")");
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putShortVector(variable, value);
        }
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Int16}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putShortMatrix(variable, value);
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        putExpression(variable, "Int32(" + String.valueOf(value) + ")");
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putIntegerVector(variable, value);
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Int32}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putIntegerMatrix(variable, value);
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        putExpression(variable, "Int64(" + String.valueOf(value) + ")");
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putLongVector(variable, value);
        }
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Int64}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putLongMatrix(variable, value);
        }
    }

    @Override
    public void putFloat(final String variable, final float value) {
        putExpression(variable, "Float32(" + String.valueOf(value) + ")");
    }

    @Override
    public void putFloatVector(final String variable, final float[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putFloatVector(variable, value);
        }
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Float32}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putFloatMatrix(variable, value);
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        putExpression(variable, "Float64(" + String.valueOf(value) + ")");
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            getEngine().unwrap().putDoubleVector(variable, value);
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        if (value == null) {
            putNull(variable);
        } else if (value.length == 0 || value[0].length == 0) {
            putExpression(variable, "Array{Float64}(undef, " + value.length + ", 0)");
        } else {
            getEngine().unwrap().putDoubleMatrix(variable, value);
        }
    }

}
