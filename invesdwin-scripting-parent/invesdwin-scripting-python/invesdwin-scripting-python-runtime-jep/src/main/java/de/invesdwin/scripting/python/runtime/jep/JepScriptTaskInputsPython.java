package de.invesdwin.scripting.python.runtime.jep;

import javax.annotation.concurrent.NotThreadSafe;

import de.invesdwin.scripting.python.runtime.contract.IScriptTaskInputsPython;
import jep.JepException;

@NotThreadSafe
public class JepScriptTaskInputsPython implements IScriptTaskInputsPython {

    private final JepScriptTaskEnginePython engine;

    public JepScriptTaskInputsPython(final JepScriptTaskEnginePython engine) {
        this.engine = engine;
    }

    @Override
    public JepScriptTaskEnginePython getEngine() {
        return engine;
    }

    @Override
    public void putByte(final String variable, final byte value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putByteVector(final String variable, final byte[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putByteMatrix(final String variable, final byte[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putCharacter(final String variable, final char value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putCharacterVector(final String variable, final char[] value) {
        if (value == null) {
            putNull(variable);
        } else {
            try {
                engine.unwrap().set(variable, value);
            } catch (final JepException e) {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void putCharacterMatrix(final String variable, final char[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putString(final String variable, final String value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putStringVector(final String variable, final String[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putStringMatrix(final String variable, final String[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putFloat(final String variable, final float value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putFloatVector(final String variable, final float[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putFloatMatrix(final String variable, final float[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putDouble(final String variable, final double value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putDoubleVector(final String variable, final double[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putDoubleMatrix(final String variable, final double[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putShort(final String variable, final short value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putShortVector(final String variable, final short[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putShortMatrix(final String variable, final short[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putInteger(final String variable, final int value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putIntegerVector(final String variable, final int[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putIntegerMatrix(final String variable, final int[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putLong(final String variable, final long value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putLongVector(final String variable, final long[] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putLongMatrix(final String variable, final long[][] value) {
        try {
            engine.unwrap().set(variable, value);
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putBoolean(final String variable, final boolean value) {
        try {
            engine.unwrap().set(variable, value);
            putExpression(variable, "bool(" + variable + ")");
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putBooleanVector(final String variable, final boolean[] value) {
        try {
            engine.unwrap().set(variable, value);
            if (value != null) {
                putExpression(variable, "[bool(x) for x in " + variable + "]");
            }
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void putBooleanMatrix(final String variable, final boolean[][] value) {
        try {
            engine.unwrap().set(variable, value);
            if (value != null) {
                putExpression(variable, "[[bool(y) for y in x] for x in " + variable + "]");
            }
        } catch (final JepException e) {
            throw new RuntimeException(e);
        }
    }

}
