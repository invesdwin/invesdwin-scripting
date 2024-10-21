package de.invesdwin.scripting.julia.sfrontiers;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.scripting.IScriptTaskEngine;
import de.invesdwin.scripting.IScriptTaskInputs;
import de.invesdwin.scripting.IScriptTaskResults;
import de.invesdwin.scripting.julia.runtime.contract.AScriptTaskJulia;

@NotThreadSafe
public class SfrontiersScriptTask extends AScriptTaskJulia<double[]> {

    private final double[] output;
    private final double[][] input;

    /**
     * The input vectors should only be of the size that the actual trades were of.
     */
    public SfrontiersScriptTask(final double[] output, final double[][] input) {
        this.output = output;
        this.input = input;
    }

    @Override
    public void populateInputs(final IScriptTaskInputs inputs) {
        inputs.putDoubleVector("y", output);
        inputs.putDoubleMatrix("x", input);
        final double[] cons = new double[output.length];
        for (int i = 0; i < cons.length; i++) {
            cons[i] = 1D;
        }
        inputs.putDoubleVector("cons", cons);
    }

    @Override
    public void executeScript(final IScriptTaskEngine engine) {
        engine.eval(new ClassPathResource(SfrontiersScriptTask.class.getSimpleName() + ".jl", getClass()));
    }

    @Override
    public double[] extractResults(final IScriptTaskResults results) {
        final double[] efficiencyScorePerStrategy = results.getDoubleVector("res.bc");
        return efficiencyScorePerStrategy;
    }

}
