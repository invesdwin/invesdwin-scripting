package de.invesdwin.context.r.optimalf;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;

import org.springframework.core.io.ClassPathResource;

import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.integration.script.IScriptTaskInputs;
import de.invesdwin.context.integration.script.IScriptTaskResults;
import de.invesdwin.context.r.runtime.contract.AScriptTaskR;
import de.invesdwin.util.math.Integers;

@NotThreadSafe
public class OptimalfScriptTask extends AScriptTaskR<List<Double>> {

    private final List<? extends List<Double>> tradesPerStrategy;

    /**
     * The input vectors should only be of the size that the actual trades were of.
     */
    public OptimalfScriptTask(final List<? extends List<Double>> tradesPerStrategy) {
        this.tradesPerStrategy = tradesPerStrategy;
    }

    @Override
    public void populateInputs(final IScriptTaskInputs inputs) {
        int maxTradesCount = 0;
        for (int i = 0; i < tradesPerStrategy.size(); i++) {
            final List<Double> strategy = tradesPerStrategy.get(i);
            maxTradesCount = Integers.max(maxTradesCount, strategy.size());
        }
        final double[][] tradesMatrix = new double[tradesPerStrategy.size()][];
        final double[][] probabilitiesMatrix = new double[tradesPerStrategy.size()][];
        for (int strategyIdx = 0; strategyIdx < tradesPerStrategy.size(); strategyIdx++) {
            final List<Double> strategy = tradesPerStrategy.get(strategyIdx);
            /*
             * we fill up the missing trades on the matrix with 0 trades having 0 probability, so the matrix has uniform
             * size per vector
             */
            final double[] tradesVector = new double[maxTradesCount + 1];
            final double[] probabilitiesVector = new double[maxTradesCount + 1];
            final double tradesCount = strategy.size();
            final double tradeProbability = 1D / tradesCount;
            for (int tradeIdx = 0; tradeIdx < tradesCount; tradeIdx++) {
                final double trade = strategy.get(tradeIdx);
                tradesVector[tradeIdx] = trade;
                probabilitiesVector[tradeIdx] = tradeProbability;
            }
            tradesMatrix[strategyIdx] = tradesVector;
            probabilitiesMatrix[strategyIdx] = probabilitiesVector;
        }
        inputs.putDoubleMatrix("trades", tradesMatrix);
        inputs.putExpression("trades", "t(trades)");
        inputs.putDoubleMatrix("probabilities", probabilitiesMatrix);
        inputs.putExpression("probabilities", "t(probabilities)");
    }

    @Override
    public void executeScript(final IScriptTaskEngine engine) {
        engine.eval(new ClassPathResource(OptimalfScriptTask.class.getSimpleName() + ".R", getClass()));
    }

    @Override
    public List<Double> extractResults(final IScriptTaskResults results) {
        final double profit = results.getDouble("profit");
        if (profit <= 0) {
            //disable trading if resulting profit is negative
            return newDisabledResult();
        } else {
            final List<Double> optimalFPerStrategy = results.getDoubleVectorAsList("optimalf");
            return optimalFPerStrategy;
        }
    }

    private List<Double> newDisabledResult() {
        final List<Double> optimalFs = new ArrayList<Double>(tradesPerStrategy.size());
        for (int strategyIdx = 0; strategyIdx < tradesPerStrategy.size(); strategyIdx++) {
            optimalFs.add(0D);
        }
        return optimalFs;
    }

}
