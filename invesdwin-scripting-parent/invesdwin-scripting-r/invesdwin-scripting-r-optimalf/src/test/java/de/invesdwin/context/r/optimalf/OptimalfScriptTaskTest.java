package de.invesdwin.context.r.optimalf;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.NotThreadSafe;
import jakarta.inject.Inject;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.jri.JriScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.rcaller.RCallerScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.renjin.RenjinScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.rserve.RserveScriptTaskRunnerR;
import de.invesdwin.context.test.ATest;
import de.invesdwin.util.assertions.Assertions;
import de.invesdwin.util.collections.Arrays;
import de.invesdwin.util.math.decimal.Decimal;

@NotThreadSafe
public class OptimalfScriptTaskTest extends ATest {

    private static final int ITERATIONS = 10;
    @Inject
    private RCallerScriptTaskRunnerR rcallerScriptTaskRunner;
    @Inject
    private RserveScriptTaskRunnerR rserveScriptTaskRunner;
    @Inject
    private JriScriptTaskRunnerR jriScriptTaskRunner;
    @Inject
    private RenjinScriptTaskRunnerR renjinScriptTaskRunner;

    @Test
    public void testRCaller() {
        for (int i = 0; i < ITERATIONS; i++) {
            run(rcallerScriptTaskRunner);
            log.info("------------------------");
        }
    }

    private void run(final IScriptTaskRunnerR runner) {
        final List<List<Double>> tradesPerStrategy = new ArrayList<>();
        //        0.5,-0.3,0.4,-0.2
        tradesPerStrategy.add(Arrays.asList(0.5, -0.3, 0.4, -0.2));
        //        0.1,-0.15,0.4,-0.1
        tradesPerStrategy.add(Arrays.asList(0.1, -0.15, 0.4, -0.1));
        final List<Double> optimalFsRaw = new OptimalfScriptTask(tradesPerStrategy).run(runner);
        final List<Decimal> optimalFs = new ArrayList<Decimal>();
        for (final Double optimalFStr : optimalFsRaw) {
            optimalFs.add(new Decimal(optimalFStr).round(3));
        }
        Assertions.assertThat(optimalFs).isEqualTo(Arrays.asList(new Decimal("0.052"), new Decimal("0.213")));
    }

    @Test
    @Disabled("not working right now")
    public void testRserve() {
        for (int i = 0; i < ITERATIONS; i++) {
            run(rserveScriptTaskRunner);
            log.info("------------------------");
        }
    }

    @Test
    public void testJri() {
        for (int i = 0; i < ITERATIONS; i++) {
            run(jriScriptTaskRunner);
            log.info("------------------------");
        }
    }

    @Test
    public void testRenjin() {
        for (int i = 0; i < ITERATIONS; i++) {
            run(renjinScriptTaskRunner);
            log.info("------------------------");
        }
    }

    @Test
    public void testNegative() {
        final List<List<Double>> tradesPerStrategy = new ArrayList<>();
        //        0.5,-0.3,0.4,-0.2
        tradesPerStrategy.add(Arrays.asList(-0.5, -0.3, -0.4, -0.2));
        //        0.1,-0.15,0.4,-0.1
        tradesPerStrategy.add(Arrays.asList(-0.1, -0.15, -0.4, -0.1));
        final List<Double> optimalFsRaw = new OptimalfScriptTask(tradesPerStrategy).run(rcallerScriptTaskRunner);
        final List<Decimal> optimalFs = new ArrayList<Decimal>();
        for (final Double optimalFStr : optimalFsRaw) {
            optimalFs.add(new Decimal(optimalFStr).round(3));
        }
        Assertions.assertThat(optimalFs).isEqualTo(Arrays.asList(Decimal.ZERO, Decimal.ZERO));
    }

    @Test
    public void testPositive() {
        final List<List<Double>> tradesPerStrategy = new ArrayList<>();
        //        0.5,-0.3,0.4,-0.2
        tradesPerStrategy.add(Arrays.asList(0.5, 0.3, 0.4, 0.2));
        //        0.1,-0.15,0.4,-0.1
        tradesPerStrategy.add(Arrays.asList(0.1, 0.15, 0.4, 0.1));
        try {
            new OptimalfScriptTask(tradesPerStrategy).run(rcallerScriptTaskRunner);
            Assertions.failExceptionExpected();
        } catch (final Throwable t) {
            Assertions.assertThat(t.getMessage())
                    .contains("all 'events' columns must have at least one negative trade");
        }
    }

    @Test
    public void testEmptyTrades() {
        final List<List<Double>> tradesPerStrategy = new ArrayList<>();
        //        0.5,-0.3,0.4,-0.2
        tradesPerStrategy.add(Arrays.asList());
        //        0.1,-0.15,0.4,-0.1
        tradesPerStrategy.add(Arrays.asList());
        try {
            new OptimalfScriptTask(tradesPerStrategy).run(rcallerScriptTaskRunner);
            Assertions.failExceptionExpected();
        } catch (final Throwable t) {
            Assertions.assertThat(t.getMessage())
                    .contains("all 'events' columns must have at least one negative trade");
        }
    }

    @Test
    public void testEmptyStrategies() {
        final List<List<Double>> tradesPerStrategy = new ArrayList<>();
        try {
            new OptimalfScriptTask(tradesPerStrategy).run(rcallerScriptTaskRunner);
            Assertions.failExceptionExpected();
        } catch (final Throwable t) {
            Assertions.assertThat(t.getMessage()).contains("No trades!");
        }
    }

}
