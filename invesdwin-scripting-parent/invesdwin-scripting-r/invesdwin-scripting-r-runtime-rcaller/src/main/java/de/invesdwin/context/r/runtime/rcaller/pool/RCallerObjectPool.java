package de.invesdwin.context.r.runtime.rcaller.pool;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import com.github.rcaller.rstuff.RCaller;

import de.invesdwin.context.r.runtime.contract.IScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.rcaller.RCallerScriptTaskRunnerR;
import de.invesdwin.context.r.runtime.rcaller.pool.internal.ModifiedRCaller;
import de.invesdwin.util.concurrent.pool.timeout.ATimeoutObjectPool;
import de.invesdwin.util.time.date.FTimeUnit;
import de.invesdwin.util.time.duration.Duration;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class RCallerObjectPool extends ATimeoutObjectPool<RCaller> implements FactoryBean<RCallerObjectPool> {

    public static final RCallerObjectPool INSTANCE = new RCallerObjectPool();

    private RCallerObjectPool() {
        super(Duration.ONE_MINUTE, new Duration(10, FTimeUnit.SECONDS));
    }

    @Override
    public void invalidateObject(final RCaller element) {
        element.StopRCallerOnline();
    }

    @Override
    protected RCaller newObject() {
        return new ModifiedRCaller();
    }

    @Override
    protected boolean passivateObject(final RCaller element) {
        element.getRCode().clear();
        element.getRCode().getCode().insert(0, IScriptTaskRunnerR.CLEANUP_SCRIPT + "\n");
        element.getRCode().addRCode(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE + " <- c()");
        element.runAndReturnResultOnline(RCallerScriptTaskRunnerR.INTERNAL_RESULT_VARIABLE);
        element.deleteTempFiles();
        return true;
    }

    @Override
    public RCallerObjectPool getObject() {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return RCallerObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
