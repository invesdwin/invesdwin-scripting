package de.invesdwin.scripting.haskell.runtime.frege.pool.jsr223;

import java.io.IOException;

import javax.annotation.concurrent.ThreadSafe;

import org.springframework.beans.factory.FactoryBean;

import de.invesdwin.util.concurrent.pool.AInvalidatingObjectPool;
import jakarta.inject.Named;

@ThreadSafe
@Named
public final class Jsr223FregeObjectPool extends AInvalidatingObjectPool<Jsr223FregeBridge>
        implements FactoryBean<Jsr223FregeObjectPool> {

    public static final Jsr223FregeObjectPool INSTANCE = new Jsr223FregeObjectPool();

    private Jsr223FregeObjectPool() {}

    @Override
    public void invalidateObject(final Jsr223FregeBridge element) {
        element.close();
    }

    @Override
    protected Jsr223FregeBridge newObject() {
        final Jsr223FregeBridge session = new Jsr223FregeBridge();
        try {
            session.open();
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
        return session;
    }

    @Override
    public Jsr223FregeObjectPool getObject() throws Exception {
        return INSTANCE;
    }

    @Override
    public Class<?> getObjectType() {
        return Jsr223FregeObjectPool.class;
    }

    @Override
    public boolean isSingleton() {
        return true;
    }

}
