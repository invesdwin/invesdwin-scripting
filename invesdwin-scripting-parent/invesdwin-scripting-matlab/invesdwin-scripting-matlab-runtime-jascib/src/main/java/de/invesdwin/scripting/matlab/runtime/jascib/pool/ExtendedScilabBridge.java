package de.invesdwin.scripting.matlab.runtime.jascib.pool;

import java.io.IOException;

import javax.annotation.concurrent.NotThreadSafe;

@NotThreadSafe
public class ExtendedScilabBridge extends ModifiedScilabBridge {

	public static final String CLEANUP_SCRIPT = "clear; clc";

	public ExtendedScilabBridge() {
		super();
	}

	@Override
	public void open() throws IOException {
		super.open();
	}

	public void reset() throws IOException {
		eval(CLEANUP_SCRIPT);
		getErrWatcher().clearLog();
	}

}
