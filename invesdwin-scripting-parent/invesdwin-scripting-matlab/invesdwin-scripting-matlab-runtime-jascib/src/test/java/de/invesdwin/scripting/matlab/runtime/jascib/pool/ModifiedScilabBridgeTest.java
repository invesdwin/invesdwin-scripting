package de.invesdwin.scripting.matlab.runtime.jascib.pool;

import javax.annotation.concurrent.NotThreadSafe;

import org.junit.jupiter.api.Test;

@NotThreadSafe
public class ModifiedScilabBridgeTest {

	@Test
	public void testBlacklist() {
		final byte[] bytes = "[?25h[?25l".getBytes();
		for (int i = 0; i < bytes.length; i++) {
			final byte b = bytes[i];
			// CHECKSTYLE:OFF
			System.out.println(b + " | " + (char) b);
			// CHECKSTYLE:ON
		}
	}

}
