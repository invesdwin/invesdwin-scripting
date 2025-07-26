// CHECKSTYLE:OFF
package de.invesdwin.scripting.rust.runtime.irust;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class IRustFlushTest {

    private static boolean stdinReceived = false;
    private static boolean stderrReceived = false;

    public static void main(final String[] args) throws IOException {

        final List<String> j = new ArrayList<String>();
        j.add("irust");
        final ProcessBuilder pbuilder = new ProcessBuilder(j);

        final Process irust = pbuilder.start();
        final InputStream stdin = irust.getInputStream();
        final InputStream stderr = irust.getErrorStream();
        final OutputStream stdout = irust.getOutputStream();

        final Thread inputThread = new Thread() {
            @Override
            public void run() {
                try {
                    while (true) {
                        if (stdin.available() > 0) {
                            stdinReceived = true;
                            final byte b = (byte) stdin.read();
                            System.out.println(b + " | " + (char) b);
                        } else {
                            TimeUnit.MILLISECONDS.sleep(1);
                        }
                    }
                } catch (final Throwable t) {
                    t.printStackTrace();
                }
            }
        };
        inputThread.start();

        final Thread errorThread = new Thread() {
            @Override
            public void run() {
                try {
                    while (true) {
                        if (stderr.available() > 0) {
                            stderrReceived = true;
                            final byte b = (byte) stderr.read();
                            System.err.println(b + " | " + (char) b);
                        } else {
                            TimeUnit.MILLISECONDS.sleep(1);
                        }
                    }
                } catch (final Throwable t) {
                    t.printStackTrace();
                }
            }
        };
        errorThread.start();

        try {
            TimeUnit.SECONDS.sleep(5);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }

        System.out.println("sending commands ...");

        System.out.println("sending command line : println!(\"hello\");\\n");
        stdout.write("println!(\"hello\");\n".getBytes());
        System.out.println("sending command line: 1+1\\n");
        stdout.write("1+1\n".getBytes());
        stdout.flush();

        System.out.println("waiting 5 seconds for stdin/stderr streams to return output ...");

        try {
            TimeUnit.SECONDS.sleep(5);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out.println("received from irust: stdinReceived=" + stdinReceived + " stderrReceived=" + stderrReceived);
        System.exit(0);
    }

}
