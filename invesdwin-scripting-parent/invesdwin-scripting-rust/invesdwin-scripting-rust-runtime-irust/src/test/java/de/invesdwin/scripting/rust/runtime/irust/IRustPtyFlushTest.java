// CHECKSTYLE:OFF
package de.invesdwin.scripting.rust.runtime.irust;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.pty4j.PtyProcess;
import com.pty4j.PtyProcessBuilder;

import de.invesdwin.util.lang.string.Strings;

public class IRustPtyFlushTest {

    private static boolean stdinReceived = false;
    private static boolean stderrReceived = false;

    public static void main(final String[] args) throws IOException, InterruptedException {
        final List<String> j = new ArrayList<String>();
        j.add("irust");
        //        j.add("--default-config");
        //        j.add("scilab");
        //        j.add("-nwni");
        final PtyProcessBuilder pbuilder = new PtyProcessBuilder();
        pbuilder.setInitialColumns(80);
        pbuilder.setInitialRows(25);
        pbuilder.setCommand(j.toArray(Strings.EMPTY_ARRAY));

        final Map<String, String> env = new HashMap<>(System.getenv());
        if (!env.containsKey("TERM")) {
            env.put("TERM", "xterm");
        }
        env.put("NO_COLOR", "1");
        pbuilder.setEnvironment(env);

        final PtyProcess irust = pbuilder.start();

        final InputStream stdin = irust.getInputStream();
        final InputStream stderr = irust.getErrorStream();
        final OutputStream stdout = irust.getOutputStream();

        final Thread inputThread = new Thread() {
            @Override
            public void run() {
                try {
                    while (true) {
                        final byte b = (byte) stdin.read();
                        if (b != -1) {
                            stdinReceived = true;
                            System.out.print((char) b);
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
                        final byte b = (byte) stderr.read();
                        if (b != -1) {
                            stderrReceived = true;
                            System.err.print((char) b);
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

        System.out.println("//sending commands ...");

        System.out.println("//sending command line : println!(\"hello\"); \\n\\r");
        stdout.write("println!(\"hello\");\n\r".getBytes());
        System.out.println("//sending command line: 1+1\\n\\r");
        stdout.write("1+1\n\r".getBytes());
        stdout.flush();

        System.out.println("//waiting 5 seconds for stdin/stderr streams to return output ...");

        try {
            TimeUnit.SECONDS.sleep(5);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out
                .println("//received from irust: stdinReceived=" + stdinReceived + " stderrReceived=" + stderrReceived);

        stdout.write(":exit\n\r".getBytes());
        stdout.flush();

        irust.destroy();

        final int result = irust.waitFor();
        System.out.println("//irust exit code " + result);

        System.exit(0);
    }

}
