// CHECKSTYLE:OFF
package de.invesdwin.scripting.rust.runtime.irust.examples;

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

public class IRustPtyFlushTest {

    private static boolean stdinReceived = false;
    private static boolean stderrReceived = false;

    public static void main(final String[] args) throws IOException, InterruptedException {
        final List<String> j = new ArrayList<String>();
        j.add("irust");
        //        j.add("--default-config");
        j.add("--bare-repl");
        //        j.add("scilab");
        //        j.add("-nwni");
        final PtyProcessBuilder pbuilder = new PtyProcessBuilder();
        pbuilder.setInitialColumns(80);
        pbuilder.setInitialRows(25);
        pbuilder.setCommand(j.toArray(new String[0]));
        //
        final Map<String, String> env = new HashMap<>(System.getenv());
        if (!env.containsKey("TERM")) {
            env.put("TERM", "xterm"); //makes sure that errors are printed as output
        }
        env.put("NO_COLOR", "1"); //controls errors to include color or not
        pbuilder.setEnvironment(env);

        final PtyProcess irust = pbuilder.start();

        final InputStream stdin = irust.getInputStream();
        final InputStream stderr = irust.getErrorStream();
        final OutputStream stdout = irust.getOutputStream();

        final Thread inputThread = new Thread() {
            @Override
            public void run() {
                final StringBuilder sb = new StringBuilder();
                try {
                    while (true) {
                        final byte b = (byte) stdin.read();
                        if (b != -1) {
                            stdinReceived = true;
                            final char c = (char) b;
                            System.out.print(c);
                            sb.append(c);
                            if (sb.toString().endsWith("IRUST_OUTPUT_END")) {
                                sb.setLength(0);
                                System.out.print("\n");
                            }
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

        writeCommand(stdout, "use std::fs;");
        writeCommand(stdout, "let data = \"Some data!\";");
        writeCommand(stdout, "fs::write(\"/tmp/foo\", data);");

        writeCommand(stdout, "println!(\"hello\");");
        writeCommand(stdout, "1+1");
        writeCommand(stdout, "asdf");
        stdout.flush();

        System.out.println("//waiting 5 seconds for stdin/stderr streams to return output ...");

        try {
            TimeUnit.SECONDS.sleep(5);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out.println();
        System.out
                .println("//received from irust: stdinReceived=" + stdinReceived + " stderrReceived=" + stderrReceived);

        writeCommand(stdout, ":exit");
        stdout.flush();

        irust.destroy();

        final int result = irust.waitFor();

        TimeUnit.SECONDS.sleep(1);

        System.out.println("//irust exit code " + result);

        System.exit(0);
    }

    private static void writeCommand(final OutputStream stdout, final String line) throws IOException {
        final String command = "IRUST_INPUT_START" + line + "IRUST_INPUT_END\n\r";
        System.out.println("//sending command line: " + command.replace("\\", "\\\\"));
        stdout.write(command.getBytes());
    }

}
