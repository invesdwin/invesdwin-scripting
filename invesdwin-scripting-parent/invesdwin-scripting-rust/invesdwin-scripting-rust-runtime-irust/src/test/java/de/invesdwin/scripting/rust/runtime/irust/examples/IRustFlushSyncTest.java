// CHECKSTYLE:OFF
package de.invesdwin.scripting.rust.runtime.irust.examples;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

// @NotThreadSafe
public class IRustFlushSyncTest {

    private static boolean stdinReceived = false;
    private static boolean stderrReceived = false;

    public static void main(final String[] args) throws IOException, InterruptedException {
        final List<String> j = new ArrayList<String>();
        j.add("irust");
        //                j.add("--default-config");
        j.add("--bare-repl");
        final ProcessBuilder pbuilder = new ProcessBuilder(j);

        final Process irust = pbuilder.start();
        final InputStream stdin = irust.getInputStream();
        final InputStream stderr = irust.getErrorStream();
        final OutputStream stdout = irust.getOutputStream();

        //        final Thread inputThread = new Thread() {
        //            @Override
        //            public void run() {
        //                try {
        //                    while (true) {
        //                        final byte b = (byte) stdin.read();
        //                        if (b != -1) {
        //                            stdinReceived = true;
        //                            System.out.print((char) b);
        //                        } else {
        //                            TimeUnit.MILLISECONDS.sleep(1);
        //                        }
        //                    }
        //                } catch (final Throwable t) {
        //                    t.printStackTrace();
        //                }
        //            }
        //        };
        //        inputThread.start();

        final Thread errorThread = new Thread() {
            @Override
            public void run() {
                try {
                    while (true) {
                        if (stdin.available() > 0) {
                            final byte b = (byte) stderr.read();
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

        writeCommand(stdin, stdout, ":add serde_json");
        writeCommand(stdin, stdout, "let data = \"Some data!\";");
        writeCommand(stdin, stdout, "std::fs::write(\"/tmp/foo\", data);");
        //statements with a semicolon are added to the internal code buffer
        writeCommand(stdin, stdout, "println!(\"hello\")");
        writeCommand(stdin, stdout, "1+1");
        writeCommand(stdin, stdout, "asdf");
        writeCommand(stdin, stdout, "let mut a = 1; a+=1; println!(\"{}\", a)");
        writeCommand(stdin, stdout, "println!(\"finished\")");
        //does not execute this, instead silently fails

        System.out.println("//waiting 5 seconds for stdin/stderr streams to return output ...");

        try {
            TimeUnit.SECONDS.sleep(5);
        } catch (final InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out
                .println("//received from irust: stdinReceived=" + stdinReceived + " stderrReceived=" + stderrReceived);

        //        writeCommand(stdout, ":exit");
        stdout.flush();

        irust.destroy();

        final int result = irust.waitFor();

        TimeUnit.SECONDS.sleep(1);

        System.out.println("//irust exit code " + result);

        System.exit(0);
    }

    private static String writeCommand(final InputStream stdin, final OutputStream stdout, final String line)
            throws IOException {
        final String command = "IRUST_INPUT_START" + line + "IRUST_INPUT_END";
        System.out.println("//sending command line: " + command.replace("\\", "\\\\"));
        stdout.write(command.getBytes());
        stdout.flush();
        return readResponse(stdin);
    }

    private static String readResponse(final InputStream stdin) {
        final StringBuilder sb = new StringBuilder();
        try {
            while (true) {
                if (stdin.available() > 0) {
                    final byte b = (byte) stdin.read();
                    stdinReceived = true;
                    final char c = (char) b;
                    System.out.print(c);
                    sb.append(c);
                    if (sb.toString().endsWith("IRUST_OUTPUT_END")) {
                        System.out.print("\n");
                        return sb.toString();
                    }
                } else {
                    TimeUnit.MILLISECONDS.sleep(1);
                }
            }
        } catch (final Throwable t) {
            t.printStackTrace();
            return "";
        }
    }

}