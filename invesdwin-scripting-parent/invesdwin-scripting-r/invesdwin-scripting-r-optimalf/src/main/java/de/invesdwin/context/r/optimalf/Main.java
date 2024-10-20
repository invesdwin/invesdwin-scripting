package de.invesdwin.context.r.optimalf;

import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.concurrent.Immutable;

import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import de.invesdwin.context.PlatformInitializerProperties;
import de.invesdwin.context.beans.init.AMain;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.string.Strings;

@Immutable
public class Main extends AMain {

    static {
        PlatformInitializerProperties.setAllowed(false);
    }

    @Option(required = true, name = "-i", aliases = "--input", usage = "The CSV file containing one or more strategy per line with its trades (holding period returns) being comma separated", metaVar = "<some/directory/file.csv>")
    private File input;

    @Option(required = true, name = "-o", aliases = "--output", usage = "This is the CSV file that will be written as a result, containing the optimal-f values for each strategy in a separate line", metaVar = "<some/directory/file.csv>")
    private File output;

    protected Main(final String[] args) {
        super(args, false);
    }

    public static void main(final String[] args) {
        new Main(args).run();
    }

    @Override
    protected void startApplication(final CmdLineParser parser) throws Exception {
        final List<String> lines = Files.readLines(input, Charset.defaultCharset());
        final List<List<Double>> tradesPerStrategy = new ArrayList<List<Double>>(lines.size());
        for (final String line : lines) {
            final String[] tradeStrs = Strings.splitPreserveAllTokens(line, ",");
            final List<Double> trades = new ArrayList<Double>(tradeStrs.length);
            for (final String tradeStr : tradeStrs) {
                trades.add(Double.parseDouble(tradeStr));
            }
            tradesPerStrategy.add(trades);
        }
        final List<Double> optimalFs = new OptimalfScriptTask(tradesPerStrategy).run();
        final StringBuilder optimalFsStr = new StringBuilder();
        for (final Double optimalF : optimalFs) {
            optimalFsStr.append(optimalF);
            optimalFsStr.append("\n");
        }
        Files.writeStringToFile(output, optimalFsStr.toString(), Charset.defaultCharset());
    }

}
