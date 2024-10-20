package de.invesdwin.context.matlab.runtime.javasci;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

import javax.annotation.concurrent.NotThreadSafe;

import org.apache.commons.io.IOUtils;

import de.invesdwin.context.ContextProperties;
import de.invesdwin.context.integration.script.IScriptTaskEngine;
import de.invesdwin.context.integration.script.IScriptTaskInputs;
import de.invesdwin.context.integration.script.IScriptTaskResults;
import de.invesdwin.context.matlab.runtime.contract.AScriptTaskMatlab;
import de.invesdwin.util.lang.Files;
import de.invesdwin.util.lang.string.UniqueNameGenerator;

/**
 * Can be used to convert matlab files to scilab, or at least to get a starting point for further conversion.
 */
@NotThreadSafe
public class MFileToSciScriptTask extends AScriptTaskMatlab<String> {

    private static final UniqueNameGenerator UNIQUE_NAME_GENERATOR = new UniqueNameGenerator() {
        @Override
        protected long getInitialValue() {
            return 1;
        }
    };
    private static final File BASE_FOLDER = new File(ContextProperties.TEMP_DIRECTORY,
            MFileToSciScriptTask.class.getSimpleName());

    private final InputStream mfileIn;
    private String outputStr;

    public MFileToSciScriptTask(final InputStream mfileIn) {
        this.mfileIn = mfileIn;
    }

    @Override
    public void populateInputs(final IScriptTaskInputs inputs) {}

    @Override
    public void executeScript(final IScriptTaskEngine engine) {
        try {
            final String folderName = UNIQUE_NAME_GENERATOR.get("conversion");
            final File folder = new File(BASE_FOLDER, folderName);
            Files.forceMkdir(folder);
            final File inputFile = new File(folder, "input.m");
            final FileOutputStream fis = new FileOutputStream(inputFile);
            IOUtils.copy(mfileIn, fis);
            fis.close();
            engine.eval("mfile2sci(\"" + inputFile.getAbsolutePath() + "\", \"" + folder.getAbsolutePath()
                    + "\", %f, %T, 1, %T)");
            final File outputFile = new File(folder, "input.sci");
            outputStr = Files.readFileToString(outputFile, Charset.defaultCharset());
            Files.deleteQuietly(folder);
        } catch (final IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String extractResults(final IScriptTaskResults results) {
        return outputStr;
    }

}
