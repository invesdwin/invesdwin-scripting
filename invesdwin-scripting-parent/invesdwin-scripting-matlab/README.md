# invesdwin-context-matlab
Integrate Matlab, Octave and Scilab functionality with these modules for the [invesdwin-context](https://github.com/subes/invesdwin-context) module system. All scripting modules provide unified bidirectional communication between Java and the respective scripting language. See test cases for examples on how to implement your script integrations.

## Maven

Releases and snapshots are deployed to this maven repository:
```
https://invesdwin.de/repo/invesdwin-oss-remote/
```

Dependency declaration:
```xml
<dependency>
	<groupId>de.invesdwin</groupId>
	<artifactId>invesdwin-context-matlab-runtime-javaoctave</artifactId>
	<version>1.0.3</version><!---project.version.invesdwin-context-matlab-parent-->
</dependency>
```

## Runtime Integration Modules

We have a few options available for integrating Matlab/Octave:
- **invesdwin-context-matlab-runtime-matconsolectl**: Via [MatConsoleCtl](https://github.com/diffplug/matconsolectl) we are able to run our Matlab scripts in the official [Matlab](https://en.wikipedia.org/wiki/MATLAB) environment. The integration works by remote controlling the Matlab command line client and transferring data efficiently over [RMI](https://en.wikipedia.org/wiki/Java_remote_method_invocation). Multiple instances of Matlab are pooled for performance reasons and to allow parallel computation. You can change the path to the executable via the following system property:
```properties
de.invesdwin.context.matlab.runtime.matconsolectl.MatConsoleCtlProperties.MATLAB_COMMAND=matlab
```
- **invesdwin-context-matlab-runtime-javaoctave**: Via [JavaOctave](https://github.com/prateek/javaoctave) we are able to run our Matlab scripts in [Octave](https://www.gnu.org/software/octave/), which is an open source alternative implementation for the Matlab language. The integration works by remote controlling the Octave command line client and transferring data via files. Multiple instances of Octave are pooled for performance reasons and to allow parallel computation. You can change the path to the executable via the following system property:
```properties
de.invesdwin.context.matlab.runtime.javaoctave.JavaOctaveProperties.OCTAVE_COMMAND=octave
```
- **invesdwin-context-matlab-runtime-javasci**: Via [javasci](https://help.scilab.org/docs/6.0.0/en_US/javasci.html) we are able to run scripts in [Scilab](http://www.scilab.org/), which is an open source mathematical programming language implementation similar to Matlab. Matlab scripts need to be converted to Scilab scripts manually, though you can use `MFileToSciScriptTask` as a wrapper around the [mfile2sci](https://help.scilab.org/docs/6.0.0/en_US/mfile2sci.html) function to let Scilab do most of the conversion and tell you where your help is required to finish the conversion. The integration works by loading the Scilab native libraries into the process. Since this javasci supports only one Scilab session, the integration is synchronized for single thread usage only. To use mutliple threads, you have to spawn additional java processes for your tasks. This module provides the following configuration options as system properties:
```properties
# specify where the libjavasci2.so and libscilab.so resides on your computer (which you might normally add to java.library.path manually, though comma separated here)
de.invesdwin.context.matlab.runtime.javasci.JavasciProperties.JAVASCI_LIBRARY_PATHS=/usr/lib/jni/,/usr/lib/scilab/
de.invesdwin.context.matlab.runtime.javasci.JavasciProperties.SCILAB_PATH=/usr/share/scilab/
```

With this you can switch easily between Matlab/Octave for your scripts to test for interoperability and decide which implementation provides the best performance for your use case. Notably Matlab takes a long time to start up but has efficient data transfer, while Octave is faster to start up but the data transfer is less efficient. Scilab might be interesting to access a wider pool of mathematical tools.

Also Octave is free while Matlab requires a paid license. Even though Octave is licensed as GPL, usage and integration with it can happen without your application falling under the GPL. Though please consider that when using modules for Octave/Matlab that are licensed under the GPL, you might have to make your own scripts available under the GPL too. Since Scilab and javasci are both licensed under the GPL, `invesdwin-context-matlab-runtime-javasci` also had to be put under the GPL, so please be aware of this fact when using the Scilab integration. For a more elaborate license discussion, see the documentation of [invesdwin-context-r](https://github.com/subes/invesdwin-context-r) which faces the same topic.

## Example Code

This is a minimal example of the famous `Hello World!` as a script:

```java
final AScriptTaskMatlab<String> script = new AScriptTaskMatlab<String>() {

    @Override
    public void populateInputs(final IScriptTaskInputs inputs) {
	inputs.putString("hello", "World");
    }

    @Override
    public void executeScript(final IScriptTaskEngine engine) {
	//execute this script inline:
	engine.eval("world = strcat({'Hello '}, hello, '!')");
	//or run it from a file:
	//engine.eval(new ClassPathResource("HelloWorldScript.m", getClass()));
    }

    @Override
    public String extractResults(final IScriptTaskResults results) {
        return results.getString("world");
    }
};
final String result = script.run(); //optionally pass a specific runner as an argument here
Assertions.assertThat(result).isEqualTo("Hello World!");
```

For more elaborate examples of the Matlab/Octave script integration, have a look at the test cases in `invesdwin-context-matlab-runtime-contract` which are executed in each individual runtime module test suite.

## Avoiding Bootstrap

If you want to use this project without the overhead of having to initialize a [invesdwin-context](https://github.com/subes/invesdwin-context) bootstrap with its spring-context and module configuration, you can disable the bootstrap with the following code before using any scripts:

```java
de.invesdwin.context.PlatformInitializerProperties.setAllowed(false);
```

The above configuration options for the invidiual runtimes can still be provided by setting system properties before calling any script. An example for all of this can be found at: [ScriptingWithoutBootstrapMain.java](https://github.com/subes/invesdwin-context/blob/master/tests/otherproject-noparent-bom-test/src/main/java/com/otherproject/scripting/ScriptingWithoutBootstrapMain.java)

## Recommended Editor

For working with Matlab we recommend using the default user interface of Matlab. If you want to run your scripts from your main application, you can do this easily with `invesdwin-context-python-runtime-matconsolectl` (add this module as a `test` scope dependency) during development (you also need to add a dependecy to the type `test-jar` of `invesdwin-context-matlab-runtime-contract` for the log level to get activated, or alternatively change the log level of `de.invesdwin.context.matlab.runtime.contract.IScriptTaskRunnerMatlab` to `DEBUG` on your own). The actual deployment distribution can choose a different runtime then as a hard dependency.

## More Programming Languages

Similar integration modules like this one also exist for the following other programming languages: 

- **R Modules**: Scripting with R
	- https://github.com/invesdwin/invesdwin-context-r 
- **Python Modules**: Scripting with Python
	- https://github.com/invesdwin/invesdwin-context-python
- **Julia Modules**: Scripting with Julia
	- https://github.com/invesdwin/invesdwin-context-julia
- **JVM Languages Modules**: Scripting with JVM Languages
	- https://github.com/invesdwin/invesdwin-context#scripting-modules-for-jvm-languages

## Support

If you need further assistance or have some ideas for improvements and don't want to create an issue here on github, feel free to start a discussion in our [invesdwin-platform](https://groups.google.com/forum/#!forum/invesdwin-platform) mailing list.
