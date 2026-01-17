# invesdwin-scripting-haskell
Integrate Haskell functionality with these modules for [invesdwin-scripting](https://github.com/invesdwin/invesdwin-scripting). All integration modules provide unified bidirectional communication between Java and Haskell. That way you can switch the Haskell provider without having to change your script implementation. See test cases for examples on how to implement your script integrations.

## Maven

Releases and snapshots are deployed to this maven repository:
```
https://invesdwin.de/repo/invesdwin-oss-remote/
```

Dependency declaration:
```xml
<dependency>
	<groupId>de.invesdwin</groupId>
	<artifactId>invesdwin-scripting-haskell-runtime-ghci</artifactId>
	<version>1.0.4-SNAPSHOT</version><!---project.version.invesdwin-scripting-parent-->
</dependency>
```

## Runtime Integration Modules

We have a few options available for integrating Haskell:
- **invesdwin-scripting-haskell-runtime-ghci**: This integrates [GHCI](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html) via a forked version of [Jajub](https://github.com/org-arl/jajub/issues/2) to make it significantly faster, make error handling better, improve robustness and make it compatible with haskell. It talks to the Haskell process via pipes. Errors are detected by checking for specific protocol messages and by parsing stderr for messages. Haskell instances are pooled which works well for parallelization. This module provides the following configuration options as system properties:
```properties
# you can switch to a different GHCI installation by defining an absolute path here
de.invesdwin.scripting.haskell.runtime.ghci.GhciProperties.GHCI_COMMAND=ghci
```
- **invesdwin-scripting-haskell-runtime-frege**: This integrates [Frege-Interpreter](https://github.com/Frege/frege-interpreter) and [Frege-REPL](https://github.com/Frege/frege-repl). This is an alternative to the GHCI integration that runs directly within the JVM, though the Haskell dialect is slightly different and the simplified callbacks are invoked differently as demonstrated in the respective test cases. Use `IO.performUnsafe ( putStrLn ... )` (which is similar to `unsafePerformIO` in GHCI) to force eager evaluation so that output is printed to the console when running inside the JSR-223 variant of Frege. The REPL variant (similarly to the GHCI REPL) does not require this workaround. Frege-REPL (which forks a separate JVM and communicates via pipes) is slower than Frege-Interpreter (running within the existing JVM via JSR-223; about 2.67 times as fast compared to Frege-REPL when measured on the test cases) and both are significantly slower than the GHCI integration (about 11 times as fast compared to Frege-Interpreter and 29.5 times as fast compared to Frege-REPL when measured on the test cases) . Frege can not use GHCI modules, instead one can import Java libraries directly via native bindings. These can be generated with [Frege-Native-Gen](https://github.com/Frege/frege-native-gen), though common AI tools are relatively good at generating those Frege native bindings when given a Java class as well. This module provides the following configuration options as system properties:
```properties
# uncomment this line to use a forked repl process instead of running frege inside of the current JVM (default is false)
#de.invesdwin.scripting.haskell.runtime.frege.FregeProperties.FORKED_REPL_PROCESS=true
```

You are free to choose which integration method you prefer by selecting the appropriate runtime module as a dependency for your application. The `invesdwin-scripting-haskell-runtime-contract` module defines interfaces for integrating your Haskell scripts in a way that works with all of the above runtime modules. So you have the benefit of being able to write your Haskell scripts once and easily test against different runtimes in order to: 
- measure the performance impact of the different runtime solutions
- gain flexibility in various deployment scenarios

## Example Code

This is a minimal example of the famous `Hello World!` as a script:

```java
final AScriptTaskHaskell<String> script = new AScriptTaskHaskell<String>() {

    @Override
    public void populateInputs(final IScriptTaskInputs inputs) {
	inputs.putString("hello", "World");
    }

    @Override
    public void executeScript(final IScriptTaskEngine engine) {
	//execute this script inline:
	engine.eval("world = \"Hello \" ++ hello ++ \"!\"");
	//or run it from a file:
	//engine.eval(new ClassPathResource("HelloWorldScript.hs", getClass()));
    }

    @Override
    public String extractResults(final IScriptTaskResults results) {
        return results.getString("world");
    }
};
final String result = script.run(); //optionally pass a specific runner as an argument here
Assertions.assertThat(result).isEqualTo("Hello World!");
```

For more elaborate examples of the Haskell script integration, have a look at the test cases in `invesdwin-scripting-haskell-runtime-contract` which are executed in each individual runtime module test suite.

## More Documentation and Programming Languages

Further documentation and similar integration modules like this one also can be found here: https://github.com/invesdwin/invesdwin-scripting
