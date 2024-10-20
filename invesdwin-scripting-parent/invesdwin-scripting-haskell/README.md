# invesdwin-context-haskell
Integrate Haskell functionality with these modules for the [invesdwin-context](https://github.com/subes/invesdwin-context) module system. All integration modules provide unified bidirectional communication between Java and Haskell. That way you can switch the Haskell provider without having to change your script implementation. See test cases for examples on how to implement your script integrations.

## Maven

Releases and snapshots are deployed to this maven repository:
```
https://invesdwin.de/repo/invesdwin-oss-remote/
```

Dependency declaration:
```xml
<dependency>
	<groupId>de.invesdwin</groupId>
	<artifactId>invesdwin-context-haskell-runtime-ghci</artifactId>
	<version>1.0.4-SNAPSHOT</version><!---project.version.invesdwin-context-haskell-parent-->
</dependency>
```

## Runtime Integration Modules

We have a few options available for integrating Haskell:
- **invesdwin-context-haskell-runtime-ghci**: 
- **invesdwin-context-haskell-runtime-eta**: 
- **invesdwin-context-haskell-runtime-frege**: 

You are free to choose which integration method you prefer by selecting the appropriate runtime module as a dependency for your application. The `invesdwin-context-haskell-runtime-contract` module defines interfaces for integrating your Haskell scripts in a way that works with all of the above runtime modules. So you have the benefit of being able to write your Haskell scripts once and easily test against different runtimes in order to: 
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

For more elaborate examples of the Haskell script integration, have a look at the test cases in `invesdwin-context-haskell-runtime-contract` which are executed in each individual runtime module test suite.


## Avoiding Bootstrap

If you want to use this project without the overhead of having to initialize a [invesdwin-context](https://github.com/invesdwin/invesdwin-context) bootstrap with its spring-context and module configuration, you can disable the bootstrap with the following code before using any scripts:

```java
de.invesdwin.context.PlatformInitializerProperties.setAllowed(false);
```

The above configuration options for the invidiual runtimes can still be provided by setting system properties before calling any script. An example for all of this can be found at: [ScriptingWithoutBootstrapMain.java](https://github.com/invesdwin/invesdwin-context/blob/master/tests/otherproject-noparent-bom-test/src/main/java/com/otherproject/scripting/ScriptingWithoutBootstrapMain.java)

## More Programming Languages

Similar integration modules like this one also exist for the following other programming languages: 

- **R Modules**: Scripting with R
	- https://github.com/invesdwin/invesdwin-context-r 
- **Julia Modules**: Scripting with Julia
	- https://github.com/invesdwin/invesdwin-context-julia
- **Python Modules**: Scripting with Python
	- https://github.com/invesdwin/invesdwin-context-python
- **Matlab/Octave/Scilab Modules**: Scripting with Matlab, Octave and Scilab
	- https://github.com/invesdwin/invesdwin-context-matlab
- **JVM Languages Modules**: Scripting with JVM Languages
	- https://github.com/invesdwin/invesdwin-context#scripting-modules-for-jvm-languages


## Support

If you need further assistance or have some ideas for improvements and don't want to create an issue here on github, feel free to start a discussion in our [invesdwin-platform](https://groups.google.com/forum/#!forum/invesdwin-platform) mailing list.
