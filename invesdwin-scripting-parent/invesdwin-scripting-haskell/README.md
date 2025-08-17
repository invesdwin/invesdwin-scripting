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
- **invesdwin-scripting-haskell-runtime-ghci**: 
- **invesdwin-scripting-haskell-runtime-frege**: 

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
