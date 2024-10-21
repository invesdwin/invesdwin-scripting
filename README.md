# invesdwin-scripting

Integrate functionality of scripting languages with these modules for the [invesdwin-context](https://github.com/subes/invesdwin-context) module system. There are multiple runtime modules provided for each scripting language with a unified bidirectional communication capability. That way you can switch the runtime provider without having to change your script implementation. See test cases for examples on how to implement your script integrations.

## Maven

Releases and snapshots are deployed to this maven repository:
```
https://invesdwin.de/repo/invesdwin-oss-remote/
```

Dependency declaration:
```xml
<dependency>
	<groupId>de.invesdwin</groupId>
	<artifactId>invesdwin-scripting-runtime-javascript</artifactId>
	<version>1.0.3</version><!---project.version.invesdwin-scripting-parent-->
</dependency>
```

## Scripting Modules for JVM Languages

This repository contains the following scripting modules for JVM languages:

- **invesdwin-scripting-runtime-clojure**: this is an integration for [Clojure](https://clojure.org/). It contains an improved [JSR-223](https://github.com/cnuernber/dtype-next/issues/52#issuecomment-1013689212) provider that supports isolated namespaces per thread, the ability to remove/clear bindings and reusing compiled scripts.
- **invesdwin-scripting-runtime-groovy**: this is an integration for [Groovy](https://groovy-lang.org/integrating.html). This is the most popular and fastest binding for java like scripts. JSR-223 is also supported. The threadlocal `GroovyProperties.setStrictOverride(true)` or global `GroovyProperties.setDefaultStrict(true)` can be used to switch to a faster but more strict groovy variant that uses `@CompileStatic` and `@TypeChecked` from the outside. Though it is also possible to define such optimized functions [inside of scripts](https://github.com/invesdwin/invesdwin-context/blob/master/invesdwin-context-parent/invesdwin-context-groovy/src/test/java/de/invesdwin/context/groovy/tests/hello/FileStrictHelloWorldScript.groovy).
- **invesdwin-scripting-runtime-beanshell**: this is an integration for [Beanshell](https://github.com/beanshell/beanshell). It provides some simplifications for java based scripts. Support for JSR-223 is also available.
- **invesdwin-scripting-runtime-jshell**: this is an integration for [JShell](https://github.com/dmac100/JShellScriptEngine) via JSR-223. It provides the purest java syntax, but is rather slow. For pure Java the [Janino](http://janino-compiler.github.io/janino/) compiler might be preferable.
- **invesdwin-scripting-runtime-kotlin**: this is an integration for [Kotlin](https://github.com/Kotlin/kotlin-script-examples). It will use [kotlin-main-kts](https://github.com/Kotlin/kotlin-script-examples/blob/master/jvm/main-kts/MainKts.md) if that is present on the classpath. Otherwise it will use the default JSR-223 engine.
- **invesdwin-scripting-runtime-scala**: this is an integration for [Scala](https://www.scala-lang.org/) via JSR-223.
- **invesdwin-scripting-runtime-(jruby|truffleruby)**: this is an integration for [JRuby](https://www.jruby.org/) and [TruffleRuby](https://www.graalvm.org/ruby/) via JSR-223.
- **invesdwin-scripting-runtime-javascript**: this is an integration for Javascript via JSR-223. It uses [GraalJS](https://github.com/oracle/graaljs) if present on the classpath with a fallback to Nashorn (only available until Java 14).
- **invesdwin-scripting-runtime-mvel**: this is an integration for [MVEL](https://github.com/mvel/mvel) via JSR-223.

All scripting modules provide unified bidirectional communication between Java and the respective scripting language. See test cases for examples on how to implement your script integrations.
Examples are available in the respective testcases of the modules or the separate project repos.

## More Programming Languages

There are also more elaborate integrations available for other languages:

- **R Modules**: Scripting with R
	- https://github.com/invesdwin/invesdwin-scripting/tree/main/invesdwin-scripting-parent/invesdwin-scripting-r 
- **Python Modules**: Scripting with Python
	- https://github.com/invesdwin/invesdwin-scripting/tree/main/invesdwin-scripting-parent/invesdwin-scripting-python
- **Matlab/Octave/Scilab Modules**: Scripting with Matlab, Octave and Scilab
	- https://github.com/invesdwin/invesdwin-scripting/tree/main/invesdwin-scripting-parent/invesdwin-scripting-matlab
- **Julia Modules**: Scripting with Julia
	- https://github.com/invesdwin/invesdwin-scripting/tree/main/invesdwin-scripting-parent/invesdwin-scripting-julia
- **Haskell Modules**: Scripting with Haskell
	- https://github.com/invesdwin/invesdwin-scripting/tree/main/invesdwin-scripting-parent/invesdwin-scripting-haskell

## Avoiding Bootstrap

If you want to use this project without the overhead of having to initialize a [invesdwin-context](https://github.com/invesdwin/invesdwin-context) bootstrap with its spring-context and module configuration, you can disable the bootstrap with the following code before using any scripts:

```java
de.invesdwin.context.PlatformInitializerProperties.setAllowed(false);
```

The above configuration options for the invidiual runtimes can still be provided by setting system properties before calling any script. An example for all of this can be found at: [ScriptingWithoutBootstrapMain.java](https://github.com/invesdwin/invesdwin-context/blob/master/tests/otherproject-noparent-bom-test/src/main/java/com/otherproject/scripting/ScriptingWithoutBootstrapMain.java)

## Support

If you need further assistance or have some ideas for improvements and don't want to create an issue here on github, feel free to start a discussion in our [invesdwin-platform](https://groups.google.com/forum/#!forum/invesdwin-platform) mailing list.
