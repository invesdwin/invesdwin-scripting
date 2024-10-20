package de.invesdwin.context.r.optimalf;

import javax.annotation.concurrent.Immutable;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({ OptimalfScriptTaskTest.class })
@Immutable
public class OptimalfTestSuite {

}
