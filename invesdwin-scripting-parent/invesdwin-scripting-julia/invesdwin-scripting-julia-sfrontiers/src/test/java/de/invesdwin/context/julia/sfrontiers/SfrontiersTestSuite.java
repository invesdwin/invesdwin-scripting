package de.invesdwin.context.julia.sfrontiers;

import javax.annotation.concurrent.Immutable;

import org.junit.platform.suite.api.SelectClasses;
import org.junit.platform.suite.api.Suite;

@Suite
@SelectClasses({ SfrontiersScriptTaskTest.class })
@Immutable
public class SfrontiersTestSuite {

}
