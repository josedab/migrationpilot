/**
 * @migrationpilot/regression
 * 
 * Automated Regression Suite Management.
 * Track, version, and maintain generated test suites over time.
 */

// ============================================================================
// Types
// ============================================================================

export interface TestSuite {
  id: string;
  projectId: string;
  name: string;
  version: string;
  status: 'active' | 'deprecated' | 'archived';
  tests: TestCase[];
  coverage: CoverageMetrics;
  createdAt: string;
  updatedAt: string;
  history: SuiteHistory[];
}

export interface TestCase {
  id: string;
  name: string;
  description: string;
  type: 'unit' | 'integration' | 'property' | 'equivalence';
  status: 'active' | 'skipped' | 'deprecated';
  sourceRuleId?: string;
  inputs: TestInput[];
  expectedOutput: ExpectedOutput;
  code: string;
  lastResult?: TestResult;
  createdAt: string;
  updatedAt: string;
}

export interface TestInput {
  name: string;
  type: string;
  value: unknown;
  description?: string;
}

export interface ExpectedOutput {
  type: 'exact' | 'pattern' | 'range' | 'property';
  value?: unknown;
  pattern?: string;
  min?: number;
  max?: number;
  property?: string;
  tolerance?: number;
}

export interface TestResult {
  passed: boolean;
  executionTime: number;
  actualOutput: unknown;
  errorMessage?: string;
  timestamp: string;
}

export interface CoverageMetrics {
  rulesTestedCount: number;
  totalRulesCount: number;
  rulesCoverage: number;
  linesCoverage: number;
  branchesCoverage: number;
  edgeCasesCovered: number;
}

export interface SuiteHistory {
  version: string;
  timestamp: string;
  changes: SuiteChange[];
  author: string;
  reason?: string;
}

export interface SuiteChange {
  type: 'add' | 'update' | 'remove' | 'deprecate';
  testId: string;
  testName: string;
  details?: string;
}

export interface RegressionRun {
  id: string;
  suiteId: string;
  suiteVersion: string;
  status: 'pending' | 'running' | 'completed' | 'failed';
  startedAt?: string;
  completedAt?: string;
  results: TestRunResult[];
  summary: RunSummary;
  environment: string;
}

export interface TestRunResult {
  testId: string;
  testName: string;
  passed: boolean;
  duration: number;
  error?: string;
  diff?: string;
}

export interface RunSummary {
  total: number;
  passed: number;
  failed: number;
  skipped: number;
  duration: number;
  passRate: number;
}

// ============================================================================
// Regression Suite Manager
// ============================================================================

export class RegressionSuiteManager {
  private suites: Map<string, TestSuite> = new Map();
  private runs: Map<string, RegressionRun> = new Map();

  /**
   * Create a new test suite
   */
  createSuite(
    projectId: string,
    name: string,
    tests: Omit<TestCase, 'id' | 'status' | 'createdAt' | 'updatedAt'>[]
  ): TestSuite {
    const id = `suite_${Date.now()}`;
    const now = new Date().toISOString();

    const fullTests: TestCase[] = tests.map((t, i) => ({
      ...t,
      id: `test_${id}_${i + 1}`,
      status: 'active' as const,
      createdAt: now,
      updatedAt: now,
    }));

    const suite: TestSuite = {
      id,
      projectId,
      name,
      version: '1.0.0',
      status: 'active',
      tests: fullTests,
      coverage: this.calculateCoverage(fullTests),
      createdAt: now,
      updatedAt: now,
      history: [{
        version: '1.0.0',
        timestamp: now,
        changes: fullTests.map(t => ({
          type: 'add' as const,
          testId: t.id,
          testName: t.name,
          details: 'Initial creation',
        })),
        author: 'system',
        reason: 'Initial suite creation',
      }],
    };

    this.suites.set(id, suite);
    return suite;
  }

  /**
   * Add tests to a suite
   */
  addTests(
    suiteId: string,
    tests: Omit<TestCase, 'id' | 'status' | 'createdAt' | 'updatedAt'>[],
    author: string,
    reason?: string
  ): TestSuite {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    const now = new Date().toISOString();
    const newTests: TestCase[] = tests.map((t, i) => ({
      ...t,
      id: `test_${suiteId}_${suite.tests.length + i + 1}`,
      status: 'active' as const,
      createdAt: now,
      updatedAt: now,
    }));

    const newVersion = this.incrementVersion(suite.version);

    const changes: SuiteChange[] = newTests.map(t => ({
      type: 'add' as const,
      testId: t.id,
      testName: t.name,
      details: reason,
    }));

    const updatedSuite: TestSuite = {
      ...suite,
      version: newVersion,
      tests: [...suite.tests, ...newTests],
      coverage: this.calculateCoverage([...suite.tests, ...newTests]),
      updatedAt: now,
      history: [...suite.history, {
        version: newVersion,
        timestamp: now,
        changes,
        author,
        reason,
      }],
    };

    this.suites.set(suiteId, updatedSuite);
    return updatedSuite;
  }

  /**
   * Update a test case
   */
  updateTest(
    suiteId: string,
    testId: string,
    updates: Partial<TestCase>,
    author: string,
    reason?: string
  ): TestSuite {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    const testIndex = suite.tests.findIndex(t => t.id === testId);
    if (testIndex === -1) {
      throw new Error(`Test not found: ${testId}`);
    }

    const existingTest = suite.tests[testIndex]!;
    const now = new Date().toISOString();
    const updatedTest: TestCase = {
      ...existingTest,
      ...updates,
      id: testId, // Prevent ID changes
      name: updates.name ?? existingTest.name,
      description: updates.description ?? existingTest.description,
      updatedAt: now,
    };

    const newVersion = this.incrementVersion(suite.version);
    const updatedTests = [...suite.tests];
    updatedTests[testIndex] = updatedTest;

    const updatedSuite: TestSuite = {
      ...suite,
      version: newVersion,
      tests: updatedTests,
      coverage: this.calculateCoverage(updatedTests),
      updatedAt: now,
      history: [...suite.history, {
        version: newVersion,
        timestamp: now,
        changes: [{
          type: 'update',
          testId,
          testName: updatedTest.name,
          details: reason,
        }],
        author,
        reason,
      }],
    };

    this.suites.set(suiteId, updatedSuite);
    return updatedSuite;
  }

  /**
   * Deprecate tests when business rules change
   */
  deprecateTestsForRule(
    suiteId: string,
    ruleId: string,
    author: string,
    reason: string
  ): TestSuite {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    const now = new Date().toISOString();
    const changes: SuiteChange[] = [];
    const updatedTests = suite.tests.map(t => {
      if (t.sourceRuleId === ruleId && t.status === 'active') {
        changes.push({
          type: 'deprecate',
          testId: t.id,
          testName: t.name,
          details: reason,
        });
        return { ...t, status: 'deprecated' as const, updatedAt: now };
      }
      return t;
    });

    if (changes.length === 0) {
      return suite;
    }

    const newVersion = this.incrementVersion(suite.version);

    const updatedSuite: TestSuite = {
      ...suite,
      version: newVersion,
      tests: updatedTests,
      coverage: this.calculateCoverage(updatedTests.filter(t => t.status === 'active')),
      updatedAt: now,
      history: [...suite.history, {
        version: newVersion,
        timestamp: now,
        changes,
        author,
        reason,
      }],
    };

    this.suites.set(suiteId, updatedSuite);
    return updatedSuite;
  }

  /**
   * Run regression tests
   */
  async runRegression(
    suiteId: string,
    environment: string
  ): Promise<RegressionRun> {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    const runId = `run_${Date.now()}`;
    const now = new Date().toISOString();

    const run: RegressionRun = {
      id: runId,
      suiteId,
      suiteVersion: suite.version,
      status: 'running',
      startedAt: now,
      results: [],
      summary: {
        total: 0,
        passed: 0,
        failed: 0,
        skipped: 0,
        duration: 0,
        passRate: 0,
      },
      environment,
    };

    this.runs.set(runId, run);

    // Execute tests (simulated)
    const activeTests = suite.tests.filter(t => t.status === 'active');
    const results: TestRunResult[] = [];
    let totalDuration = 0;

    for (const test of activeTests) {
      // Simulate test execution
      const passed = Math.random() > 0.1; // 90% pass rate for demo
      const duration = Math.random() * 100;
      totalDuration += duration;

      results.push({
        testId: test.id,
        testName: test.name,
        passed,
        duration,
        error: passed ? undefined : 'Assertion failed',
      });

      // Update test with last result
      test.lastResult = {
        passed,
        executionTime: duration,
        actualOutput: {},
        errorMessage: passed ? undefined : 'Assertion failed',
        timestamp: new Date().toISOString(),
      };
    }

    const skippedTests = suite.tests.filter(t => t.status !== 'active');
    const passed = results.filter(r => r.passed).length;
    const failed = results.filter(r => !r.passed).length;

    run.status = failed === 0 ? 'completed' : 'failed';
    run.completedAt = new Date().toISOString();
    run.results = results;
    run.summary = {
      total: results.length,
      passed,
      failed,
      skipped: skippedTests.length,
      duration: totalDuration,
      passRate: results.length > 0 ? passed / results.length : 0,
    };

    this.runs.set(runId, run);
    return run;
  }

  /**
   * Get suite history
   */
  getHistory(suiteId: string): SuiteHistory[] {
    const suite = this.suites.get(suiteId);
    return suite?.history || [];
  }

  /**
   * Get suite at specific version
   */
  getSuiteAtVersion(suiteId: string, version: string): TestSuite | null {
    const suite = this.suites.get(suiteId);
    if (!suite) return null;

    const historyIndex = suite.history.findIndex(h => h.version === version);
    if (historyIndex === -1) return null;

    // Reconstruct suite at version (simplified - would need full snapshot in real impl)
    return suite;
  }

  /**
   * Compare two suite versions
   */
  compareSuiteVersions(suiteId: string, v1: string, v2: string): {
    added: string[];
    removed: string[];
    modified: string[];
  } {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    const v1Index = suite.history.findIndex(h => h.version === v1);
    const v2Index = suite.history.findIndex(h => h.version === v2);

    if (v1Index === -1 || v2Index === -1) {
      throw new Error('Version not found');
    }

    const changes = suite.history
      .slice(Math.min(v1Index, v2Index) + 1, Math.max(v1Index, v2Index) + 1)
      .flatMap(h => h.changes);

    return {
      added: changes.filter(c => c.type === 'add').map(c => c.testName),
      removed: changes.filter(c => c.type === 'remove' || c.type === 'deprecate').map(c => c.testName),
      modified: changes.filter(c => c.type === 'update').map(c => c.testName),
    };
  }

  /**
   * Get run history for a suite
   */
  getRunHistory(suiteId: string): RegressionRun[] {
    return Array.from(this.runs.values())
      .filter(r => r.suiteId === suiteId)
      .sort((a, b) => 
        new Date(b.startedAt || 0).getTime() - new Date(a.startedAt || 0).getTime()
      );
  }

  private calculateCoverage(tests: TestCase[]): CoverageMetrics {
    const activeTests = tests.filter(t => t.status === 'active');
    const ruleIds = new Set(activeTests.map(t => t.sourceRuleId).filter(Boolean));

    return {
      rulesTestedCount: ruleIds.size,
      totalRulesCount: ruleIds.size, // Would come from rule repository
      rulesCoverage: ruleIds.size > 0 ? 1.0 : 0,
      linesCoverage: 0.85, // Placeholder
      branchesCoverage: 0.75, // Placeholder
      edgeCasesCovered: activeTests.filter(t => t.type === 'property').length,
    };
  }

  private incrementVersion(version: string): string {
    const [major, minor, patch] = version.split('.').map(Number);
    return `${major}.${minor}.${(patch ?? 0) + 1}`;
  }
}

// ============================================================================
// Exports
// ============================================================================

export { RegressionSuiteManager as default };
