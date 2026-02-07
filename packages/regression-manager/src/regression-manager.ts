/**
 * Regression Manager
 * Main service for managing regression test suites
 */

import type { BusinessRule } from '@migrationpilot/core';
import type {
  TestSuite,
  TestCase,
  TestRun,
  TestResult,
  CoverageMetrics,
  GenerationConfig,
  GenerationResult,
  GenerationWarning,
  ComparisonResult,
  TestMatch,
  TestMismatch,
  OutputDifference,
  RunSummary,
} from './types.js';
import { DEFAULT_GENERATION_CONFIG } from './types.js';
import { TestGenerator } from './test-generator.js';
import { CodeGenerator } from './code-generator.js';

export class RegressionManager {
  private suites: Map<string, TestSuite> = new Map();
  private runs: Map<string, TestRun> = new Map();
  private generator: TestGenerator;
  private codeGenerator: CodeGenerator;

  constructor(config: Partial<GenerationConfig> = {}) {
    this.generator = new TestGenerator(config);
    this.codeGenerator = new CodeGenerator(
      config.targetLanguage || 'typescript',
      config.targetFramework || 'vitest'
    );
  }

  /**
   * Create a new test suite for a project
   */
  async createSuite(
    projectId: string,
    name: string,
    rules: BusinessRule[]
  ): Promise<TestSuite> {
    const suiteId = `suite_${projectId}_${Date.now()}`;
    const now = new Date();

    // Generate initial test cases
    const testCases = this.generator.generateTestCases(rules);

    // Calculate initial coverage
    const coverage = this.calculateCoverage(testCases, rules);

    const suite: TestSuite = {
      id: suiteId,
      projectId,
      name,
      description: `Test suite for ${rules.length} business rules`,
      testCases,
      coverage,
      status: 'draft',
      createdAt: now,
      updatedAt: now,
      runHistory: [],
    };

    this.suites.set(suiteId, suite);
    return suite;
  }

  /**
   * Generate test cases for a suite
   */
  async generateTestCases(
    suiteId: string,
    rules: BusinessRule[],
    config: GenerationConfig = DEFAULT_GENERATION_CONFIG
  ): Promise<GenerationResult> {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    // Create generator with custom config
    const generator = new TestGenerator(config);
    const testCases = generator.generateTestCases(rules);

    // Generate code files
    const codeGenerator = new CodeGenerator(config.targetLanguage, config.targetFramework);
    const generatedFiles = codeGenerator.generateCode(testCases);

    // Check for warnings
    const warnings = this.checkGenerationWarnings(rules, testCases);

    // Group test cases by rule
    const testCasesByRule: Record<string, string[]> = {};
    for (const tc of testCases) {
      for (const ruleId of tc.relatedRuleIds) {
        if (!testCasesByRule[ruleId]) {
          testCasesByRule[ruleId] = [];
        }
        testCasesByRule[ruleId].push(tc.id);
      }
    }

    // Update suite
    suite.testCases = testCases;
    suite.coverage = this.calculateCoverage(testCases, rules);
    suite.updatedAt = new Date();

    return {
      suiteId,
      testCasesGenerated: testCases.length,
      testCasesByRule,
      coverage: suite.coverage,
      warnings,
      generatedFiles,
    };
  }

  /**
   * Run a test suite (simulation)
   */
  async runSuite(suiteId: string): Promise<TestRun> {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    const runId = `run_${suiteId}_${Date.now()}`;
    const startTime = new Date();

    // Simulate running tests
    const results: TestResult[] = [];
    let passed = 0;
    let failed = 0;
    let skipped = 0;

    for (const testCase of suite.testCases) {
      // Simulate test execution
      const duration = Math.floor(Math.random() * 100) + 10;
      const success = Math.random() > 0.1; // 90% pass rate

      if (testCase.status === 'deprecated') {
        skipped++;
        results.push({
          testCaseId: testCase.id,
          status: 'skipped',
          duration,
        });
      } else if (success) {
        passed++;
        results.push({
          testCaseId: testCase.id,
          status: 'passed',
          duration,
          actualOutputs: this.generateActualOutputs(testCase),
        });
      } else {
        failed++;
        results.push({
          testCaseId: testCase.id,
          status: 'failed',
          duration,
          actualOutputs: this.generateIncorrectOutputs(testCase),
          error: {
            message: 'Assertion failed',
            type: 'AssertionError',
            expected: testCase.expectedOutputs[0]?.value,
            actual: 'incorrect_value',
          },
        });
      }
    }

    const completedAt = new Date();
    const totalDuration = completedAt.getTime() - startTime.getTime();

    const summary: RunSummary = {
      total: suite.testCases.length,
      passed,
      failed,
      skipped,
      error: 0,
      duration: totalDuration,
      passRate: suite.testCases.length > 0 ? (passed / suite.testCases.length) * 100 : 0,
    };

    const run: TestRun = {
      id: runId,
      suiteId,
      startedAt: startTime,
      completedAt,
      status: 'completed',
      results,
      summary,
      environment: {
        platform: 'node',
        runtime: 'node',
        runtimeVersion: '20.0.0',
        testFramework: 'vitest',
        testFrameworkVersion: '1.2.0',
      },
    };

    this.runs.set(runId, run);
    suite.runHistory.push(run);

    return run;
  }

  /**
   * Compare two test runs
   */
  async compareRuns(legacyRunId: string, modernRunId: string): Promise<ComparisonResult> {
    const legacyRun = this.runs.get(legacyRunId);
    const modernRun = this.runs.get(modernRunId);

    if (!legacyRun || !modernRun) {
      throw new Error('One or both runs not found');
    }

    const matches: TestMatch[] = [];
    const mismatches: TestMismatch[] = [];

    // Create maps for quick lookup
    const legacyResults = new Map(legacyRun.results.map(r => [r.testCaseId, r]));
    const modernResults = new Map(modernRun.results.map(r => [r.testCaseId, r]));

    // Compare results
    for (const [testCaseId, legacyResult] of legacyResults) {
      const modernResult = modernResults.get(testCaseId);
      if (!modernResult) continue;

      // Check if outputs match
      const differences = this.findOutputDifferences(legacyResult, modernResult);

      if (differences.length === 0) {
        matches.push({ testCaseId, legacyResult, modernResult });
      } else {
        const severity = this.calculateMismatchSeverity(differences);
        mismatches.push({
          testCaseId,
          legacyResult,
          modernResult,
          differences,
          severity,
        });
      }
    }

    const matchRate = matches.length + mismatches.length > 0
      ? (matches.length / (matches.length + mismatches.length)) * 100
      : 100;

    return {
      id: `comparison_${Date.now()}`,
      legacyRunId,
      modernRunId,
      timestamp: new Date(),
      matches,
      mismatches,
      summary: {
        totalTests: matches.length + mismatches.length,
        matchingTests: matches.length,
        mismatchingTests: mismatches.length,
        matchRate,
        criticalMismatches: mismatches.filter(m => m.severity === 'critical').length,
        majorMismatches: mismatches.filter(m => m.severity === 'major').length,
        minorMismatches: mismatches.filter(m => m.severity === 'minor').length,
      },
    };
  }

  /**
   * Get coverage metrics for a suite
   */
  getCoverage(suiteId: string): CoverageMetrics {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }
    return suite.coverage;
  }

  /**
   * Get a test suite by ID
   */
  getSuite(suiteId: string): TestSuite | undefined {
    return this.suites.get(suiteId);
  }

  /**
   * Get a test run by ID
   */
  getRun(runId: string): TestRun | undefined {
    return this.runs.get(runId);
  }

  /**
   * List all suites for a project
   */
  listSuites(projectId: string): TestSuite[] {
    return Array.from(this.suites.values())
      .filter(s => s.projectId === projectId);
  }

  /**
   * Export suite in various formats
   */
  exportSuite(suiteId: string, format: 'json' | 'yaml' | 'code'): string {
    const suite = this.suites.get(suiteId);
    if (!suite) {
      throw new Error(`Suite not found: ${suiteId}`);
    }

    switch (format) {
      case 'json':
        return JSON.stringify(suite, null, 2);
      case 'yaml':
        return this.toYaml(suite);
      case 'code':
        const files = this.codeGenerator.generateCode(suite.testCases);
        return files.map(f => `// === ${f.path} ===\n${f.content}`).join('\n\n');
      default:
        return JSON.stringify(suite, null, 2);
    }
  }

  // ============================================================================
  // PRIVATE METHODS
  // ============================================================================

  private calculateCoverage(testCases: TestCase[], rules: BusinessRule[]): CoverageMetrics {
    const coveredRuleIds = new Set<string>();
    const coveredEdgeCases = new Set<string>();

    for (const tc of testCases) {
      for (const ruleId of tc.relatedRuleIds) {
        coveredRuleIds.add(ruleId);
      }
      // Track edge case coverage by tags
      for (const tag of tc.tags) {
        if (tag.startsWith('edge-case')) {
          coveredEdgeCases.add(`${tc.relatedRuleIds[0]}_${tag}`);
        }
      }
    }

    const uncoveredRules = rules
      .filter(r => !coveredRuleIds.has(r.id))
      .map(r => r.id);

    // Count total edge cases
    let totalEdgeCases = 0;
    for (const rule of rules) {
      totalEdgeCases += (rule.edgeCases?.length || 0);
    }

    // Extract boundary conditions from test cases
    const boundaryConditions = testCases
      .filter(tc => tc.type === 'boundary')
      .map(tc => ({
        ruleId: tc.relatedRuleIds[0] || '',
        variable: tc.tags.find(t => !['boundary', tc.relatedRuleIds[0]].includes(t)) || '',
        boundary: '',
        coveredByTestIds: [tc.id],
      }));

    // Extract equivalence classes
    const equivalenceClasses = testCases
      .filter(tc => tc.type === 'equivalence')
      .map(tc => ({
        ruleId: tc.relatedRuleIds[0] || '',
        variable: '',
        className: tc.tags.find(t => t !== 'equivalence' && !tc.relatedRuleIds.includes(t)) || '',
        values: [] as unknown[],
        coveredByTestIds: [tc.id],
      }));

    return {
      rulesTotal: rules.length,
      rulesCovered: coveredRuleIds.size,
      rulesUncovered: uncoveredRules,
      rulesCoveragePercent: rules.length > 0
        ? (coveredRuleIds.size / rules.length) * 100
        : 100,
      edgeCasesTotal: totalEdgeCases,
      edgeCasesCovered: coveredEdgeCases.size,
      boundaryConditions,
      equivalenceClasses,
    };
  }

  private checkGenerationWarnings(rules: BusinessRule[], _testCases: TestCase[]): GenerationWarning[] {
    const warnings: GenerationWarning[] = [];

    for (const rule of rules) {
      // Check for missing inputs
      if (rule.inputs.length === 0) {
        warnings.push({
          ruleId: rule.id,
          type: 'missing_inputs',
          message: `Rule "${rule.name}" has no defined inputs`,
        });
      }

      // Check for ambiguous logic
      if (!rule.logic && !rule.formula) {
        warnings.push({
          ruleId: rule.id,
          type: 'ambiguous_logic',
          message: `Rule "${rule.name}" has no defined logic or formula`,
        });
      }

      // Check for missing edge cases
      if (!rule.edgeCases || rule.edgeCases.length === 0) {
        warnings.push({
          ruleId: rule.id,
          type: 'no_edge_cases',
          message: `Rule "${rule.name}" has no defined edge cases`,
        });
      }

      // Check for low confidence
      if (rule.confidence < 0.5) {
        warnings.push({
          ruleId: rule.id,
          type: 'low_confidence',
          message: `Rule "${rule.name}" has low confidence (${Math.round(rule.confidence * 100)}%)`,
        });
      }
    }

    return warnings;
  }

  private generateActualOutputs(testCase: TestCase): Record<string, unknown> {
    const outputs: Record<string, unknown> = {};
    for (const expected of testCase.expectedOutputs) {
      outputs[expected.name] = expected.value;
    }
    return outputs;
  }

  private generateIncorrectOutputs(testCase: TestCase): Record<string, unknown> {
    const outputs: Record<string, unknown> = {};
    for (const expected of testCase.expectedOutputs) {
      if (typeof expected.value === 'number') {
        outputs[expected.name] = expected.value * 1.1; // 10% off
      } else {
        outputs[expected.name] = 'incorrect_value';
      }
    }
    return outputs;
  }

  private findOutputDifferences(
    legacyResult: TestResult,
    modernResult: TestResult
  ): OutputDifference[] {
    const differences: OutputDifference[] = [];

    if (!legacyResult.actualOutputs || !modernResult.actualOutputs) {
      return differences;
    }

    const legacyOutputs = legacyResult.actualOutputs;
    const modernOutputs = modernResult.actualOutputs;

    // Check legacy outputs
    for (const [name, legacyValue] of Object.entries(legacyOutputs)) {
      const modernValue = modernOutputs[name];

      if (modernValue === undefined) {
        differences.push({
          outputName: name,
          legacyValue,
          modernValue: undefined,
          differenceType: 'missing',
          withinTolerance: false,
        });
      } else if (typeof legacyValue !== typeof modernValue) {
        differences.push({
          outputName: name,
          legacyValue,
          modernValue,
          differenceType: 'type',
          withinTolerance: false,
        });
      } else if (legacyValue !== modernValue) {
        const withinTolerance = this.checkTolerance(legacyValue, modernValue);
        differences.push({
          outputName: name,
          legacyValue,
          modernValue,
          differenceType: 'value',
          withinTolerance,
        });
      }
    }

    // Check for extra outputs in modern
    for (const name of Object.keys(modernOutputs)) {
      if (!(name in legacyOutputs)) {
        differences.push({
          outputName: name,
          legacyValue: undefined,
          modernValue: modernOutputs[name],
          differenceType: 'extra',
          withinTolerance: false,
        });
      }
    }

    return differences;
  }

  private checkTolerance(legacy: unknown, modern: unknown): boolean {
    if (typeof legacy === 'number' && typeof modern === 'number') {
      const tolerance = 0.01;
      return Math.abs(legacy - modern) <= Math.abs(legacy * tolerance);
    }
    return false;
  }

  private calculateMismatchSeverity(
    differences: OutputDifference[]
  ): 'critical' | 'major' | 'minor' {
    // Critical: type mismatches or missing values
    if (differences.some(d => d.differenceType === 'type' || d.differenceType === 'missing')) {
      return 'critical';
    }

    // Major: value differences outside tolerance
    if (differences.some(d => !d.withinTolerance)) {
      return 'major';
    }

    // Minor: differences within tolerance
    return 'minor';
  }

  private toYaml(suite: TestSuite): string {
    const lines: string[] = [
      `id: ${suite.id}`,
      `projectId: ${suite.projectId}`,
      `name: ${suite.name}`,
      `description: ${suite.description}`,
      `status: ${suite.status}`,
      `createdAt: ${suite.createdAt.toISOString()}`,
      `updatedAt: ${suite.updatedAt.toISOString()}`,
      '',
      'coverage:',
      `  rulesTotal: ${suite.coverage.rulesTotal}`,
      `  rulesCovered: ${suite.coverage.rulesCovered}`,
      `  rulesCoveragePercent: ${suite.coverage.rulesCoveragePercent.toFixed(1)}%`,
      '',
      'testCases:',
    ];

    for (const tc of suite.testCases) {
      lines.push(`  - id: ${tc.id}`);
      lines.push(`    name: ${tc.name}`);
      lines.push(`    type: ${tc.type}`);
      lines.push(`    priority: ${tc.priority}`);
      lines.push(`    status: ${tc.status}`);
    }

    return lines.join('\n');
  }
}
