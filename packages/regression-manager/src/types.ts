/**
 * Regression Manager Types
 * Types for automated regression test suite management
 */

import type { BusinessRule } from '@migrationpilot/core';

// ============================================================================
// TEST SUITE TYPES
// ============================================================================

export interface TestSuite {
  id: string;
  projectId: string;
  name: string;
  description: string;
  testCases: TestCase[];
  coverage: CoverageMetrics;
  status: SuiteStatus;
  createdAt: Date;
  updatedAt: Date;
  runHistory: TestRun[];
}

export interface TestCase {
  id: string;
  name: string;
  description: string;
  type: TestType;
  priority: TestPriority;
  relatedRuleIds: string[];
  inputs: TestInput[];
  expectedOutputs: ExpectedOutput[];
  preconditions: string[];
  postconditions: string[];
  tags: string[];
  status: TestCaseStatus;
  generatedCode?: GeneratedTestCode;
}

export interface TestInput {
  name: string;
  type: string;
  value: unknown;
  description?: string;
}

export interface ExpectedOutput {
  name: string;
  type: string;
  value: unknown;
  tolerance?: number;
  description?: string;
}

export interface GeneratedTestCode {
  language: string;
  framework: TestFramework;
  code: string;
  dependencies: string[];
}

export type TestType =
  | 'unit'
  | 'integration'
  | 'boundary'
  | 'equivalence'
  | 'regression'
  | 'smoke'
  | 'business-rule';

export type TestPriority = 'critical' | 'high' | 'medium' | 'low';

export type TestCaseStatus = 'draft' | 'ready' | 'approved' | 'deprecated';

export type SuiteStatus = 'draft' | 'active' | 'archived';

export type TestFramework =
  | 'jest'
  | 'vitest'
  | 'mocha'
  | 'pytest'
  | 'junit'
  | 'nunit'
  | 'xunit';

// ============================================================================
// TEST RUN TYPES
// ============================================================================

export interface TestRun {
  id: string;
  suiteId: string;
  startedAt: Date;
  completedAt?: Date;
  status: RunStatus;
  results: TestResult[];
  summary: RunSummary;
  environment: RunEnvironment;
}

export interface TestResult {
  testCaseId: string;
  status: ResultStatus;
  duration: number;
  actualOutputs?: Record<string, unknown>;
  error?: TestError;
  logs?: string[];
}

export interface TestError {
  message: string;
  type: string;
  stack?: string;
  expected?: unknown;
  actual?: unknown;
}

export interface RunSummary {
  total: number;
  passed: number;
  failed: number;
  skipped: number;
  error: number;
  duration: number;
  passRate: number;
}

export interface RunEnvironment {
  platform: string;
  runtime: string;
  runtimeVersion: string;
  testFramework: string;
  testFrameworkVersion: string;
}

export type RunStatus = 'running' | 'completed' | 'failed' | 'cancelled';
export type ResultStatus = 'passed' | 'failed' | 'skipped' | 'error';

// ============================================================================
// COVERAGE TYPES
// ============================================================================

export interface CoverageMetrics {
  rulesTotal: number;
  rulesCovered: number;
  rulesUncovered: string[];
  rulesCoveragePercent: number;
  edgeCasesTotal: number;
  edgeCasesCovered: number;
  boundaryConditions: BoundaryCondition[];
  equivalenceClasses: EquivalenceClass[];
}

export interface BoundaryCondition {
  ruleId: string;
  variable: string;
  boundary: string;
  coveredByTestIds: string[];
}

export interface EquivalenceClass {
  ruleId: string;
  variable: string;
  className: string;
  values: unknown[];
  coveredByTestIds: string[];
}

// ============================================================================
// GENERATION TYPES
// ============================================================================

export interface GenerationConfig {
  targetLanguage: string;
  targetFramework: TestFramework;
  generateBoundaryTests: boolean;
  generateEquivalenceTests: boolean;
  generateNegativeTests: boolean;
  maxTestsPerRule: number;
  includeEdgeCases: boolean;
  includeAssertions: boolean;
  includeDocumentation: boolean;
  testNamePattern?: string;
}

export interface GenerationResult {
  suiteId: string;
  testCasesGenerated: number;
  testCasesByRule: Record<string, string[]>;
  coverage: CoverageMetrics;
  warnings: GenerationWarning[];
  generatedFiles: GeneratedFile[];
}

export interface GenerationWarning {
  ruleId: string;
  type: 'missing_inputs' | 'ambiguous_logic' | 'no_edge_cases' | 'low_confidence';
  message: string;
}

export interface GeneratedFile {
  path: string;
  content: string;
  testCaseIds: string[];
}

// ============================================================================
// COMPARISON TYPES
// ============================================================================

export interface ComparisonResult {
  id: string;
  legacyRunId: string;
  modernRunId: string;
  timestamp: Date;
  matches: TestMatch[];
  mismatches: TestMismatch[];
  summary: ComparisonSummary;
}

export interface TestMatch {
  testCaseId: string;
  legacyResult: TestResult;
  modernResult: TestResult;
}

export interface TestMismatch {
  testCaseId: string;
  legacyResult: TestResult;
  modernResult: TestResult;
  differences: OutputDifference[];
  severity: 'critical' | 'major' | 'minor';
}

export interface OutputDifference {
  outputName: string;
  legacyValue: unknown;
  modernValue: unknown;
  differenceType: 'value' | 'type' | 'missing' | 'extra';
  withinTolerance: boolean;
}

export interface ComparisonSummary {
  totalTests: number;
  matchingTests: number;
  mismatchingTests: number;
  matchRate: number;
  criticalMismatches: number;
  majorMismatches: number;
  minorMismatches: number;
}

// ============================================================================
// SERVICE INTERFACE
// ============================================================================

export interface IRegressionManager {
  createSuite(projectId: string, name: string, rules: BusinessRule[]): Promise<TestSuite>;
  generateTestCases(suiteId: string, rules: BusinessRule[], config: GenerationConfig): Promise<GenerationResult>;
  runSuite(suiteId: string): Promise<TestRun>;
  compareRuns(legacyRunId: string, modernRunId: string): Promise<ComparisonResult>;
  getCoverage(suiteId: string): CoverageMetrics;
  exportSuite(suiteId: string, format: 'json' | 'yaml' | 'code'): string;
}

// ============================================================================
// DEFAULT CONFIGURATION
// ============================================================================

export const DEFAULT_GENERATION_CONFIG: GenerationConfig = {
  targetLanguage: 'typescript',
  targetFramework: 'vitest',
  generateBoundaryTests: true,
  generateEquivalenceTests: true,
  generateNegativeTests: true,
  maxTestsPerRule: 10,
  includeEdgeCases: true,
  includeAssertions: true,
  includeDocumentation: true,
  testNamePattern: 'should {action} when {condition}',
};
