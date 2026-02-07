/**
 * Regression Manager Package
 * Automated regression test suite management for legacy code migration
 */

// Main exports
export { RegressionManager } from './regression-manager.js';
export { TestGenerator } from './test-generator.js';
export { CodeGenerator } from './code-generator.js';

// Type exports
export type {
  // Suite types
  TestSuite,
  TestCase,
  TestInput,
  ExpectedOutput,
  GeneratedTestCode,
  TestType,
  TestPriority,
  TestCaseStatus,
  SuiteStatus,
  TestFramework,
  // Run types
  TestRun,
  TestResult,
  TestError,
  RunSummary,
  RunEnvironment,
  RunStatus,
  ResultStatus,
  // Coverage types
  CoverageMetrics,
  BoundaryCondition,
  EquivalenceClass,
  // Generation types
  GenerationConfig,
  GenerationResult,
  GenerationWarning,
  GeneratedFile,
  // Comparison types
  ComparisonResult,
  TestMatch,
  TestMismatch,
  OutputDifference,
  ComparisonSummary,
  // Service interface
  IRegressionManager,
} from './types.js';

export { DEFAULT_GENERATION_CONFIG } from './types.js';
