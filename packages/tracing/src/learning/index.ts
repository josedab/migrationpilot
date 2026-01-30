/**
 * Learning Module
 * 
 * Behavioral learning from execution traces
 */

export { 
  TraceAnalyzer, 
  type TraceAnalyzerConfig,
  type RuleValidation,
  type RuleDiscrepancy,
  type RuleValidationReport,
  type ValidationSummary,
} from './trace-analyzer.js';

export { 
  CoverageCalculator, 
  type CoverageConfig,
  type CoverageSummary,
} from './coverage-calculator.js';
