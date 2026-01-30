/**
 * Types for Equivalence Testing
 */

// Re-export for convenience - don't import directly
export type { BusinessRule as _BusinessRuleRef } from '@migrationpilot/core';

// Local type for business rules used in testing
export interface TestBusinessRule {
  id: string;
  name: string;
  description: string;
  inputs?: TestRuleInput[];
  outputs?: TestRuleOutput[];
  edgeCases?: string[];
  confidence: number;
  logic?: {
    calculation?: string;
  };
  sourceLocation?: {
    file: string;
    startLine: number;
    endLine: number;
  };
}

export interface TestRuleInput {
  name: string;
  type: string;
  description?: string;
  constraints?: {
    min?: number;
    max?: number;
    enum?: string[];
  };
}

export interface TestRuleOutput {
  name: string;
  type: string;
  description?: string;
}

export interface TestCase {
  id: string;
  name: string;
  description: string;
  inputs: Record<string, unknown>;
  expectedOutput?: unknown;
  tolerance?: ToleranceConfig;
  tags: string[];
  sourceRule?: string;
  generationStrategy: TestGenerationStrategy;
  priority: 'critical' | 'high' | 'medium' | 'low';
}

export interface ToleranceConfig {
  numeric?: number;
  string?: 'exact' | 'trim' | 'case-insensitive';
  date?: number; // milliseconds
  array?: 'ordered' | 'unordered';
}

export type TestGenerationStrategy = 
  | 'boundary'
  | 'equivalence-partition'
  | 'historical-replay'
  | 'property-based'
  | 'random'
  | 'manual';

export interface TestResult {
  testCase: TestCase;
  legacyOutput: unknown;
  modernOutput: unknown;
  equivalent: boolean;
  executionTimeMs: {
    legacy: number;
    modern: number;
  };
  differences?: Difference[];
  error?: string;
}

export interface Difference {
  path: string;
  legacyValue: unknown;
  modernValue: unknown;
  type: 'missing' | 'extra' | 'type-mismatch' | 'value-mismatch';
  severity: 'critical' | 'warning' | 'info';
}

export interface EquivalenceReport {
  projectId: string;
  moduleId: string;
  timestamp: string;
  totalTests: number;
  passed: number;
  failed: number;
  skipped: number;
  coverage: CoverageMetrics;
  confidence: number;
  results: TestResult[];
  summary: ReportSummary;
}

export interface CoverageMetrics {
  rulesCovered: number;
  totalRules: number;
  codePathsCovered: number;
  totalCodePaths: number;
  boundaryTestsCovered: number;
  edgeCasesCovered: number;
}

export interface ReportSummary {
  status: 'passed' | 'failed' | 'warning';
  passRate: number;
  criticalFailures: number;
  recommendations: string[];
  riskAssessment: 'low' | 'medium' | 'high';
}

export interface SystemAdapter {
  name: string;
  type: 'legacy' | 'modern';
  execute(inputs: Record<string, unknown>): Promise<ExecutionResult>;
  healthCheck(): Promise<boolean>;
}

export interface ExecutionResult {
  output: unknown;
  executionTimeMs: number;
  metadata?: Record<string, unknown>;
  error?: string;
}

export interface PropertyDefinition {
  name: string;
  description: string;
  inputGenerator: () => Record<string, unknown>;
  property: (legacyOutput: unknown, modernOutput: unknown, inputs: Record<string, unknown>) => boolean;
  iterations?: number;
}

export interface TraceEvent {
  timestamp: number;
  type: 'input' | 'output' | 'call' | 'return' | 'variable' | 'branch';
  location: string;
  data: unknown;
}

export interface ExecutionTrace {
  sessionId: string;
  systemType: 'legacy' | 'modern';
  startTime: number;
  endTime: number;
  inputs: Record<string, unknown>;
  output: unknown;
  events: TraceEvent[];
}
