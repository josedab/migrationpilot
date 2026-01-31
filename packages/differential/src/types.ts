/**
 * Behavioral Differential Analysis Types
 */

// ============================================================================
// Core Types
// ============================================================================

export interface DifferentialConfig {
  maxIterations?: number;
  timeout?: number;
  fuzzingDepth?: number;
  numericTolerance?: number;
  stringComparison?: 'exact' | 'normalized' | 'fuzzy';
  dateFormatTolerance?: boolean;
  nullEquivalents?: string[];
  ignoreWhitespace?: boolean;
  parallelExecution?: boolean;
  maxConcurrency?: number;
}

export const DEFAULT_CONFIG: DifferentialConfig = {
  maxIterations: 1000,
  timeout: 30000,
  fuzzingDepth: 3,
  numericTolerance: 0.0001,
  stringComparison: 'normalized',
  dateFormatTolerance: true,
  nullEquivalents: ['', 'null', 'NULL', 'None', 'undefined', 'N/A'],
  ignoreWhitespace: true,
  parallelExecution: true,
  maxConcurrency: 4,
};

// ============================================================================
// Execution Types
// ============================================================================

export interface ExecutableCode {
  id: string;
  language: 'cobol' | 'java' | 'python' | 'typescript' | 'csharp' | 'go';
  code: string;
  entryPoint: string;
  dependencies?: string[];
}

export interface ExecutionContext {
  inputs: Record<string, DataValue>;
  environment?: Record<string, string>;
  mockServices?: MockServiceDefinition[];
  timeout?: number;
}

export interface DataValue {
  type: 'string' | 'number' | 'boolean' | 'date' | 'decimal' | 'array' | 'object' | 'null';
  value: unknown;
  format?: string;
  precision?: number;
}

export interface MockServiceDefinition {
  name: string;
  type: 'database' | 'api' | 'file' | 'queue';
  responses: MockResponse[];
}

export interface MockResponse {
  pattern: string;
  response: unknown;
  delay?: number;
}

// ============================================================================
// Execution Result Types
// ============================================================================

export interface ExecutionResult {
  success: boolean;
  outputs: Record<string, DataValue>;
  sideEffects: SideEffect[];
  trace: ExecutionTrace;
  duration: number;
  error?: ExecutionError;
}

export interface ExecutionTrace {
  steps: TraceStep[];
  branches: BranchInfo[];
  calls: CallInfo[];
  memory: MemorySnapshot[];
}

export interface TraceStep {
  timestamp: number;
  location: CodeLocation;
  operation: string;
  values: Record<string, unknown>;
}

export interface BranchInfo {
  location: CodeLocation;
  condition: string;
  result: boolean;
  coverage: 'taken' | 'not-taken' | 'both';
}

export interface CallInfo {
  location: CodeLocation;
  target: string;
  arguments: unknown[];
  returnValue: unknown;
  duration: number;
}

export interface MemorySnapshot {
  timestamp: number;
  variables: Record<string, DataValue>;
}

export interface CodeLocation {
  file: string;
  line: number;
  column?: number;
  function?: string;
}

export interface SideEffect {
  type: 'database' | 'file' | 'network' | 'queue' | 'log';
  operation: string;
  target: string;
  data: unknown;
  timestamp: number;
}

export interface ExecutionError {
  type: 'runtime' | 'timeout' | 'resource' | 'assertion';
  message: string;
  location?: CodeLocation;
  stackTrace?: string;
}

// ============================================================================
// Comparison Result Types
// ============================================================================

export interface BehaviorComparison {
  id: string;
  timestamp: Date;
  legacyCode: ExecutableCode;
  migratedCode: ExecutableCode;
  testCases: TestCaseResult[];
  summary: ComparisonSummary;
  differences: BehaviorDifference[];
}

export interface TestCaseResult {
  id: string;
  name: string;
  context: ExecutionContext;
  legacyResult: ExecutionResult;
  migratedResult: ExecutionResult;
  comparison: TestCaseComparison;
}

export interface TestCaseComparison {
  equivalent: boolean;
  outputDiffs: OutputDifference[];
  traceDiffs: TraceDifference[];
  sideEffectDiffs: SideEffectDifference[];
  performanceDiff?: PerformanceDiff;
}

export interface OutputDifference {
  path: string;
  legacyValue: DataValue;
  migratedValue: DataValue;
  diffType: 'missing' | 'added' | 'changed' | 'type-mismatch';
  severity: 'critical' | 'warning' | 'info';
  explanation?: string;
}

export interface TraceDifference {
  type: 'branch' | 'call' | 'order';
  location: CodeLocation;
  legacy: unknown;
  migrated: unknown;
  impact: 'high' | 'medium' | 'low';
}

export interface SideEffectDifference {
  type: SideEffect['type'];
  operation: string;
  legacy?: SideEffect;
  migrated?: SideEffect;
  diff: 'missing' | 'added' | 'modified';
}

export interface PerformanceDiff {
  legacyDuration: number;
  migratedDuration: number;
  ratio: number;
  significant: boolean;
}

export interface ComparisonSummary {
  totalTestCases: number;
  equivalentCases: number;
  failedCases: number;
  warningCases: number;
  equivalenceRate: number;
  criticalDifferences: number;
  warningDifferences: number;
  averagePerformanceRatio: number;
}

// ============================================================================
// Input Generation Types
// ============================================================================

export interface InputGenerator {
  id: string;
  strategy: InputStrategy;
  constraints: InputConstraint[];
}

export type InputStrategy = 
  | 'random'
  | 'boundary'
  | 'symbolic'
  | 'property-based'
  | 'historic'
  | 'mutation';

export interface InputConstraint {
  field: string;
  type: DataValue['type'];
  constraints: {
    min?: number;
    max?: number;
    pattern?: string;
    enum?: unknown[];
    nullable?: boolean;
    length?: { min?: number; max?: number };
  };
}

export interface GeneratedInput {
  id: string;
  strategy: InputStrategy;
  values: Record<string, DataValue>;
  rationale?: string;
}

// ============================================================================
// Report Types
// ============================================================================

export interface DifferentialReport {
  id: string;
  timestamp: Date;
  comparison: BehaviorComparison;
  format: 'json' | 'html' | 'markdown' | 'pdf';
  sections: ReportSection[];
}

export interface ReportSection {
  id: string;
  title: string;
  type: 'summary' | 'details' | 'recommendations' | 'metrics';
  content: unknown;
}

export interface ReportMetrics {
  equivalenceScore: number;
  confidenceLevel: number;
  coverageMetrics: CoverageMetrics;
  riskAssessment: RiskAssessment;
}

export interface CoverageMetrics {
  lineCoverage: number;
  branchCoverage: number;
  pathCoverage: number;
  inputSpaceCoverage: number;
}

export interface RiskAssessment {
  overallRisk: 'low' | 'medium' | 'high' | 'critical';
  factors: RiskFactor[];
}

export interface RiskFactor {
  category: string;
  description: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  recommendation: string;
}

// ============================================================================
// Event Types
// ============================================================================

export type DifferentialEvent = 
  | { type: 'analysis-started'; comparison: BehaviorComparison }
  | { type: 'test-case-started'; testCase: TestCaseResult }
  | { type: 'test-case-completed'; testCase: TestCaseResult }
  | { type: 'difference-detected'; difference: BehaviorDifference }
  | { type: 'analysis-completed'; comparison: BehaviorComparison }
  | { type: 'analysis-error'; error: Error };

export interface BehaviorDifference {
  id: string;
  type: 'output' | 'trace' | 'side-effect' | 'performance';
  severity: 'critical' | 'warning' | 'info';
  testCaseId: string;
  description: string;
  legacy: unknown;
  migrated: unknown;
  possibleCauses: string[];
  suggestedFixes: string[];
}
