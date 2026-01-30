/**
 * Execution Tracing Types
 * 
 * Core types for instrumenting legacy systems and capturing execution traces
 */

import type { SourceLanguage } from '@migrationpilot/core';

// ============================================================================
// TRACE EVENT TYPES
// ============================================================================

export type TraceEventType = 
  | 'program_start'
  | 'program_end'
  | 'procedure_entry'
  | 'procedure_exit'
  | 'variable_read'
  | 'variable_write'
  | 'branch_taken'
  | 'branch_not_taken'
  | 'loop_iteration'
  | 'file_open'
  | 'file_read'
  | 'file_write'
  | 'file_close'
  | 'db_query'
  | 'db_result'
  | 'external_call'
  | 'external_return'
  | 'error'
  | 'user_input'
  | 'user_output';

export interface TraceEvent {
  id: string;
  sessionId: string;
  timestamp: number;
  sequenceNumber: number;
  type: TraceEventType;
  location: TraceLocation;
  data: TraceEventData;
  metadata?: Record<string, unknown>;
}

export interface TraceLocation {
  file: string;
  line: number;
  column?: number;
  procedure?: string;
  section?: string;
}

export interface TraceEventData {
  name?: string;
  value?: unknown;
  previousValue?: unknown;
  parameters?: Record<string, unknown>;
  returnValue?: unknown;
  condition?: string;
  conditionResult?: boolean;
  query?: string;
  recordCount?: number;
  errorCode?: string;
  errorMessage?: string;
  raw?: string;
  [key: string]: unknown;  // Allow additional fields
}

// ============================================================================
// EXECUTION TRACE TYPES
// ============================================================================

export interface ExecutionTrace {
  id: string;
  sessionId: string;
  projectId: string;
  programName: string;
  language: SourceLanguage;
  
  // Timing
  startTime: number;
  endTime: number;
  durationMs: number;
  
  // I/O Capture
  inputs: CapturedIO;
  outputs: CapturedIO;
  
  // Execution path
  events: TraceEvent[];
  callStack: CallStackEntry[];
  
  // Aggregated metrics
  metrics: TraceMetrics;
  
  // Metadata
  environment?: EnvironmentInfo;
  capturedAt: Date;
}

export interface CapturedIO {
  parameters: Record<string, unknown>;
  files: FileIO[];
  database: DatabaseIO[];
  screens: ScreenIO[];
  messages: MessageIO[];
}

export interface FileIO {
  filename: string;
  operation: 'read' | 'write' | 'update' | 'delete';
  records: unknown[];
  recordCount: number;
  format?: string;
}

export interface DatabaseIO {
  operation: 'select' | 'insert' | 'update' | 'delete' | 'call';
  table?: string;
  query?: string;
  parameters?: Record<string, unknown>;
  results?: unknown[];
  rowCount: number;
  durationMs: number;
}

export interface ScreenIO {
  screenId: string;
  operation: 'display' | 'accept';
  fields: Record<string, unknown>;
}

export interface MessageIO {
  queue?: string;
  operation: 'send' | 'receive';
  content: unknown;
  correlationId?: string;
}

export interface CallStackEntry {
  procedure: string;
  enteredAt: number;
  exitedAt?: number;
  parameters?: Record<string, unknown>;
  returnValue?: unknown;
  depth: number;
}

export interface TraceMetrics {
  totalEvents: number;
  proceduresCalled: number;
  branchesTaken: number;
  loopIterations: number;
  fileOperations: number;
  dbOperations: number;
  externalCalls: number;
  errorsEncountered: number;
  uniqueCodePaths: number;
}

export interface EnvironmentInfo {
  hostname?: string;
  platform?: string;
  runtime?: string;
  version?: string;
  timezone?: string;
  locale?: string;
  customVariables?: Record<string, string>;
}

// ============================================================================
// TRACER CONFIGURATION
// ============================================================================

export interface TracerConfig {
  language: SourceLanguage;
  projectId: string;
  
  // What to capture
  captureVariables: boolean;
  captureBranches: boolean;
  captureLoops: boolean;
  captureFileIO: boolean;
  captureDatabaseIO: boolean;
  captureExternalCalls: boolean;
  
  // Performance settings
  samplingRate: number; // 0.0 to 1.0, 1.0 = capture everything
  maxEventsPerTrace: number;
  maxTracesDuration: number; // ms
  asyncCapture: boolean;
  
  // Security
  anonymizePII: boolean;
  piiPatterns?: RegExp[];
  excludeFields?: string[];
  
  // Storage
  storageBackend: 'memory' | 'file' | 'database' | 's3';
  storageConfig?: Record<string, unknown>;
}

export const DEFAULT_TRACER_CONFIG: Partial<TracerConfig> = {
  captureVariables: true,
  captureBranches: true,
  captureLoops: true,
  captureFileIO: true,
  captureDatabaseIO: true,
  captureExternalCalls: true,
  samplingRate: 1.0,
  maxEventsPerTrace: 100000,
  maxTracesDuration: 300000, // 5 minutes
  asyncCapture: true,
  anonymizePII: true,
  storageBackend: 'database',
};

// ============================================================================
// SHADOW TRAFFIC TYPES
// ============================================================================

export interface ShadowConfig {
  projectId: string;
  enabled: boolean;
  
  // Traffic selection
  samplingRate: number;
  includePatterns?: string[];
  excludePatterns?: string[];
  
  // Capture settings
  captureRequest: boolean;
  captureResponse: boolean;
  captureTimings: boolean;
  
  // Storage
  retentionDays: number;
  maxStorageMB: number;
  
  // Real-time comparison
  enableComparison: boolean;
  comparisonEndpoint?: string;
  toleranceConfig?: ToleranceConfig;
}

export interface ToleranceConfig {
  numericTolerance: number;
  stringNormalization: 'none' | 'trim' | 'lowercase' | 'alphanumeric';
  dateToleranceMs: number;
  ignoreFields: string[];
  customComparators?: Record<string, (a: unknown, b: unknown) => boolean>;
}

export interface ShadowCapture {
  id: string;
  projectId: string;
  sessionId: string;
  
  // Request
  requestTimestamp: Date;
  requestData: unknown;
  requestMetadata?: Record<string, unknown>;
  
  // Legacy response
  legacyResponse: unknown;
  legacyDurationMs: number;
  legacyError?: string;
  
  // Modern response (if comparison enabled)
  modernResponse?: unknown;
  modernDurationMs?: number;
  modernError?: string;
  
  // Comparison result
  comparisonResult?: ComparisonResult;
  
  // Metadata
  capturedAt: Date;
  processed: boolean;
}

export interface ComparisonResult {
  equivalent: boolean;
  differences: FieldDifference[];
  legacyOnly: string[];
  modernOnly: string[];
  matchedFields: number;
  totalFields: number;
  confidence: number;
}

export interface FieldDifference {
  path: string;
  legacyValue: unknown;
  modernValue: unknown;
  differenceType: 'value' | 'type' | 'missing' | 'extra' | 'tolerance';
  severity: 'critical' | 'warning' | 'info';
  toleranceExceeded?: boolean;
}

// ============================================================================
// BEHAVIORAL LEARNING TYPES
// ============================================================================

export interface LearnedBehavior {
  id: string;
  projectId: string;
  ruleId?: string;
  
  // Pattern identification
  pattern: BehaviorPattern;
  frequency: number;
  confidence: number;
  
  // Evidence
  supportingTraces: string[];
  exampleInputs: unknown[];
  exampleOutputs: unknown[];
  
  // Status
  status: 'discovered' | 'validated' | 'rejected' | 'merged';
  validatedBy?: string;
  validatedAt?: Date;
  
  discoveredAt: Date;
}

export interface BehaviorPattern {
  type: 'calculation' | 'validation' | 'decision' | 'transformation' | 'constraint';
  description: string;
  inputSignature: FieldSignature[];
  outputSignature: FieldSignature[];
  conditions?: ConditionPattern[];
  formula?: string;
  edgeCases?: EdgeCasePattern[];
}

export interface FieldSignature {
  name: string;
  type: 'string' | 'number' | 'boolean' | 'date' | 'object' | 'array';
  nullable: boolean;
  observedRange?: { min: unknown; max: unknown };
  observedValues?: unknown[];
  distribution?: 'uniform' | 'normal' | 'skewed' | 'categorical';
}

export interface ConditionPattern {
  condition: string;
  frequency: number;
  outcome: string;
}

export interface EdgeCasePattern {
  description: string;
  trigger: Record<string, unknown>;
  expectedBehavior: string;
  observedCount: number;
}

export interface CoverageAnalysis {
  projectId: string;
  analyzedAt: Date;
  
  // Rule coverage
  totalRules: number;
  rulesWithTraces: number;
  ruleCoverage: number;
  
  // Code path coverage
  totalCodePaths: number;
  observedCodePaths: number;
  pathCoverage: number;
  
  // Edge case coverage
  totalEdgeCases: number;
  observedEdgeCases: number;
  edgeCaseCoverage: number;
  
  // Gaps
  uncoveredRules: string[];
  uncoveredPaths: CodePathGap[];
  suggestedTestCases: SuggestedTestCase[];
}

export interface CodePathGap {
  location: TraceLocation;
  condition: string;
  neverTakenBranch: 'true' | 'false';
  suggestedInput?: Record<string, unknown>;
}

export interface SuggestedTestCase {
  ruleId?: string;
  reason: string;
  inputs: Record<string, unknown>;
  expectedBehavior: string;
  priority: 'high' | 'medium' | 'low';
}

// ============================================================================
// TRACER INTERFACE
// ============================================================================

export interface ITracer {
  readonly language: SourceLanguage;
  readonly config: TracerConfig;
  
  // Lifecycle
  initialize(): Promise<void>;
  shutdown(): Promise<void>;
  
  // Trace management
  startTrace(programName: string, inputs: Record<string, unknown>): string;
  endTrace(sessionId: string, outputs: Record<string, unknown>): ExecutionTrace;
  
  // Event recording
  recordEvent(sessionId: string, event: Omit<TraceEvent, 'id' | 'sessionId' | 'timestamp' | 'sequenceNumber'>): void;
  
  // Query
  getTrace(sessionId: string): ExecutionTrace | null;
  listTraces(filter?: TraceFilter): ExecutionTrace[];
}

export interface TraceFilter {
  projectId?: string;
  programName?: string;
  startTimeAfter?: Date;
  startTimeBefore?: Date;
  minDuration?: number;
  maxDuration?: number;
  hasErrors?: boolean;
  limit?: number;
  offset?: number;
}
