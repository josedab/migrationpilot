/**
 * Mainframe Execution Tracing Types
 * 
 * Types for capturing live execution traces from z/OS mainframe systems,
 * including CICS transactions, DB2 operations, and batch jobs.
 */

import type { SourceLanguage } from '@migrationpilot/core';
import type { TraceEvent, TraceLocation, CapturedIO } from '../types.js';

// ============================================================================
// Z/OS TRACE CAPTURE TYPES
// ============================================================================

export type MainframePlatform = 'zos' | 'iseries' | 'unisys' | 'tandem';
export type MainframeTraceSource = 'debug_tool' | 'xpediter' | 'cedf' | 'strobe' | 'smf' | 'gtf' | 'custom_agent';

export interface MainframeTraceConfig {
  projectId: string;
  platform: MainframePlatform;
  traceSource: MainframeTraceSource;
  
  // Connection settings
  connection: MainframeConnection;
  
  // Capture settings
  captureSettings: MainframeCaptureSettings;
  
  // Security
  security: MainframeSecurityConfig;
  
  // Performance
  performance: MainframePerformanceConfig;
}

export interface MainframeConnection {
  host: string;
  port: number;
  protocol: 'tn3270' | 'ssh' | 'mq' | 'rest' | 'agent';
  
  // For agent-based collection
  agentEndpoint?: string;
  agentPort?: number;
  
  // For MQ-based collection
  queueManager?: string;
  inputQueue?: string;
  outputQueue?: string;
  
  // Timeouts
  connectionTimeoutMs: number;
  readTimeoutMs: number;
}

export interface MainframeCaptureSettings {
  // What to capture
  captureCICS: boolean;
  captureDB2: boolean;
  captureIMS: boolean;
  captureBatch: boolean;
  captureVSAM: boolean;
  
  // Granularity
  captureStatements: boolean;
  captureVariables: boolean;
  captureWorkingStorage: boolean;
  captureLinkageSection: boolean;
  
  // Transaction filtering
  transactionFilter?: TransactionFilter;
  
  // Batch filtering
  jobFilter?: BatchJobFilter;
  
  // Sampling for high-volume environments
  samplingMode: 'all' | 'random' | 'time_based' | 'transaction_based';
  samplingRate: number;
  samplingWindowMs?: number;
}

export interface TransactionFilter {
  includeTransactionIds?: string[];
  excludeTransactionIds?: string[];
  includeProgramNames?: string[];
  excludeProgramNames?: string[];
  includeUserIds?: string[];
  minResponseTimeMs?: number;
}

export interface BatchJobFilter {
  includeJobNames?: string[];
  excludeJobNames?: string[];
  includeStepNames?: string[];
  includeJobClasses?: string[];
}

export interface MainframeSecurityConfig {
  // Authentication
  authType: 'racf' | 'acf2' | 'top_secret' | 'api_key' | 'certificate';
  userId?: string;
  encryptedPassword?: string;
  certificatePath?: string;
  apiKey?: string;
  
  // Data protection
  anonymizePII: boolean;
  piiFields: string[];
  maskPattern?: string;
  encryptTraces: boolean;
  encryptionKeyId?: string;
  
  // Audit
  auditAllAccess: boolean;
  auditDestination?: string;
}

export interface MainframePerformanceConfig {
  // Resource limits
  maxCpuPercent: number;
  maxMemoryMB: number;
  maxTraceBufferMB: number;
  
  // Batching
  batchSize: number;
  flushIntervalMs: number;
  
  // Throttling
  maxEventsPerSecond: number;
  backoffMultiplier: number;
  maxBackoffMs: number;
  
  // Storage
  compressionEnabled: boolean;
  compressionLevel: number;
}

// ============================================================================
// CICS-SPECIFIC TYPES
// ============================================================================

export interface CICSTraceEvent extends TraceEvent {
  cicsContext: CICSContext;
}

export interface CICSContext {
  transactionId: string;
  taskNumber: number;
  terminalId?: string;
  userId: string;
  applid: string;
  sysid: string;
  
  // EIB fields
  eibcalen?: number;
  eibtrnid?: string;
  eibtaskn?: number;
  eibtime?: number;
  eibdate?: number;
  
  // Resource tracking
  programName: string;
  callingProgram?: string;
  commarea?: CICSCommarea;
  channels?: CICSChannel[];
}

export interface CICSCommarea {
  length: number;
  dataHex: string;
  parsedFields?: Record<string, unknown>;
}

export interface CICSChannel {
  name: string;
  containers: CICSContainer[];
}

export interface CICSContainer {
  name: string;
  dataType: 'char' | 'bit';
  length: number;
  data: string;
  parsedFields?: Record<string, unknown>;
}

export interface CICSCommand {
  type: CICSCommandType;
  timestamp: number;
  responseCode?: number;
  resource?: string;
  options?: Record<string, unknown>;
  duration?: number;
}

export type CICSCommandType = 
  | 'LINK' | 'XCTL' | 'RETURN'
  | 'READ' | 'WRITE' | 'REWRITE' | 'DELETE' | 'UNLOCK'
  | 'READQ' | 'WRITEQ' | 'DELETEQ'
  | 'GET' | 'PUT'
  | 'SEND' | 'RECEIVE'
  | 'SYNCPOINT' | 'SYNCPOINT_ROLLBACK'
  | 'ASKTIME' | 'FORMATTIME'
  | 'START' | 'RETRIEVE' | 'CANCEL'
  | 'GETMAIN' | 'FREEMAIN'
  | 'ABEND' | 'HANDLE';

// ============================================================================
// DB2-SPECIFIC TYPES
// ============================================================================

export interface DB2TraceEvent extends TraceEvent {
  db2Context: DB2Context;
}

export interface DB2Context {
  planName: string;
  packageName?: string;
  collectionId?: string;
  correlationId: string;
  authId: string;
  
  // Statement info
  statementId?: number;
  statementType: DB2StatementType;
  sqlText: string;
  hostVariables?: DB2HostVariable[];
  
  // Execution info
  sqlCode: number;
  sqlState?: string;
  rowsAffected?: number;
  fetchCount?: number;
  
  // Performance
  cpuTimeMicroseconds?: number;
  elapsedTimeMicroseconds?: number;
  getpages?: number;
  bufferPoolReads?: number;
}

export type DB2StatementType = 
  | 'SELECT' | 'INSERT' | 'UPDATE' | 'DELETE'
  | 'CALL' | 'MERGE'
  | 'OPEN' | 'FETCH' | 'CLOSE'
  | 'COMMIT' | 'ROLLBACK'
  | 'DECLARE' | 'PREPARE' | 'EXECUTE';

export interface DB2HostVariable {
  name: string;
  dataType: string;
  length: number;
  value: unknown;
  indicator?: number;
}

// ============================================================================
// BATCH JOB TYPES
// ============================================================================

export interface BatchJobTrace {
  id: string;
  projectId: string;
  
  // Job identification
  jobName: string;
  jobId: string;
  jobClass: string;
  submittedBy: string;
  
  // Timing
  submittedAt: Date;
  startedAt: Date;
  endedAt?: Date;
  durationMs?: number;
  
  // Job steps
  steps: BatchStepTrace[];
  
  // Resources
  datasets: DatasetAccess[];
  
  // Return codes
  conditionCode?: number;
  abendCode?: string;
}

export interface BatchStepTrace {
  stepName: string;
  procStep?: string;
  programName: string;
  
  startedAt: Date;
  endedAt?: Date;
  durationMs?: number;
  
  // Execution trace
  events: TraceEvent[];
  
  // I/O
  ddStatements: DDStatement[];
  
  // Return
  conditionCode?: number;
  abendCode?: string;
}

export interface DDStatement {
  ddName: string;
  dsName?: string;
  disposition: 'NEW' | 'OLD' | 'SHR' | 'MOD';
  recordFormat?: string;
  recordLength?: number;
  blockSize?: number;
  
  // I/O statistics
  recordsRead?: number;
  recordsWritten?: number;
  bytesTransferred?: number;
}

export interface DatasetAccess {
  dsName: string;
  ddName: string;
  accessType: 'read' | 'write' | 'update';
  organization: 'PS' | 'PO' | 'VSAM_KSDS' | 'VSAM_ESDS' | 'VSAM_RRDS' | 'GDG';
  recordCount?: number;
}

// ============================================================================
// VSAM-SPECIFIC TYPES
// ============================================================================

export interface VSAMOperation {
  id: string;
  timestamp: number;
  
  filename: string;
  organization: 'KSDS' | 'ESDS' | 'RRDS' | 'LDS';
  operation: VSAMOperationType;
  
  // Key/record info
  key?: string;
  rba?: number;
  rrn?: number;
  record?: string;
  recordLength?: number;
  
  // Result
  feedbackCode: number;
  returnCode: number;
}

export type VSAMOperationType = 
  | 'OPEN' | 'CLOSE'
  | 'GET' | 'PUT' | 'ERASE'
  | 'POINT' | 'ENDREQ'
  | 'READ_NEXT' | 'READ_PREV'
  | 'WRITE' | 'REWRITE' | 'DELETE';

// ============================================================================
// TRACE AGGREGATION TYPES
// ============================================================================

export interface MainframeExecutionTrace {
  id: string;
  projectId: string;
  platform: MainframePlatform;
  language: SourceLanguage;
  
  // Source identification
  programName: string;
  transactionId?: string;
  jobName?: string;
  
  // Timing
  startTime: number;
  endTime: number;
  durationMs: number;
  
  // Context
  cicsContext?: CICSContext;
  db2Context?: DB2Context;
  batchContext?: BatchJobTrace;
  
  // Events
  events: TraceEvent[];
  cicsCommands?: CICSCommand[];
  db2Operations?: DB2TraceEvent[];
  vsamOperations?: VSAMOperation[];
  
  // I/O
  inputs: MainframeCapturedIO;
  outputs: MainframeCapturedIO;
  
  // Aggregated metrics
  metrics: MainframeTraceMetrics;
  
  // Data quality
  captureQuality: CaptureQuality;
  
  capturedAt: Date;
}

export interface MainframeCapturedIO extends CapturedIO {
  commarea?: CICSCommarea;
  channels?: CICSChannel[];
  linkageSection?: Record<string, unknown>;
  workingStorage?: Record<string, unknown>;
  datasets?: DatasetCapture[];
}

export interface DatasetCapture {
  dsName: string;
  ddName: string;
  operation: 'input' | 'output' | 'update';
  records: unknown[];
  recordCount: number;
}

export interface MainframeTraceMetrics {
  totalEvents: number;
  statementCount: number;
  proceduresCalled: number;
  paragraphsExecuted: number;
  
  // CICS metrics
  cicsCommandCount: number;
  linkCount: number;
  xctlCount: number;
  
  // DB2 metrics
  db2CallCount: number;
  sqlSelectCount: number;
  sqlInsertCount: number;
  sqlUpdateCount: number;
  sqlDeleteCount: number;
  totalRowsProcessed: number;
  
  // I/O metrics
  fileOperations: number;
  vsamOperations: number;
  recordsRead: number;
  recordsWritten: number;
  
  // Performance
  cpuTimeMicroseconds: number;
  waitTimeMicroseconds: number;
  
  // Quality
  branchesCovered: number;
  totalBranches: number;
  uniqueCodePaths: number;
}

export interface CaptureQuality {
  completeness: number;
  eventDropCount: number;
  truncatedFields: number;
  parseErrors: string[];
  warnings: string[];
}

// ============================================================================
// TRACE NORMALIZATION TYPES
// ============================================================================

export interface NormalizedTrace {
  id: string;
  projectId: string;
  sourceTraceId: string;
  sourceType: 'mainframe' | 'emulated' | 'replayed';
  
  // Normalized program info
  program: NormalizedProgram;
  
  // Normalized execution flow
  executionPath: ExecutionPathNode[];
  
  // Normalized data
  inputData: NormalizedDataSet;
  outputData: NormalizedDataSet;
  intermediateStates: NormalizedDataSet[];
  
  // Inferred behavior
  inferredRules: InferredRule[];
  
  normalizedAt: Date;
}

export interface NormalizedProgram {
  name: string;
  language: SourceLanguage;
  entryPoint: string;
  procedures: NormalizedProcedure[];
  dataStructures: NormalizedDataStructure[];
}

export interface NormalizedProcedure {
  name: string;
  type: 'paragraph' | 'section' | 'function' | 'subroutine';
  callCount: number;
  averageDurationMs: number;
  calledBy: string[];
  calls: string[];
}

export interface NormalizedDataStructure {
  name: string;
  level: number;
  type: string;
  length: number;
  children?: NormalizedDataStructure[];
}

export interface ExecutionPathNode {
  sequenceNumber: number;
  nodeType: 'statement' | 'branch' | 'call' | 'return' | 'loop';
  location: TraceLocation;
  
  // For branches
  condition?: string;
  conditionResult?: boolean;
  
  // For calls
  calledProcedure?: string;
  parameters?: Record<string, unknown>;
  
  // For loops
  iterationCount?: number;
  
  // State at this point
  relevantVariables?: Record<string, unknown>;
}

export interface NormalizedDataSet {
  timestamp: number;
  variables: NormalizedVariable[];
  files: NormalizedFileState[];
  databases: NormalizedDBState[];
}

export interface NormalizedVariable {
  name: string;
  qualifiedName: string;
  type: string;
  value: unknown;
  previousValue?: unknown;
  changedAt?: number;
}

export interface NormalizedFileState {
  name: string;
  records: unknown[];
  cursor?: number;
}

export interface NormalizedDBState {
  table: string;
  operation: string;
  affectedRows?: unknown[];
}

export interface InferredRule {
  id: string;
  type: 'calculation' | 'validation' | 'decision' | 'transformation';
  description: string;
  confidence: number;
  
  // Location
  sourceLocations: TraceLocation[];
  
  // Pattern
  inputs: string[];
  outputs: string[];
  logic: string;
  formula?: string;
  
  // Evidence
  observationCount: number;
  exampleTraces: string[];
}

// ============================================================================
// MAINFRAME TRACER INTERFACE
// ============================================================================

export interface IMainframeTracer {
  readonly config: MainframeTraceConfig;
  
  // Lifecycle
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  isConnected(): boolean;
  
  // Trace capture
  startCapture(filter?: TransactionFilter | BatchJobFilter): Promise<string>;
  stopCapture(captureSessionId: string): Promise<MainframeExecutionTrace[]>;
  
  // Real-time streaming
  onTrace(callback: (trace: MainframeExecutionTrace) => void): void;
  offTrace(callback: (trace: MainframeExecutionTrace) => void): void;
  
  // Query
  getTrace(traceId: string): Promise<MainframeExecutionTrace | null>;
  queryTraces(filter: MainframeTraceFilter): Promise<MainframeExecutionTrace[]>;
  
  // Normalization
  normalizeTrace(trace: MainframeExecutionTrace): Promise<NormalizedTrace>;
  
  // Health
  getHealth(): Promise<MainframeTracerHealth>;
}

export interface MainframeTraceFilter {
  programName?: string;
  transactionId?: string;
  jobName?: string;
  userId?: string;
  startTimeAfter?: Date;
  startTimeBefore?: Date;
  minDuration?: number;
  maxDuration?: number;
  hasErrors?: boolean;
  limit?: number;
  offset?: number;
}

export interface MainframeTracerHealth {
  connected: boolean;
  platform: MainframePlatform;
  traceSource: MainframeTraceSource;
  
  // Stats
  tracesCollected: number;
  eventsCollected: number;
  bytesCollected: number;
  errorsEncountered: number;
  
  // Performance
  avgCollectionLatencyMs: number;
  eventsPerSecond: number;
  bufferUtilization: number;
  
  // Status
  lastTraceAt?: Date;
  lastErrorAt?: Date;
  lastError?: string;
}
