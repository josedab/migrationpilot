/**
 * Mainframe Observability Bridge Types
 * Types for distributed tracing and metrics collection from mainframe systems
 */

// ============================================================================
// TRACE TYPES
// ============================================================================

export interface MainframeTrace {
  traceId: string;
  parentSpanId?: string;
  spanId: string;
  operationName: string;
  serviceName: string;
  subsystem: MainframeSubsystem;
  startTime: Date;
  endTime?: Date;
  duration?: number;
  status: TraceStatus;
  attributes: TraceAttributes;
  events: TraceEvent[];
  links: TraceLink[];
  resource: ResourceInfo;
}

export type MainframeSubsystem =
  | 'cics'
  | 'ims'
  | 'db2'
  | 'mq'
  | 'batch'
  | 'tso'
  | 'vtam'
  | 'vsam'
  | 'unknown';

export interface TraceStatus {
  code: StatusCode;
  message?: string;
  abendCode?: string;
}

export type StatusCode = 'OK' | 'ERROR' | 'UNSET';

export interface TraceAttributes {
  // Common attributes
  'mainframe.region'?: string;
  'mainframe.lpar'?: string;
  'mainframe.sysplex'?: string;
  'mainframe.job.name'?: string;
  'mainframe.job.id'?: string;
  'mainframe.user'?: string;

  // CICS attributes
  'cics.transaction.id'?: string;
  'cics.task.number'?: number;
  'cics.program.name'?: string;
  'cics.terminal.id'?: string;
  'cics.applid'?: string;
  'cics.response.code'?: number;
  'cics.response.name'?: string;
  'cics.abend.code'?: string;

  // IMS attributes
  'ims.transaction.code'?: string;
  'ims.pst.region'?: string;
  'ims.program.name'?: string;
  'ims.pcb.name'?: string;
  'ims.status.code'?: string;

  // DB2 attributes
  'db2.plan.name'?: string;
  'db2.package.name'?: string;
  'db2.statement.type'?: string;
  'db2.table.name'?: string;
  'db2.sqlcode'?: number;
  'db2.rows.affected'?: number;
  'db2.correlation.id'?: string;

  // MQ attributes
  'mq.queue.manager'?: string;
  'mq.queue.name'?: string;
  'mq.message.id'?: string;
  'mq.correlation.id'?: string;
  'mq.operation'?: string;

  // Batch attributes
  'batch.step.name'?: string;
  'batch.proc.name'?: string;
  'batch.dd.name'?: string;
  'batch.return.code'?: number;

  // Custom attributes
  [key: string]: string | number | boolean | undefined;
}

export interface TraceEvent {
  name: string;
  timestamp: Date;
  attributes?: Record<string, string | number | boolean>;
}

export interface TraceLink {
  traceId: string;
  spanId: string;
  type: LinkType;
  attributes?: Record<string, string | number | boolean>;
}

export type LinkType = 'parent' | 'follows_from' | 'child';

export interface ResourceInfo {
  'service.name': string;
  'service.version'?: string;
  'service.namespace'?: string;
  'host.name'?: string;
  'host.type'?: string;
  'os.type'?: string;
  'os.version'?: string;
  'telemetry.sdk.name'?: string;
  'telemetry.sdk.version'?: string;
  [key: string]: string | undefined;
}

// ============================================================================
// METRIC TYPES
// ============================================================================

export interface MainframeMetric {
  name: string;
  description: string;
  unit: string;
  type: MetricType;
  subsystem: MainframeSubsystem;
  dataPoints: MetricDataPoint[];
  resource: ResourceInfo;
}

export type MetricType = 'counter' | 'gauge' | 'histogram' | 'summary';

export interface MetricDataPoint {
  timestamp: Date;
  value: number;
  attributes?: Record<string, string | number | boolean>;
  // For histograms
  buckets?: HistogramBucket[];
  sum?: number;
  count?: number;
  min?: number;
  max?: number;
}

export interface HistogramBucket {
  boundary: number;
  count: number;
}

// Pre-defined mainframe metrics
export interface MainframeMetrics {
  // CICS metrics
  cicsTransactionCount: MainframeMetric;
  cicsTransactionDuration: MainframeMetric;
  cicsTaskCount: MainframeMetric;
  cicsAbendCount: MainframeMetric;
  cicsStorageUsed: MainframeMetric;
  cicsCpuTime: MainframeMetric;

  // IMS metrics
  imsTransactionCount: MainframeMetric;
  imsTransactionDuration: MainframeMetric;
  imsDbCallCount: MainframeMetric;
  imsQueueDepth: MainframeMetric;

  // DB2 metrics
  db2SqlCount: MainframeMetric;
  db2SqlDuration: MainframeMetric;
  db2BufferPoolHitRatio: MainframeMetric;
  db2Deadlocks: MainframeMetric;
  db2LockWaitTime: MainframeMetric;

  // MQ metrics
  mqMessageCount: MainframeMetric;
  mqQueueDepth: MainframeMetric;
  mqMessageAge: MainframeMetric;

  // Batch metrics
  batchJobCount: MainframeMetric;
  batchJobDuration: MainframeMetric;
  batchStepDuration: MainframeMetric;
  batchAbendCount: MainframeMetric;

  // System metrics
  cpuUtilization: MainframeMetric;
  memoryUsage: MainframeMetric;
  ioRate: MainframeMetric;
  pagingRate: MainframeMetric;
}

// ============================================================================
// LOG TYPES
// ============================================================================

export interface MainframeLog {
  timestamp: Date;
  severity: LogSeverity;
  body: string;
  traceId?: string;
  spanId?: string;
  subsystem: MainframeSubsystem;
  attributes: LogAttributes;
  resource: ResourceInfo;
}

export type LogSeverity =
  | 'TRACE'
  | 'DEBUG'
  | 'INFO'
  | 'WARN'
  | 'ERROR'
  | 'FATAL';

export interface LogAttributes {
  'log.source'?: string;
  'log.record.id'?: string;
  'exception.type'?: string;
  'exception.message'?: string;
  'exception.stacktrace'?: string;
  [key: string]: string | number | boolean | undefined;
}

// ============================================================================
// SMF RECORD TYPES (IBM System Management Facility)
// ============================================================================

export interface SmfRecord {
  type: SmfRecordType;
  subtype?: number;
  timestamp: Date;
  systemId: string;
  jobName: string;
  jobId: string;
  data: SmfRecordData;
}

export type SmfRecordType =
  | 30   // Common address space work
  | 70   // RMF processor activity
  | 71   // RMF paging activity
  | 72   // RMF workload activity
  | 74   // RMF device activity
  | 80   // RACF processing
  | 92   // File activity
  | 101  // DB2 accounting
  | 102  // DB2 statistics
  | 110  // CICS transaction
  | 111  // CICS performance
  | 116  // MQ accounting
  | number;

export type SmfRecordData =
  | SmfType30Data
  | SmfType101Data
  | SmfType110Data
  | SmfType116Data
  | Record<string, unknown>;

export interface SmfType30Data {
  intervalType: 'job' | 'step' | 'interval';
  stepName?: string;
  procStepName?: string;
  programName?: string;
  cpuTime: number;
  elapsedTime: number;
  ioCount: number;
  excpCount: number;
  returnCode?: number;
}

export interface SmfType101Data {
  planName: string;
  correlationId: string;
  authId: string;
  class1ElapsedTime: number;
  class2CpuTime: number;
  sqlCallCount: number;
  commitCount: number;
  abortCount: number;
  getpageCount: number;
  readCount: number;
  writeCount: number;
}

export interface SmfType110Data {
  transactionId: string;
  taskNumber: number;
  programName: string;
  terminalId?: string;
  userId?: string;
  responseTime: number;
  cpuTime: number;
  waitTime: number;
  dispatchCount: number;
  fileControlCalls: number;
  tsqCalls: number;
  tdqCalls: number;
  abendCode?: string;
}

export interface SmfType116Data {
  queueManager: string;
  queueName: string;
  putCount: number;
  getCount: number;
  browseCount: number;
  messageBytes: number;
  connectionId: string;
}

// ============================================================================
// OPENTELEMETRY EXPORT TYPES
// ============================================================================

export interface OtlpSpan {
  traceId: string;
  spanId: string;
  parentSpanId?: string;
  name: string;
  kind: SpanKind;
  startTimeUnixNano: string;
  endTimeUnixNano: string;
  attributes: OtlpAttribute[];
  events: OtlpEvent[];
  links: OtlpLink[];
  status: OtlpStatus;
}

export type SpanKind =
  | 'SPAN_KIND_UNSPECIFIED'
  | 'SPAN_KIND_INTERNAL'
  | 'SPAN_KIND_SERVER'
  | 'SPAN_KIND_CLIENT'
  | 'SPAN_KIND_PRODUCER'
  | 'SPAN_KIND_CONSUMER';

export interface OtlpAttribute {
  key: string;
  value: OtlpValue;
}

export interface OtlpValue {
  stringValue?: string;
  intValue?: string;
  doubleValue?: number;
  boolValue?: boolean;
  arrayValue?: { values: OtlpValue[] };
}

export interface OtlpEvent {
  name: string;
  timeUnixNano: string;
  attributes: OtlpAttribute[];
}

export interface OtlpLink {
  traceId: string;
  spanId: string;
  attributes: OtlpAttribute[];
}

export interface OtlpStatus {
  code: 'STATUS_CODE_UNSET' | 'STATUS_CODE_OK' | 'STATUS_CODE_ERROR';
  message?: string;
}

export interface OtlpMetric {
  name: string;
  description: string;
  unit: string;
  sum?: OtlpSum;
  gauge?: OtlpGauge;
  histogram?: OtlpHistogram;
}

export interface OtlpSum {
  dataPoints: OtlpNumberDataPoint[];
  aggregationTemporality: 'AGGREGATION_TEMPORALITY_DELTA' | 'AGGREGATION_TEMPORALITY_CUMULATIVE';
  isMonotonic: boolean;
}

export interface OtlpGauge {
  dataPoints: OtlpNumberDataPoint[];
}

export interface OtlpHistogram {
  dataPoints: OtlpHistogramDataPoint[];
  aggregationTemporality: 'AGGREGATION_TEMPORALITY_DELTA' | 'AGGREGATION_TEMPORALITY_CUMULATIVE';
}

export interface OtlpNumberDataPoint {
  attributes: OtlpAttribute[];
  startTimeUnixNano: string;
  timeUnixNano: string;
  asInt?: string;
  asDouble?: number;
}

export interface OtlpHistogramDataPoint {
  attributes: OtlpAttribute[];
  startTimeUnixNano: string;
  timeUnixNano: string;
  count: string;
  sum?: number;
  bucketCounts: string[];
  explicitBounds: number[];
  min?: number;
  max?: number;
}

// ============================================================================
// CORRELATION TYPES
// ============================================================================

export interface TraceCorrelation {
  modernTraceId: string;
  mainframeTraceId: string;
  correlationMethod: CorrelationMethod;
  confidence: number;
  timestamp: Date;
  metadata: CorrelationMetadata;
}

export type CorrelationMethod =
  | 'header-propagation'
  | 'correlation-id'
  | 'timestamp-matching'
  | 'transaction-id'
  | 'manual';

export interface CorrelationMetadata {
  modernService?: string;
  mainframeSubsystem?: MainframeSubsystem;
  transactionId?: string;
  messageId?: string;
  correlationId?: string;
  matchedFields?: string[];
}

// ============================================================================
// CONFIGURATION TYPES
// ============================================================================

export interface BridgeConfig {
  // Connection settings
  smfCollector: SmfCollectorConfig;
  otlpExporter: OtlpExporterConfig;

  // Processing settings
  batchSize: number;
  flushInterval: number;
  maxQueueSize: number;

  // Sampling
  sampling: SamplingConfig;

  // Filtering
  filters: FilterConfig;

  // Mapping
  attributeMapping: AttributeMappingConfig;
}

export interface SmfCollectorConfig {
  type: 'file' | 'realtime' | 'api';
  source: string;
  formats: SmfRecordType[];
  pollInterval?: number;
  credentials?: {
    userId?: string;
    password?: string;
    certificate?: string;
  };
}

export interface OtlpExporterConfig {
  endpoint: string;
  protocol: 'grpc' | 'http/protobuf' | 'http/json';
  headers?: Record<string, string>;
  compression?: 'gzip' | 'none';
  timeout?: number;
  retryConfig?: {
    maxRetries: number;
    initialBackoff: number;
    maxBackoff: number;
  };
}

export interface SamplingConfig {
  strategy: 'always_on' | 'always_off' | 'probability' | 'rate_limiting';
  probability?: number;
  rateLimit?: number;
  parentBased?: boolean;
}

export interface FilterConfig {
  includeSubsystems?: MainframeSubsystem[];
  excludeSubsystems?: MainframeSubsystem[];
  includeTransactions?: string[];
  excludeTransactions?: string[];
  minDuration?: number;
  errorOnly?: boolean;
}

export interface AttributeMappingConfig {
  customMappings: AttributeMapping[];
  includeDefaultMappings: boolean;
  dropUnmapped: boolean;
}

export interface AttributeMapping {
  source: string;
  target: string;
  transform?: 'lowercase' | 'uppercase' | 'trim' | 'number';
}

// ============================================================================
// SERVICE INTERFACE
// ============================================================================

export interface IObservabilityBridge {
  // Trace processing
  ingestTrace(trace: MainframeTrace): Promise<void>;
  ingestSmfRecord(record: SmfRecord): Promise<void>;
  exportTraces(): Promise<OtlpSpan[]>;

  // Metric processing
  recordMetric(metric: MainframeMetric): Promise<void>;
  exportMetrics(): Promise<OtlpMetric[]>;

  // Log processing
  ingestLog(log: MainframeLog): Promise<void>;

  // Correlation
  correlateTraces(modernTraceId: string, mainframeTraceId: string): Promise<TraceCorrelation>;
  findCorrelatedTraces(traceId: string): Promise<TraceCorrelation[]>;

  // Query
  queryTraces(filter: TraceQueryFilter): Promise<MainframeTrace[]>;
  queryMetrics(filter: MetricQueryFilter): Promise<MainframeMetric[]>;

  // Configuration
  configure(config: Partial<BridgeConfig>): void;
  getConfig(): BridgeConfig;
}

export interface TraceQueryFilter {
  traceId?: string;
  subsystem?: MainframeSubsystem;
  transactionId?: string;
  startTime?: Date;
  endTime?: Date;
  minDuration?: number;
  status?: StatusCode;
  limit?: number;
}

export interface MetricQueryFilter {
  name?: string;
  subsystem?: MainframeSubsystem;
  startTime?: Date;
  endTime?: Date;
  aggregation?: 'sum' | 'avg' | 'min' | 'max' | 'count';
}
