/**
 * Mainframe Observability Bridge Package
 * Distributed tracing and metrics collection from mainframe systems
 */

// Main export
export { ObservabilityBridge } from './bridge.js';

// Type exports
export type {
  // Trace types
  MainframeTrace,
  MainframeSubsystem,
  TraceStatus,
  StatusCode,
  TraceAttributes,
  TraceEvent,
  TraceLink,
  LinkType,
  ResourceInfo,
  // Metric types
  MainframeMetric,
  MetricType,
  MetricDataPoint,
  HistogramBucket,
  MainframeMetrics,
  // Log types
  MainframeLog,
  LogSeverity,
  LogAttributes,
  // SMF types
  SmfRecord,
  SmfRecordType,
  SmfRecordData,
  SmfType30Data,
  SmfType101Data,
  SmfType110Data,
  SmfType116Data,
  // OTLP types
  OtlpSpan,
  SpanKind,
  OtlpAttribute,
  OtlpValue,
  OtlpEvent,
  OtlpLink,
  OtlpStatus,
  OtlpMetric,
  OtlpSum,
  OtlpGauge,
  OtlpHistogram,
  OtlpNumberDataPoint,
  OtlpHistogramDataPoint,
  // Correlation types
  TraceCorrelation,
  CorrelationMethod,
  CorrelationMetadata,
  // Configuration types
  BridgeConfig,
  SmfCollectorConfig,
  OtlpExporterConfig,
  SamplingConfig,
  FilterConfig,
  AttributeMappingConfig,
  AttributeMapping,
  // Query types
  TraceQueryFilter,
  MetricQueryFilter,
  // Service interface
  IObservabilityBridge,
} from './types.js';
