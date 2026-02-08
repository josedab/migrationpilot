/**
 * Mainframe Observability Bridge
 * Collects, transforms, and exports mainframe telemetry to OpenTelemetry format
 */

import type {
  MainframeTrace,
  MainframeMetric,
  MainframeLog,
  SmfRecord,
  SmfType30Data,
  SmfType101Data,
  SmfType110Data,
  SmfType116Data,
  OtlpSpan,
  OtlpMetric,
  OtlpAttribute,
  OtlpEvent,
  OtlpStatus,
  SpanKind,
  TraceCorrelation,
  TraceQueryFilter,
  MetricQueryFilter,
  BridgeConfig,
  MainframeSubsystem,
  StatusCode,
  ResourceInfo,
  IObservabilityBridge,
  MetricDataPoint,
} from './types.js';

const DEFAULT_CONFIG: BridgeConfig = {
  smfCollector: {
    type: 'api',
    source: 'localhost',
    formats: [30, 101, 110, 116],
  },
  otlpExporter: {
    endpoint: 'http://localhost:4317',
    protocol: 'grpc',
    timeout: 30000,
  },
  batchSize: 100,
  flushInterval: 5000,
  maxQueueSize: 10000,
  sampling: {
    strategy: 'always_on',
  },
  filters: {},
  attributeMapping: {
    customMappings: [],
    includeDefaultMappings: true,
    dropUnmapped: false,
  },
};

export class ObservabilityBridge implements IObservabilityBridge {
  private config: BridgeConfig;
  private traces: Map<string, MainframeTrace> = new Map();
  private metrics: Map<string, MainframeMetric> = new Map();
  private logs: MainframeLog[] = [];
  private correlations: Map<string, TraceCorrelation[]> = new Map();
  private pendingSpans: OtlpSpan[] = [];
  private pendingMetrics: OtlpMetric[] = [];
  private flushTimer: ReturnType<typeof setInterval> | null = null;

  constructor(config: Partial<BridgeConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.startAutoFlush();
  }

  // ============================================================================
  // TRACE PROCESSING
  // ============================================================================

  async ingestTrace(trace: MainframeTrace): Promise<void> {
    // Apply filters
    if (!this.shouldProcessTrace(trace)) {
      return;
    }

    // Apply sampling
    if (!this.shouldSample(trace)) {
      return;
    }

    // Store the trace
    this.traces.set(trace.spanId, trace);

    // Convert to OTLP format
    const span = this.convertTraceToOtlp(trace);
    this.pendingSpans.push(span);

    // Flush if batch size reached
    if (this.pendingSpans.length >= this.config.batchSize) {
      await this.flushTraces();
    }
  }

  async ingestSmfRecord(record: SmfRecord): Promise<void> {
    // Convert SMF record to trace
    const trace = this.convertSmfToTrace(record);
    if (trace) {
      await this.ingestTrace(trace);
    }

    // Extract metrics from SMF record
    const metrics = this.extractMetricsFromSmf(record);
    for (const metric of metrics) {
      await this.recordMetric(metric);
    }
  }

  async exportTraces(): Promise<OtlpSpan[]> {
    const spans = [...this.pendingSpans];
    this.pendingSpans = [];
    return spans;
  }

  private shouldProcessTrace(trace: MainframeTrace): boolean {
    const { filters } = this.config;

    // Check subsystem filters
    if (filters.includeSubsystems && filters.includeSubsystems.length > 0) {
      if (!filters.includeSubsystems.includes(trace.subsystem)) {
        return false;
      }
    }

    if (filters.excludeSubsystems && filters.excludeSubsystems.length > 0) {
      if (filters.excludeSubsystems.includes(trace.subsystem)) {
        return false;
      }
    }

    // Check transaction filters
    const txnId = trace.attributes['cics.transaction.id'] ||
                  trace.attributes['ims.transaction.code'];
    if (txnId && filters.includeTransactions && filters.includeTransactions.length > 0) {
      if (!filters.includeTransactions.includes(String(txnId))) {
        return false;
      }
    }

    if (txnId && filters.excludeTransactions && filters.excludeTransactions.length > 0) {
      if (filters.excludeTransactions.includes(String(txnId))) {
        return false;
      }
    }

    // Check duration filter
    if (filters.minDuration && trace.duration && trace.duration < filters.minDuration) {
      return false;
    }

    // Check error filter
    if (filters.errorOnly && trace.status.code !== 'ERROR') {
      return false;
    }

    return true;
  }

  private shouldSample(trace: MainframeTrace): boolean {
    const { sampling } = this.config;

    switch (sampling.strategy) {
      case 'always_on':
        return true;
      case 'always_off':
        return false;
      case 'probability':
        return Math.random() < (sampling.probability || 1);
      case 'rate_limiting':
        // Simple rate limiting - would need proper implementation
        return true;
      default:
        return true;
    }
  }

  private convertTraceToOtlp(trace: MainframeTrace): OtlpSpan {
    const startTimeNano = BigInt(trace.startTime.getTime()) * BigInt(1000000);
    const endTimeNano = trace.endTime
      ? BigInt(trace.endTime.getTime()) * BigInt(1000000)
      : startTimeNano;

    return {
      traceId: trace.traceId,
      spanId: trace.spanId,
      parentSpanId: trace.parentSpanId,
      name: trace.operationName,
      kind: this.mapSpanKind(trace.subsystem),
      startTimeUnixNano: startTimeNano.toString(),
      endTimeUnixNano: endTimeNano.toString(),
      attributes: this.convertAttributes(trace.attributes),
      events: this.convertEvents(trace.events),
      links: trace.links.map(link => ({
        traceId: link.traceId,
        spanId: link.spanId,
        attributes: this.convertAttributes(link.attributes || {}),
      })),
      status: this.convertStatus(trace.status),
    };
  }

  private mapSpanKind(subsystem: MainframeSubsystem): SpanKind {
    switch (subsystem) {
      case 'cics':
      case 'ims':
        return 'SPAN_KIND_SERVER';
      case 'db2':
        return 'SPAN_KIND_CLIENT';
      case 'mq':
        return 'SPAN_KIND_PRODUCER';
      case 'batch':
        return 'SPAN_KIND_INTERNAL';
      default:
        return 'SPAN_KIND_INTERNAL';
    }
  }

  private convertAttributes(attrs: Record<string, string | number | boolean | undefined>): OtlpAttribute[] {
    const result: OtlpAttribute[] = [];

    for (const [key, value] of Object.entries(attrs)) {
      if (value === undefined) continue;

      const otlpAttr: OtlpAttribute = { key, value: {} };

      if (typeof value === 'string') {
        otlpAttr.value = { stringValue: value };
      } else if (typeof value === 'number') {
        if (Number.isInteger(value)) {
          otlpAttr.value = { intValue: value.toString() };
        } else {
          otlpAttr.value = { doubleValue: value };
        }
      } else if (typeof value === 'boolean') {
        otlpAttr.value = { boolValue: value };
      }

      result.push(otlpAttr);
    }

    // Apply custom attribute mappings
    for (const mapping of this.config.attributeMapping.customMappings) {
      const sourceAttr = result.find(a => a.key === mapping.source);
      if (sourceAttr) {
        const mappedAttr: OtlpAttribute = {
          key: mapping.target,
          value: { ...sourceAttr.value },
        };

        // Apply transform if specified
        if (mapping.transform && mappedAttr.value.stringValue) {
          switch (mapping.transform) {
            case 'lowercase':
              mappedAttr.value.stringValue = mappedAttr.value.stringValue.toLowerCase();
              break;
            case 'uppercase':
              mappedAttr.value.stringValue = mappedAttr.value.stringValue.toUpperCase();
              break;
            case 'trim':
              mappedAttr.value.stringValue = mappedAttr.value.stringValue.trim();
              break;
            case 'number':
              const num = parseFloat(mappedAttr.value.stringValue);
              if (!isNaN(num)) {
                mappedAttr.value = { doubleValue: num };
              }
              break;
          }
        }

        result.push(mappedAttr);
      }
    }

    return result;
  }

  private convertEvents(events: MainframeTrace['events']): OtlpEvent[] {
    return events.map(event => ({
      name: event.name,
      timeUnixNano: (BigInt(event.timestamp.getTime()) * BigInt(1000000)).toString(),
      attributes: this.convertAttributes(event.attributes || {}),
    }));
  }

  private convertStatus(status: MainframeTrace['status']): OtlpStatus {
    const codeMap: Record<StatusCode, OtlpStatus['code']> = {
      OK: 'STATUS_CODE_OK',
      ERROR: 'STATUS_CODE_ERROR',
      UNSET: 'STATUS_CODE_UNSET',
    };

    return {
      code: codeMap[status.code],
      message: status.message || status.abendCode,
    };
  }

  private convertSmfToTrace(record: SmfRecord): MainframeTrace | null {
    switch (record.type) {
      case 110:
        return this.convertCicsSmfToTrace(record);
      case 101:
        return this.convertDb2SmfToTrace(record);
      case 116:
        return this.convertMqSmfToTrace(record);
      case 30:
        return this.convertBatchSmfToTrace(record);
      default:
        return null;
    }
  }

  private convertCicsSmfToTrace(record: SmfRecord): MainframeTrace {
    const data = record.data as SmfType110Data;
    const traceId = this.generateTraceId();
    const spanId = this.generateSpanId();

    return {
      traceId,
      spanId,
      operationName: `CICS:${data.transactionId}`,
      serviceName: 'cics-region',
      subsystem: 'cics',
      startTime: record.timestamp,
      endTime: new Date(record.timestamp.getTime() + data.responseTime),
      duration: data.responseTime,
      status: {
        code: data.abendCode ? 'ERROR' : 'OK',
        abendCode: data.abendCode,
      },
      attributes: {
        'mainframe.job.name': record.jobName,
        'mainframe.job.id': record.jobId,
        'cics.transaction.id': data.transactionId,
        'cics.task.number': data.taskNumber,
        'cics.program.name': data.programName,
        'cics.terminal.id': data.terminalId,
        'mainframe.user': data.userId,
        'cics.abend.code': data.abendCode,
      },
      events: [],
      links: [],
      resource: this.createResource('cics-region', record.systemId),
    };
  }

  private convertDb2SmfToTrace(record: SmfRecord): MainframeTrace {
    const data = record.data as SmfType101Data;
    const traceId = this.generateTraceId();
    const spanId = this.generateSpanId();

    return {
      traceId,
      spanId,
      operationName: `DB2:${data.planName}`,
      serviceName: 'db2-subsystem',
      subsystem: 'db2',
      startTime: record.timestamp,
      endTime: new Date(record.timestamp.getTime() + data.class1ElapsedTime),
      duration: data.class1ElapsedTime,
      status: {
        code: data.abortCount > 0 ? 'ERROR' : 'OK',
      },
      attributes: {
        'mainframe.job.name': record.jobName,
        'mainframe.job.id': record.jobId,
        'db2.plan.name': data.planName,
        'db2.correlation.id': data.correlationId,
        'mainframe.user': data.authId,
      },
      events: [],
      links: [],
      resource: this.createResource('db2-subsystem', record.systemId),
    };
  }

  private convertMqSmfToTrace(record: SmfRecord): MainframeTrace {
    const data = record.data as SmfType116Data;
    const traceId = this.generateTraceId();
    const spanId = this.generateSpanId();

    return {
      traceId,
      spanId,
      operationName: `MQ:${data.queueName}`,
      serviceName: data.queueManager,
      subsystem: 'mq',
      startTime: record.timestamp,
      status: { code: 'OK' },
      attributes: {
        'mainframe.job.name': record.jobName,
        'mainframe.job.id': record.jobId,
        'mq.queue.manager': data.queueManager,
        'mq.queue.name': data.queueName,
      },
      events: [],
      links: [],
      resource: this.createResource(data.queueManager, record.systemId),
    };
  }

  private convertBatchSmfToTrace(record: SmfRecord): MainframeTrace {
    const data = record.data as SmfType30Data;
    const traceId = this.generateTraceId();
    const spanId = this.generateSpanId();

    return {
      traceId,
      spanId,
      operationName: `BATCH:${record.jobName}${data.stepName ? '.' + data.stepName : ''}`,
      serviceName: 'batch-job',
      subsystem: 'batch',
      startTime: record.timestamp,
      endTime: new Date(record.timestamp.getTime() + data.elapsedTime),
      duration: data.elapsedTime,
      status: {
        code: data.returnCode !== undefined && data.returnCode > 4 ? 'ERROR' : 'OK',
      },
      attributes: {
        'mainframe.job.name': record.jobName,
        'mainframe.job.id': record.jobId,
        'batch.step.name': data.stepName,
        'batch.proc.name': data.procStepName,
        'batch.return.code': data.returnCode,
      },
      events: [],
      links: [],
      resource: this.createResource('batch-job', record.systemId),
    };
  }

  // ============================================================================
  // METRIC PROCESSING
  // ============================================================================

  async recordMetric(metric: MainframeMetric): Promise<void> {
    const key = `${metric.name}:${metric.subsystem}`;
    const existing = this.metrics.get(key);

    if (existing) {
      // Merge data points
      existing.dataPoints.push(...metric.dataPoints);
    } else {
      this.metrics.set(key, metric);
    }

    // Convert to OTLP and queue
    const otlpMetric = this.convertMetricToOtlp(metric);
    this.pendingMetrics.push(otlpMetric);
  }

  async exportMetrics(): Promise<OtlpMetric[]> {
    const metrics = [...this.pendingMetrics];
    this.pendingMetrics = [];
    return metrics;
  }

  private convertMetricToOtlp(metric: MainframeMetric): OtlpMetric {
    const dataPoints = metric.dataPoints.map(dp => this.convertDataPoint(dp));

    const otlpMetric: OtlpMetric = {
      name: metric.name,
      description: metric.description,
      unit: metric.unit,
    };

    switch (metric.type) {
      case 'counter':
        otlpMetric.sum = {
          dataPoints,
          aggregationTemporality: 'AGGREGATION_TEMPORALITY_CUMULATIVE',
          isMonotonic: true,
        };
        break;
      case 'gauge':
        otlpMetric.gauge = { dataPoints };
        break;
      case 'histogram':
        otlpMetric.histogram = {
          dataPoints: metric.dataPoints.map(dp => this.convertHistogramDataPoint(dp)),
          aggregationTemporality: 'AGGREGATION_TEMPORALITY_DELTA',
        };
        break;
    }

    return otlpMetric;
  }

  private convertDataPoint(dp: MetricDataPoint) {
    return {
      attributes: this.convertAttributes(dp.attributes || {}),
      startTimeUnixNano: (BigInt(dp.timestamp.getTime()) * BigInt(1000000)).toString(),
      timeUnixNano: (BigInt(dp.timestamp.getTime()) * BigInt(1000000)).toString(),
      asDouble: dp.value,
    };
  }

  private convertHistogramDataPoint(dp: MetricDataPoint) {
    return {
      attributes: this.convertAttributes(dp.attributes || {}),
      startTimeUnixNano: (BigInt(dp.timestamp.getTime()) * BigInt(1000000)).toString(),
      timeUnixNano: (BigInt(dp.timestamp.getTime()) * BigInt(1000000)).toString(),
      count: String(dp.count || 0),
      sum: dp.sum,
      bucketCounts: (dp.buckets || []).map(b => String(b.count)),
      explicitBounds: (dp.buckets || []).map(b => b.boundary),
      min: dp.min,
      max: dp.max,
    };
  }

  private extractMetricsFromSmf(record: SmfRecord): MainframeMetric[] {
    const metrics: MainframeMetric[] = [];
    const timestamp = record.timestamp;
    const resource = this.createResource('mainframe', record.systemId);

    switch (record.type) {
      case 110: {
        const data = record.data as SmfType110Data;
        metrics.push({
          name: 'cics.transaction.duration',
          description: 'CICS transaction response time',
          unit: 'ms',
          type: 'histogram',
          subsystem: 'cics',
          dataPoints: [{
            timestamp,
            value: data.responseTime,
            attributes: { 'cics.transaction.id': data.transactionId },
          }],
          resource,
        });
        metrics.push({
          name: 'cics.cpu.time',
          description: 'CICS CPU time used',
          unit: 'ms',
          type: 'counter',
          subsystem: 'cics',
          dataPoints: [{
            timestamp,
            value: data.cpuTime,
            attributes: { 'cics.transaction.id': data.transactionId },
          }],
          resource,
        });
        break;
      }
      case 101: {
        const data = record.data as SmfType101Data;
        metrics.push({
          name: 'db2.sql.calls',
          description: 'Number of SQL calls',
          unit: '{calls}',
          type: 'counter',
          subsystem: 'db2',
          dataPoints: [{
            timestamp,
            value: data.sqlCallCount,
            attributes: { 'db2.plan.name': data.planName },
          }],
          resource,
        });
        metrics.push({
          name: 'db2.elapsed.time',
          description: 'DB2 class 1 elapsed time',
          unit: 'ms',
          type: 'histogram',
          subsystem: 'db2',
          dataPoints: [{
            timestamp,
            value: data.class1ElapsedTime,
            attributes: { 'db2.plan.name': data.planName },
          }],
          resource,
        });
        break;
      }
      case 116: {
        const data = record.data as SmfType116Data;
        metrics.push({
          name: 'mq.messages.put',
          description: 'Number of messages put',
          unit: '{messages}',
          type: 'counter',
          subsystem: 'mq',
          dataPoints: [{
            timestamp,
            value: data.putCount,
            attributes: {
              'mq.queue.manager': data.queueManager,
              'mq.queue.name': data.queueName,
            },
          }],
          resource,
        });
        metrics.push({
          name: 'mq.messages.get',
          description: 'Number of messages gotten',
          unit: '{messages}',
          type: 'counter',
          subsystem: 'mq',
          dataPoints: [{
            timestamp,
            value: data.getCount,
            attributes: {
              'mq.queue.manager': data.queueManager,
              'mq.queue.name': data.queueName,
            },
          }],
          resource,
        });
        break;
      }
    }

    return metrics;
  }

  // ============================================================================
  // LOG PROCESSING
  // ============================================================================

  async ingestLog(log: MainframeLog): Promise<void> {
    this.logs.push(log);

    // Would export to log collector in production
  }

  // ============================================================================
  // CORRELATION
  // ============================================================================

  async correlateTraces(
    modernTraceId: string,
    mainframeTraceId: string
  ): Promise<TraceCorrelation> {
    const correlation: TraceCorrelation = {
      modernTraceId,
      mainframeTraceId,
      correlationMethod: 'manual',
      confidence: 1.0,
      timestamp: new Date(),
      metadata: {},
    };

    // Store correlation
    const existing = this.correlations.get(modernTraceId) || [];
    existing.push(correlation);
    this.correlations.set(modernTraceId, existing);

    // Also index by mainframe trace ID
    const mainframeExisting = this.correlations.get(mainframeTraceId) || [];
    mainframeExisting.push(correlation);
    this.correlations.set(mainframeTraceId, mainframeExisting);

    return correlation;
  }

  async findCorrelatedTraces(traceId: string): Promise<TraceCorrelation[]> {
    return this.correlations.get(traceId) || [];
  }

  // Auto-correlation based on correlation IDs
  async autoCorrelate(modernTraceId: string, correlationId: string): Promise<TraceCorrelation | null> {
    // Search for mainframe traces with matching correlation ID
    for (const trace of this.traces.values()) {
      const mfCorrelationId =
        trace.attributes['db2.correlation.id'] ||
        trace.attributes['mq.correlation.id'];

      if (mfCorrelationId === correlationId) {
        const correlation = await this.correlateTraces(modernTraceId, trace.traceId);
        correlation.correlationMethod = 'correlation-id';
        correlation.metadata.correlationId = correlationId;
        return correlation;
      }
    }

    return null;
  }

  // ============================================================================
  // QUERY
  // ============================================================================

  async queryTraces(filter: TraceQueryFilter): Promise<MainframeTrace[]> {
    let results = Array.from(this.traces.values());

    if (filter.traceId) {
      results = results.filter(t => t.traceId === filter.traceId);
    }

    if (filter.subsystem) {
      results = results.filter(t => t.subsystem === filter.subsystem);
    }

    if (filter.transactionId) {
      results = results.filter(t => {
        const txnId = t.attributes['cics.transaction.id'] ||
                      t.attributes['ims.transaction.code'];
        return txnId === filter.transactionId;
      });
    }

    if (filter.startTime) {
      results = results.filter(t => t.startTime >= filter.startTime!);
    }

    if (filter.endTime) {
      results = results.filter(t => t.startTime <= filter.endTime!);
    }

    if (filter.minDuration) {
      results = results.filter(t => t.duration && t.duration >= filter.minDuration!);
    }

    if (filter.status) {
      results = results.filter(t => t.status.code === filter.status);
    }

    if (filter.limit) {
      results = results.slice(0, filter.limit);
    }

    return results;
  }

  async queryMetrics(filter: MetricQueryFilter): Promise<MainframeMetric[]> {
    let results = Array.from(this.metrics.values());

    if (filter.name) {
      results = results.filter(m => m.name === filter.name);
    }

    if (filter.subsystem) {
      results = results.filter(m => m.subsystem === filter.subsystem);
    }

    if (filter.startTime || filter.endTime) {
      results = results.map(m => ({
        ...m,
        dataPoints: m.dataPoints.filter(dp => {
          if (filter.startTime && dp.timestamp < filter.startTime) return false;
          if (filter.endTime && dp.timestamp > filter.endTime) return false;
          return true;
        }),
      })).filter(m => m.dataPoints.length > 0);
    }

    return results;
  }

  // ============================================================================
  // CONFIGURATION
  // ============================================================================

  configure(config: Partial<BridgeConfig>): void {
    this.config = { ...this.config, ...config };

    // Restart auto-flush with new interval
    this.stopAutoFlush();
    this.startAutoFlush();
  }

  getConfig(): BridgeConfig {
    return { ...this.config };
  }

  // ============================================================================
  // UTILITIES
  // ============================================================================

  private generateTraceId(): string {
    return Array.from({ length: 32 }, () =>
      Math.floor(Math.random() * 16).toString(16)
    ).join('');
  }

  private generateSpanId(): string {
    return Array.from({ length: 16 }, () =>
      Math.floor(Math.random() * 16).toString(16)
    ).join('');
  }

  private createResource(serviceName: string, hostName?: string): ResourceInfo {
    return {
      'service.name': serviceName,
      'service.namespace': 'mainframe',
      'host.name': hostName,
      'host.type': 'mainframe',
      'os.type': 'z/OS',
      'telemetry.sdk.name': 'migrationpilot-observability-bridge',
      'telemetry.sdk.version': '0.1.0',
    };
  }

  private startAutoFlush(): void {
    if (this.config.flushInterval > 0) {
      this.flushTimer = setInterval(() => {
        this.flush();
      }, this.config.flushInterval);
    }
  }

  private stopAutoFlush(): void {
    if (this.flushTimer) {
      clearInterval(this.flushTimer);
      this.flushTimer = null;
    }
  }

  private async flush(): Promise<void> {
    await this.flushTraces();
    await this.flushMetrics();
  }

  private async flushTraces(): Promise<void> {
    if (this.pendingSpans.length === 0) return;

    // In production, would send to OTLP endpoint
    // const spans = await this.exportTraces();
    // await this.sendToOtlp(spans);
  }

  private async flushMetrics(): Promise<void> {
    if (this.pendingMetrics.length === 0) return;

    // In production, would send to OTLP endpoint
    // const metrics = await this.exportMetrics();
    // await this.sendToOtlp(metrics);
  }

  // Clean up
  destroy(): void {
    this.stopAutoFlush();
  }
}
