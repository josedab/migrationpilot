/**
 * Observability Bridge API Routes
 * Endpoints for mainframe distributed tracing and metrics
 */

import { Hono } from 'hono';
import {
  ObservabilityBridge,
  type MainframeTrace,
  type MainframeMetric,
  type MainframeLog,
  type SmfRecord,
  type TraceQueryFilter,
  type MetricQueryFilter,
  type BridgeConfig,
} from '@migrationpilot/observability-bridge';

const app = new Hono();

// Single shared bridge instance
const bridge = new ObservabilityBridge();

// ============================================================================
// TRACE ENDPOINTS
// ============================================================================

// Ingest a mainframe trace
app.post('/traces', async (c) => {
  const trace = await c.req.json<MainframeTrace>();

  // Ensure dates are properly parsed
  trace.startTime = new Date(trace.startTime);
  if (trace.endTime) {
    trace.endTime = new Date(trace.endTime);
  }

  await bridge.ingestTrace(trace);

  return c.json({
    status: 'accepted',
    traceId: trace.traceId,
    spanId: trace.spanId,
  });
});

// Batch ingest traces
app.post('/traces/batch', async (c) => {
  const body = await c.req.json<{ traces: MainframeTrace[] }>();

  const results = [];
  for (const trace of body.traces) {
    trace.startTime = new Date(trace.startTime);
    if (trace.endTime) {
      trace.endTime = new Date(trace.endTime);
    }
    await bridge.ingestTrace(trace);
    results.push({ traceId: trace.traceId, spanId: trace.spanId });
  }

  return c.json({
    status: 'accepted',
    count: results.length,
    traces: results,
  });
});

// Query traces
app.get('/traces', async (c) => {
  const filter: TraceQueryFilter = {
    traceId: c.req.query('traceId'),
    subsystem: c.req.query('subsystem') as TraceQueryFilter['subsystem'],
    transactionId: c.req.query('transactionId'),
    startTime: c.req.query('startTime') ? new Date(c.req.query('startTime')!) : undefined,
    endTime: c.req.query('endTime') ? new Date(c.req.query('endTime')!) : undefined,
    minDuration: c.req.query('minDuration') ? parseInt(c.req.query('minDuration')!, 10) : undefined,
    status: c.req.query('status') as TraceQueryFilter['status'],
    limit: c.req.query('limit') ? parseInt(c.req.query('limit')!, 10) : 100,
  };

  const traces = await bridge.queryTraces(filter);

  return c.json({
    count: traces.length,
    traces,
  });
});

// Export traces in OTLP format
app.get('/traces/export', async (c) => {
  const spans = await bridge.exportTraces();

  return c.json({
    resourceSpans: [{
      resource: {
        attributes: [
          { key: 'service.name', value: { stringValue: 'mainframe-bridge' } },
        ],
      },
      scopeSpans: [{
        scope: {
          name: 'migrationpilot-observability-bridge',
          version: '0.1.0',
        },
        spans,
      }],
    }],
  });
});

// ============================================================================
// SMF RECORD ENDPOINTS
// ============================================================================

// Ingest an SMF record
app.post('/smf', async (c) => {
  const record = await c.req.json<SmfRecord>();
  record.timestamp = new Date(record.timestamp);

  await bridge.ingestSmfRecord(record);

  return c.json({
    status: 'accepted',
    type: record.type,
    subtype: record.subtype,
    jobName: record.jobName,
  });
});

// Batch ingest SMF records
app.post('/smf/batch', async (c) => {
  const body = await c.req.json<{ records: SmfRecord[] }>();

  const results = [];
  for (const record of body.records) {
    record.timestamp = new Date(record.timestamp);
    await bridge.ingestSmfRecord(record);
    results.push({ type: record.type, jobName: record.jobName });
  }

  return c.json({
    status: 'accepted',
    count: results.length,
    records: results,
  });
});

// ============================================================================
// METRIC ENDPOINTS
// ============================================================================

// Record a metric
app.post('/metrics', async (c) => {
  const metric = await c.req.json<MainframeMetric>();

  // Parse timestamps
  for (const dp of metric.dataPoints) {
    dp.timestamp = new Date(dp.timestamp);
  }

  await bridge.recordMetric(metric);

  return c.json({
    status: 'accepted',
    name: metric.name,
    subsystem: metric.subsystem,
  });
});

// Query metrics
app.get('/metrics', async (c) => {
  const filter: MetricQueryFilter = {
    name: c.req.query('name'),
    subsystem: c.req.query('subsystem') as MetricQueryFilter['subsystem'],
    startTime: c.req.query('startTime') ? new Date(c.req.query('startTime')!) : undefined,
    endTime: c.req.query('endTime') ? new Date(c.req.query('endTime')!) : undefined,
    aggregation: c.req.query('aggregation') as MetricQueryFilter['aggregation'],
  };

  const metrics = await bridge.queryMetrics(filter);

  return c.json({
    count: metrics.length,
    metrics,
  });
});

// Export metrics in OTLP format
app.get('/metrics/export', async (c) => {
  const metrics = await bridge.exportMetrics();

  return c.json({
    resourceMetrics: [{
      resource: {
        attributes: [
          { key: 'service.name', value: { stringValue: 'mainframe-bridge' } },
        ],
      },
      scopeMetrics: [{
        scope: {
          name: 'migrationpilot-observability-bridge',
          version: '0.1.0',
        },
        metrics,
      }],
    }],
  });
});

// ============================================================================
// LOG ENDPOINTS
// ============================================================================

// Ingest a log entry
app.post('/logs', async (c) => {
  const log = await c.req.json<MainframeLog>();
  log.timestamp = new Date(log.timestamp);

  await bridge.ingestLog(log);

  return c.json({
    status: 'accepted',
    severity: log.severity,
    subsystem: log.subsystem,
  });
});

// Batch ingest logs
app.post('/logs/batch', async (c) => {
  const body = await c.req.json<{ logs: MainframeLog[] }>();

  for (const log of body.logs) {
    log.timestamp = new Date(log.timestamp);
    await bridge.ingestLog(log);
  }

  return c.json({
    status: 'accepted',
    count: body.logs.length,
  });
});

// ============================================================================
// CORRELATION ENDPOINTS
// ============================================================================

// Create a trace correlation
app.post('/correlations', async (c) => {
  const body = await c.req.json<{
    modernTraceId: string;
    mainframeTraceId: string;
  }>();

  const correlation = await bridge.correlateTraces(
    body.modernTraceId,
    body.mainframeTraceId
  );

  return c.json({
    status: 'created',
    correlation,
  });
});

// Find correlated traces
app.get('/correlations/:traceId', async (c) => {
  const traceId = c.req.param('traceId');
  const correlations = await bridge.findCorrelatedTraces(traceId);

  return c.json({
    traceId,
    count: correlations.length,
    correlations,
  });
});

// Auto-correlate based on correlation ID
app.post('/correlations/auto', async (c) => {
  const body = await c.req.json<{
    modernTraceId: string;
    correlationId: string;
  }>();

  const correlation = await bridge.autoCorrelate(
    body.modernTraceId,
    body.correlationId
  );

  if (!correlation) {
    return c.json({
      status: 'not_found',
      message: 'No matching mainframe trace found for correlation ID',
    }, 404);
  }

  return c.json({
    status: 'correlated',
    correlation,
  });
});

// ============================================================================
// CONFIGURATION ENDPOINTS
// ============================================================================

// Get current configuration
app.get('/config', (c) => {
  const config = bridge.getConfig();

  return c.json({
    config,
  });
});

// Update configuration
app.patch('/config', async (c) => {
  const updates = await c.req.json<Partial<BridgeConfig>>();

  bridge.configure(updates);

  return c.json({
    status: 'updated',
    config: bridge.getConfig(),
  });
});

// ============================================================================
// HEALTH & STATS
// ============================================================================

// Health check
app.get('/health', (c) => {
  return c.json({
    status: 'healthy',
    version: '0.1.0',
  });
});

// Get bridge statistics
app.get('/stats', async (c) => {
  const traces = await bridge.queryTraces({ limit: 1000 });
  const metrics = await bridge.queryMetrics({});

  // Calculate subsystem distribution
  const subsystemCounts: Record<string, number> = {};
  for (const trace of traces) {
    subsystemCounts[trace.subsystem] = (subsystemCounts[trace.subsystem] || 0) + 1;
  }

  // Calculate status distribution
  const statusCounts: Record<string, number> = { OK: 0, ERROR: 0, UNSET: 0 };
  for (const trace of traces) {
    statusCounts[trace.status.code] = (statusCounts[trace.status.code] || 0) + 1;
  }

  return c.json({
    traces: {
      total: traces.length,
      bySubsystem: subsystemCounts,
      byStatus: statusCounts,
    },
    metrics: {
      total: metrics.length,
      names: metrics.map(m => m.name),
    },
  });
});

export default app;
