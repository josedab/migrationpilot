/**
 * Prometheus Metrics Endpoint
 * 
 * Exposes application metrics in OpenMetrics/Prometheus format
 */

import { Hono } from 'hono';

// Metric types (MetricValue used for future type reference)
type MetricType = 'counter' | 'gauge' | 'histogram' | 'summary';

interface Metric {
  name: string;
  help: string;
  type: MetricType;
  labelNames: string[];
}

// Type definition for metric values (exported for future use)
export interface MetricValue {
  labels: Record<string, string>;
  value: number;
  timestamp?: number;
}

interface HistogramBucket {
  le: number;
  count: number;
}

interface HistogramValue {
  labels: Record<string, string>;
  buckets: HistogramBucket[];
  sum: number;
  count: number;
}

// In-memory metric storage
class MetricsRegistry {
  private counters = new Map<string, Map<string, number>>();
  private gauges = new Map<string, Map<string, number>>();
  private histograms = new Map<string, Map<string, HistogramValue>>();
  private metrics = new Map<string, Metric>();

  // Default histogram buckets
  private defaultBuckets = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10];

  registerCounter(name: string, help: string, labelNames: string[] = []): void {
    this.metrics.set(name, { name, help, type: 'counter', labelNames });
    this.counters.set(name, new Map());
  }

  registerGauge(name: string, help: string, labelNames: string[] = []): void {
    this.metrics.set(name, { name, help, type: 'gauge', labelNames });
    this.gauges.set(name, new Map());
  }

  registerHistogram(name: string, help: string, labelNames: string[] = []): void {
    this.metrics.set(name, { name, help, type: 'histogram', labelNames });
    this.histograms.set(name, new Map());
  }

  incrementCounter(name: string, labels: Record<string, string> = {}, value = 1): void {
    const counter = this.counters.get(name);
    if (!counter) return;
    
    const key = this.labelsKey(labels);
    counter.set(key, (counter.get(key) || 0) + value);
  }

  setGauge(name: string, value: number, labels: Record<string, string> = {}): void {
    const gauge = this.gauges.get(name);
    if (!gauge) return;
    
    const key = this.labelsKey(labels);
    gauge.set(key, value);
  }

  observeHistogram(name: string, value: number, labels: Record<string, string> = {}): void {
    const histogram = this.histograms.get(name);
    if (!histogram) return;
    
    const key = this.labelsKey(labels);
    let hist = histogram.get(key);
    
    if (!hist) {
      hist = {
        labels,
        buckets: this.defaultBuckets.map(le => ({ le, count: 0 })),
        sum: 0,
        count: 0,
      };
      histogram.set(key, hist);
    }
    
    hist.sum += value;
    hist.count += 1;
    
    for (const bucket of hist.buckets) {
      if (value <= bucket.le) {
        bucket.count += 1;
      }
    }
  }

  private labelsKey(labels: Record<string, string>): string {
    return Object.entries(labels)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}="${v}"`)
      .join(',');
  }

  formatLabels(labels: Record<string, string>): string {
    const entries = Object.entries(labels);
    if (entries.length === 0) return '';
    return `{${entries.map(([k, v]) => `${k}="${v}"`).join(',')}}`;
  }

  export(): string {
    const lines: string[] = [];

    // Export counters
    for (const [name, values] of this.counters) {
      const metric = this.metrics.get(name)!;
      lines.push(`# HELP ${name} ${metric.help}`);
      lines.push(`# TYPE ${name} counter`);
      
      for (const [key, value] of values) {
        const labels = key ? `{${key}}` : '';
        lines.push(`${name}${labels} ${value}`);
      }
    }

    // Export gauges
    for (const [name, values] of this.gauges) {
      const metric = this.metrics.get(name)!;
      lines.push(`# HELP ${name} ${metric.help}`);
      lines.push(`# TYPE ${name} gauge`);
      
      for (const [key, value] of values) {
        const labels = key ? `{${key}}` : '';
        lines.push(`${name}${labels} ${value}`);
      }
    }

    // Export histograms
    for (const [name, values] of this.histograms) {
      const metric = this.metrics.get(name)!;
      lines.push(`# HELP ${name} ${metric.help}`);
      lines.push(`# TYPE ${name} histogram`);
      
      for (const [, hist] of values) {
        const labelStr = this.formatLabels(hist.labels);
        const baseLabels = labelStr ? labelStr.slice(0, -1) + ',' : '{';
        
        for (const bucket of hist.buckets) {
          lines.push(`${name}_bucket${baseLabels}le="${bucket.le}"} ${bucket.count}`);
        }
        lines.push(`${name}_bucket${baseLabels}le="+Inf"} ${hist.count}`);
        lines.push(`${name}_sum${labelStr} ${hist.sum}`);
        lines.push(`${name}_count${labelStr} ${hist.count}`);
      }
    }

    return lines.join('\n');
  }
}

// Global registry
export const metricsRegistry = new MetricsRegistry();

// Register default metrics
metricsRegistry.registerCounter(
  'migrationpilot_requests_total',
  'Total number of HTTP requests',
  ['method', 'path', 'status']
);

metricsRegistry.registerHistogram(
  'migrationpilot_request_duration_seconds',
  'HTTP request duration in seconds',
  ['method', 'path']
);

metricsRegistry.registerGauge(
  'migrationpilot_active_migrations',
  'Number of currently active migrations',
  []
);

metricsRegistry.registerCounter(
  'migrationpilot_rules_extracted_total',
  'Total business rules extracted',
  ['language', 'confidence']
);

metricsRegistry.registerCounter(
  'migrationpilot_lines_parsed_total',
  'Total lines of legacy code parsed',
  ['language']
);

metricsRegistry.registerCounter(
  'migrationpilot_lines_generated_total',
  'Total lines of modern code generated',
  ['language']
);

metricsRegistry.registerCounter(
  'migrationpilot_tests_executed_total',
  'Total equivalence tests executed',
  ['result']
);

metricsRegistry.registerGauge(
  'migrationpilot_equivalence_rate',
  'Current equivalence test pass rate',
  ['project_id']
);

metricsRegistry.registerHistogram(
  'migrationpilot_agent_duration_seconds',
  'AI agent execution duration',
  ['agent']
);

metricsRegistry.registerCounter(
  'migrationpilot_agent_errors_total',
  'Total AI agent errors',
  ['agent', 'error_type']
);

// Metrics routes
export const metricsRoutes = new Hono();

metricsRoutes.get('/', (c) => {
  const metrics = metricsRegistry.export();
  return c.text(metrics, 200, {
    'Content-Type': 'text/plain; version=0.0.4; charset=utf-8',
  });
});

metricsRoutes.get('/health', (c) => {
  return c.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
  });
});

// Middleware to track request metrics
export function metricsMiddleware() {
  return async (c: { req: { method: string; path: string }; res: { status: number } }, next: () => Promise<void>) => {
    const start = Date.now();
    const method = c.req.method;
    const path = c.req.path;

    try {
      await next();
    } finally {
      const duration = (Date.now() - start) / 1000;
      const status = c.res.status.toString();

      metricsRegistry.incrementCounter('migrationpilot_requests_total', {
        method,
        path: normalizePath(path),
        status,
      });

      metricsRegistry.observeHistogram('migrationpilot_request_duration_seconds', duration, {
        method,
        path: normalizePath(path),
      });
    }
  };
}

// Normalize path to avoid high cardinality
function normalizePath(path: string): string {
  // Replace UUIDs with placeholder
  let normalized = path.replace(
    /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/gi,
    ':id'
  );
  
  // Replace numeric IDs with placeholder
  normalized = normalized.replace(/\/\d+/g, '/:id');
  
  return normalized;
}

// Helper functions for recording metrics
export const metrics = {
  recordRequest(method: string, path: string, status: number, durationMs: number): void {
    metricsRegistry.incrementCounter('migrationpilot_requests_total', {
      method,
      path: normalizePath(path),
      status: status.toString(),
    });
    metricsRegistry.observeHistogram('migrationpilot_request_duration_seconds', durationMs / 1000, {
      method,
      path: normalizePath(path),
    });
  },

  recordRuleExtraction(language: string, confidence: 'high' | 'medium' | 'low'): void {
    metricsRegistry.incrementCounter('migrationpilot_rules_extracted_total', {
      language,
      confidence,
    });
  },

  recordLinesParsed(language: string, count: number): void {
    metricsRegistry.incrementCounter('migrationpilot_lines_parsed_total', { language }, count);
  },

  recordLinesGenerated(language: string, count: number): void {
    metricsRegistry.incrementCounter('migrationpilot_lines_generated_total', { language }, count);
  },

  recordTestExecution(passed: boolean): void {
    metricsRegistry.incrementCounter('migrationpilot_tests_executed_total', {
      result: passed ? 'passed' : 'failed',
    });
  },

  setEquivalenceRate(projectId: string, rate: number): void {
    metricsRegistry.setGauge('migrationpilot_equivalence_rate', rate, { project_id: projectId });
  },

  setActiveMigrations(count: number): void {
    metricsRegistry.setGauge('migrationpilot_active_migrations', count);
  },

  recordAgentDuration(agent: string, durationMs: number): void {
    metricsRegistry.observeHistogram('migrationpilot_agent_duration_seconds', durationMs / 1000, {
      agent,
    });
  },

  recordAgentError(agent: string, errorType: string): void {
    metricsRegistry.incrementCounter('migrationpilot_agent_errors_total', {
      agent,
      error_type: errorType,
    });
  },
};
