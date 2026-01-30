# @migrationpilot/tracing

Execution tracing, shadow testing, and observability for MigrationPilot.

## Overview

This package provides execution tracing capabilities for understanding legacy system behavior, shadow testing for validating migrations in production, and continuous learning from production traffic.

## Features

- **Execution Tracing**: Capture detailed execution paths
- **Shadow Testing**: Run modern code alongside legacy in production
- **Traffic Capture**: Record production inputs/outputs
- **Continuous Learning**: Improve test coverage from real data
- **Observability**: Metrics, logs, and distributed tracing

## Usage

### Execution Tracing

```typescript
import { ExecutionTracer } from '@migrationpilot/tracing';

const tracer = new ExecutionTracer();

// Trace a function execution
const trace = await tracer.trace(async () => {
  return await payrollService.calculate(employee);
}, {
  captureArguments: true,
  captureReturns: true,
  captureExceptions: true,
  maxDepth: 10,
});

// Analyze trace
console.log(`Call graph depth: ${trace.maxDepth}`);
console.log(`Total function calls: ${trace.calls.length}`);
console.log(`Unique functions: ${trace.uniqueFunctions.size}`);

// Visualize
const mermaid = trace.toMermaid();
console.log(mermaid);
```

### Shadow Testing

```typescript
import { ShadowTester } from '@migrationpilot/tracing';

const shadow = new ShadowTester({
  primary: legacySystem,
  shadow: modernSystem,
  comparisonStrategy: 'async', // Don't block on comparison
});

// Install as middleware
app.use('/api/payroll', shadow.middleware({
  sampleRate: 0.1,  // Shadow test 10% of requests
  logDifferences: true,
  alertOnMismatch: true,
}));

// Get shadow testing results
const results = await shadow.getResults({
  since: new Date('2024-01-01'),
  groupBy: 'endpoint',
});

console.log(`Shadow tests run: ${results.total}`);
console.log(`Mismatches: ${results.mismatches}`);
console.log(`Match rate: ${results.matchRate}%`);
```

### Traffic Capture

```typescript
import { TrafficCapture } from '@migrationpilot/tracing/capture';

const capture = new TrafficCapture({
  storage: 's3://migration-captures',
  encryption: true,
  sampling: {
    rate: 0.01,     // Capture 1% of traffic
    stratified: true, // Ensure coverage across endpoints
  },
  pii: {
    detect: true,
    mask: ['ssn', 'creditCard', 'email'],
  },
});

// Start capturing
capture.start({
  endpoints: ['/api/payroll/*', '/api/benefits/*'],
  duration: '24h',
});

// Export for test generation
const samples = await capture.export({
  count: 1000,
  diverse: true,  // Maximize input diversity
});
```

### Continuous Learning

```typescript
import { ContinuousLearner } from '@migrationpilot/tracing/learning';

const learner = new ContinuousLearner({
  feedbackLoop: true,
  minConfidence: 0.95,
});

// Learn from production traffic
await learner.learn({
  captures: await capture.getRecent(10000),
  businessRules: existingRules,
});

// Discover new rules
const newRules = learner.getDiscoveredRules();
console.log(`Discovered ${newRules.length} new business rules`);

// Generate new test cases
const newTests = learner.generateTestCases({
  coverageTarget: 0.95,
  prioritizeEdgeCases: true,
});
```

### OpenTelemetry Integration

```typescript
import { TracingProvider } from '@migrationpilot/tracing';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';

const tracing = new TracingProvider({
  serviceName: 'migrationpilot',
  exporter: new OTLPTraceExporter({
    url: 'http://jaeger:4318/v1/traces',
  }),
});

// Instrument migration operations
const span = tracing.startSpan('migration.analyze');
try {
  const result = await archeologist.analyze(code);
  span.setAttributes({
    'migration.rules_found': result.businessRules.length,
    'migration.confidence': result.confidence,
  });
} finally {
  span.end();
}
```

### Metrics Collection

```typescript
import { MetricsCollector } from '@migrationpilot/tracing';

const metrics = new MetricsCollector({
  prefix: 'migrationpilot',
  labels: { environment: 'production' },
});

// Track migration metrics
metrics.counter('rules_extracted_total').inc();
metrics.histogram('analysis_duration_seconds').observe(duration);
metrics.gauge('confidence_score').set(confidence);

// Expose Prometheus endpoint
app.get('/metrics', async (c) => {
  return c.text(await metrics.serialize());
});
```

## Trace Visualization

```typescript
import { TraceVisualizer } from '@migrationpilot/tracing';

const visualizer = new TraceVisualizer();

// Generate call graph
const callGraph = visualizer.renderCallGraph(trace, {
  format: 'mermaid',
  maxNodes: 50,
  highlightCriticalPath: true,
});

// Generate sequence diagram
const sequence = visualizer.renderSequenceDiagram(trace, {
  format: 'mermaid',
  includeTimings: true,
});

// Generate flame graph
const flame = visualizer.renderFlameGraph(trace, {
  format: 'svg',
  width: 1200,
  height: 600,
});
```

## Configuration

```typescript
interface TracingConfig {
  // Sampling
  sampling: {
    rate: number;
    rules: SamplingRule[];
  };
  
  // Storage
  storage: {
    type: 's3' | 'local' | 'database';
    retention: string;
    compression: boolean;
  };
  
  // Privacy
  privacy: {
    maskPII: boolean;
    piiPatterns: string[];
    excludeHeaders: string[];
  };
  
  // Alerting
  alerting: {
    mismatchThreshold: number;
    channels: AlertChannel[];
  };
}
```

## Installation

```bash
pnpm add @migrationpilot/tracing
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Watch mode
pnpm dev
```

## License

MIT
