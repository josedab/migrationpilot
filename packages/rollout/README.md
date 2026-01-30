# @migrationpilot/rollout

Progressive rollout orchestrator for safe migration deployments.

## Overview

This package provides tools for safely rolling out migrated code using patterns like Strangler Fig, canary deployments, and feature flags. It enables incremental migration with automatic rollback capabilities.

## Features

- **Strangler Fig Pattern**: Gradually route traffic from legacy to modern
- **Canary Deployments**: Test with a small percentage of traffic
- **Feature Flags**: Fine-grained control over migration features
- **Traffic Splitting**: Intelligent routing based on rules
- **Automatic Rollback**: Detect issues and revert automatically
- **Metrics & Monitoring**: Track rollout health

## Usage

### Strangler Fig Routing

```typescript
import { StranglerFigRouter } from '@migrationpilot/rollout';

const router = new StranglerFigRouter({
  legacy: {
    url: 'http://legacy-system:8080',
    healthCheck: '/health',
  },
  modern: {
    url: 'http://modern-system:8080',
    healthCheck: '/health',
  },
});

// Configure routing rules
router.addRule({
  endpoint: '/api/payroll/calculate',
  strategy: 'percentage',
  modernPercentage: 25,  // 25% to modern
  sticky: true,          // Same user always goes to same system
});

router.addRule({
  endpoint: '/api/benefits/*',
  strategy: 'all-modern',  // Fully migrated
});

router.addRule({
  endpoint: '/api/legacy/*',
  strategy: 'all-legacy',  // Not yet migrated
});

// Use as middleware
app.use(router.middleware());
```

### Canary Deployment

```typescript
import { CanaryDeployer } from '@migrationpilot/rollout';

const canary = new CanaryDeployer({
  stages: [
    { percentage: 1, duration: '1h', successThreshold: 99.9 },
    { percentage: 5, duration: '2h', successThreshold: 99.5 },
    { percentage: 25, duration: '4h', successThreshold: 99.0 },
    { percentage: 50, duration: '8h', successThreshold: 99.0 },
    { percentage: 100, duration: '24h', successThreshold: 99.0 },
  ],
  metrics: {
    errorRate: { max: 0.01 },
    latencyP99: { max: 500 },
    equivalenceScore: { min: 99.5 },
  },
});

// Start canary rollout
const deployment = await canary.start({
  service: 'payroll-service',
  version: 'v2.0.0',
});

// Monitor progress
deployment.on('stage-complete', (stage) => {
  console.log(`Completed stage: ${stage.percentage}%`);
});

deployment.on('rollback', (reason) => {
  console.log(`Rolling back: ${reason}`);
});

// Manual promotion/rollback
await deployment.promote();  // Move to next stage
await deployment.rollback(); // Revert to previous
```

### Feature Flags

```typescript
import { FeatureFlags } from '@migrationpilot/rollout';

const flags = new FeatureFlags({
  provider: 'launchdarkly', // or 'unleash', 'flagsmith', 'local'
  sdkKey: process.env.FEATURE_FLAG_KEY,
});

// Check feature flag
if (await flags.isEnabled('use-modern-payroll', { userId })) {
  return await modernPayroll.calculate(input);
} else {
  return await legacyPayroll.calculate(input);
}

// Gradual rollout with targeting
await flags.configure('use-modern-payroll', {
  enabled: true,
  rules: [
    { attribute: 'department', values: ['engineering'], percentage: 100 },
    { attribute: 'region', values: ['us-west'], percentage: 50 },
    { percentage: 10 },  // Default 10% for everyone else
  ],
});
```

### Traffic Comparison

```typescript
import { TrafficComparator } from '@migrationpilot/rollout';

const comparator = new TrafficComparator({
  parallel: true,  // Run both systems in parallel
  compare: true,   // Compare outputs
  primary: 'legacy', // Return legacy response
});

// Both systems receive traffic, outputs compared
app.post('/api/payroll', comparator.middleware({
  legacy: legacyHandler,
  modern: modernHandler,
  onMismatch: async (legacy, modern, request) => {
    await logMismatch(legacy, modern, request);
    await alertIfCritical(legacy, modern);
  },
}));

// Get comparison report
const report = await comparator.getReport({
  since: new Date('2024-01-01'),
  endpoint: '/api/payroll',
});

console.log(`Requests compared: ${report.total}`);
console.log(`Match rate: ${report.matchRate}%`);
```

### Automatic Rollback

```typescript
import { RollbackManager } from '@migrationpilot/rollout';

const rollback = new RollbackManager({
  triggers: [
    { metric: 'error_rate', threshold: 0.05, window: '5m' },
    { metric: 'latency_p99', threshold: 1000, window: '5m' },
    { metric: 'equivalence_score', threshold: 0.98, comparison: 'lt' },
  ],
  actions: [
    { type: 'alert', channels: ['slack', 'pagerduty'] },
    { type: 'rollback', automatic: true },
  ],
});

// Monitor and auto-rollback
rollback.monitor(deployment);

// Manual rollback
await rollback.execute({
  deployment: 'payroll-v2',
  reason: 'Manual rollback due to data inconsistency',
});
```

## Rollout Strategies

### Strangler Fig Pattern

```
┌──────────────────────────────────────────────────────────┐
│                      API Gateway                          │
│    ┌─────────────────────────────────────────────────┐   │
│    │             Strangler Fig Router                 │   │
│    └─────────────────────────────────────────────────┘   │
│                    │                │                     │
│                    ▼                ▼                     │
│    ┌─────────────────┐    ┌─────────────────┐           │
│    │  Legacy System  │    │  Modern System  │           │
│    │    (70%)        │    │    (30%)        │           │
│    └─────────────────┘    └─────────────────┘           │
└──────────────────────────────────────────────────────────┘
```

### Canary Stages

```
Stage 1:  ▓░░░░░░░░░░░░░░░░░░░  1%   (1 hour)
Stage 2:  ▓░░░░░░░░░░░░░░░░░░░  5%   (2 hours)
Stage 3:  ▓▓▓▓▓░░░░░░░░░░░░░░░  25%  (4 hours)
Stage 4:  ▓▓▓▓▓▓▓▓▓▓░░░░░░░░░░  50%  (8 hours)
Stage 5:  ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓  100% (24 hours)
```

## Configuration

```typescript
interface RolloutConfig {
  // Routing
  routing: {
    defaultStrategy: 'legacy' | 'modern' | 'split';
    stickyRouting: boolean;
    stickyKey: 'userId' | 'sessionId' | 'custom';
  };
  
  // Health checks
  healthCheck: {
    interval: number;
    timeout: number;
    unhealthyThreshold: number;
  };
  
  // Metrics
  metrics: {
    provider: 'prometheus' | 'datadog' | 'cloudwatch';
    sampleRate: number;
  };
  
  // Rollback
  rollback: {
    automatic: boolean;
    cooldownPeriod: number;
  };
}
```

## Installation

```bash
pnpm add @migrationpilot/rollout
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Lint
pnpm lint
```

## License

MIT
