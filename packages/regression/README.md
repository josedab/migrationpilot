# @migrationpilot/regression

Automated regression suite management for generated test suites.

## Overview

This package provides tools for managing and maintaining regression test suites, ensuring that migrated code continues to work correctly as it evolves.

## Features

- **Suite Management**: Organize and maintain test suites
- **Test Selection**: Intelligent test selection for changes
- **Flaky Detection**: Identify and handle flaky tests
- **Coverage Tracking**: Monitor coverage over time
- **CI Integration**: Seamless CI/CD integration

## Usage

### Manage Test Suites

```typescript
import { RegressionManager } from '@migrationpilot/regression';

const manager = new RegressionManager({
  storage: './regression-suites',
});

// Create a regression suite
const suite = await manager.createSuite({
  name: 'payroll-regression',
  project: projectId,
  tests: generatedTests,
});

// Run suite
const results = await manager.runSuite(suite.id);

// Get report
console.log(`Passed: ${results.passed}/${results.total}`);
console.log(`Duration: ${results.duration}ms`);
```

### Smart Test Selection

```typescript
import { TestSelector } from '@migrationpilot/regression';

const selector = new TestSelector();

// Select tests affected by changes
const selectedTests = await selector.select({
  changes: gitDiff,
  allTests: suite.tests,
  strategy: 'affected', // or 'critical', 'all'
});

console.log(`Running ${selectedTests.length} of ${suite.tests.length} tests`);
```

### Flaky Test Management

```typescript
import { FlakyDetector } from '@migrationpilot/regression';

const detector = new FlakyDetector();

// Detect flaky tests
const flaky = await detector.detect(suite, {
  runs: 10,
  threshold: 0.8, // 80% pass rate to be considered flaky
});

// Quarantine flaky tests
await manager.quarantine(flaky.map(t => t.id));
```

### Coverage Tracking

```typescript
import { CoverageTracker } from '@migrationpilot/regression';

const tracker = new CoverageTracker();

// Track coverage
await tracker.record(results.coverage);

// Get trends
const trends = await tracker.getTrends({
  period: '30d',
  metrics: ['line', 'branch', 'rule'],
});

// Alert on regression
if (trends.current.line < trends.baseline.line - 5) {
  console.warn('Coverage dropped by more than 5%');
}
```

## CI Integration

### GitHub Actions

```yaml
- name: Run Regression Tests
  run: |
    npx @migrationpilot/regression run \
      --suite payroll-regression \
      --select affected \
      --report junit
```

### Jenkins

```groovy
stage('Regression') {
  steps {
    sh 'npx @migrationpilot/regression run --suite main'
  }
  post {
    always {
      junit 'regression-results.xml'
    }
  }
}
```

## Installation

```bash
pnpm add @migrationpilot/regression
```

## License

MIT
