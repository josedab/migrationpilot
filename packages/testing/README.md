# @migrationpilot/testing

Equivalence testing engine for validating behavioral equivalence between legacy and modern code.

## Overview

This package provides comprehensive testing capabilities to ensure that migrated code behaves identically to the original legacy system. It includes test case generation, differential testing, property-based testing, and regression suite generation.

## Features

- **Test Case Generation**: Automatic generation from business rules
- **Equivalence Validation**: Compare legacy vs modern outputs
- **Differential Testing**: Find behavioral differences
- **Property-Based Testing**: Random input generation for edge cases
- **Regression Suite**: Generate comprehensive test suites
- **Trace Recording**: Capture execution traces for debugging

## Usage

### Test Case Generation

```typescript
import { TestCaseGenerator } from '@migrationpilot/testing';

const generator = new TestCaseGenerator();

const testCases = await generator.generate(businessRules, {
  strategies: ['boundary', 'partition', 'property', 'historical'],
  maxTestsPerRule: 10,
  includeEdgeCases: true,
});

// Generated test cases include:
// - Boundary value tests (min, max, just inside/outside)
// - Equivalence partition tests
// - Edge cases (null, empty, max precision)
// - Property-based random inputs
```

### Equivalence Validation

```typescript
import { EquivalenceValidator } from '@migrationpilot/testing';

const validator = new EquivalenceValidator({
  tolerances: {
    decimal: 0.01,      // Allow 1% difference for decimals
    date: 1000,         // Allow 1 second difference for dates
  },
  ignoreFields: ['timestamp', 'id'],
});

const result = await validator.validate({
  testCases,
  legacyExecutor: async (input) => runLegacySystem(input),
  modernExecutor: async (input) => runModernSystem(input),
});

console.log(`Equivalence Score: ${result.score}%`);
console.log(`Passed: ${result.passed}/${result.total}`);

for (const failure of result.failures) {
  console.log(`
    Test: ${failure.testCase.name}
    Input: ${JSON.stringify(failure.input)}
    Legacy Output: ${JSON.stringify(failure.legacyOutput)}
    Modern Output: ${JSON.stringify(failure.modernOutput)}
    Difference: ${failure.difference}
  `);
}
```

### Differential Testing

```typescript
import { DifferentialTester } from '@migrationpilot/testing';

const tester = new DifferentialTester();

// Find inputs that cause different behavior
const differences = await tester.findDifferences({
  legacySystem: legacyExecutor,
  modernSystem: modernExecutor,
  inputGenerator: generateRandomInputs,
  maxIterations: 10000,
  stopOnFirst: false,
});

for (const diff of differences) {
  console.log(`Found difference for input: ${JSON.stringify(diff.input)}`);
}
```

### Property-Based Testing

```typescript
import { PropertyTester } from '@migrationpilot/testing';

const propTester = new PropertyTester();

// Define properties that should always hold
propTester.addProperty('overtime-never-negative', {
  generator: () => ({
    hoursWorked: Math.random() * 100,
    hourlyRate: Math.random() * 100,
  }),
  property: (input, output) => output.overtimePay >= 0,
});

propTester.addProperty('equivalence', {
  generator: generatePayrollInput,
  property: async (input) => {
    const legacy = await runLegacy(input);
    const modern = await runModern(input);
    return isEquivalent(legacy, modern);
  },
});

const results = await propTester.run({ iterations: 1000 });
```

### Regression Suite Generation

```typescript
import { RegressionGenerator } from '@migrationpilot/testing';

const regressionGen = new RegressionGenerator();

// Generate a regression test suite from historical data
const suite = await regressionGen.generate({
  historicalData: await loadHistoricalInputs(),
  businessRules,
  coverage: {
    minRuleCoverage: 95,
    minPathCoverage: 85,
  },
});

// Export as test files
await suite.exportTo('./tests/regression', {
  format: 'jest',       // 'jest' | 'vitest' | 'pytest' | 'junit'
  language: 'typescript',
});
```

### Trace Recording

```typescript
import { TraceRecorder } from '@migrationpilot/testing';

const recorder = new TraceRecorder();

// Record execution trace
const trace = await recorder.record(async () => {
  return await modernSystem.calculatePayroll(input);
});

// Analyze trace
console.log(`Functions called: ${trace.calls.length}`);
console.log(`Branches taken: ${trace.branches}`);
console.log(`Execution time: ${trace.duration}ms`);

// Compare traces
const comparison = recorder.compare(legacyTrace, modernTrace);
console.log(`Execution path similarity: ${comparison.similarity}%`);
```

## Test Oracle

The test oracle determines expected outputs when no legacy system is available:

```typescript
import { TestOracle } from '@migrationpilot/testing/oracle';

const oracle = new TestOracle({
  businessRules,
  constraints: [
    { field: 'totalPay', min: 0 },
    { field: 'overtimePay', formula: '(hours - 40) * rate * 1.5' },
  ],
});

// Validate output against oracle
const isValid = oracle.validate(input, output);
```

## Configuration

```typescript
interface TestingConfig {
  // Tolerance for comparison
  tolerances: {
    decimal: number;
    date: number;
    string: 'exact' | 'normalized' | 'fuzzy';
  };
  
  // Test generation
  generation: {
    maxTestsPerRule: number;
    includeEdgeCases: boolean;
    strategies: Strategy[];
  };
  
  // Execution
  execution: {
    timeout: number;
    retries: number;
    parallel: boolean;
    maxConcurrency: number;
  };
}
```

## Reports

```typescript
import { ReportGenerator } from '@migrationpilot/testing';

const reportGen = new ReportGenerator();

const report = await reportGen.generate(validationResults, {
  format: 'html',    // 'html' | 'json' | 'markdown' | 'pdf'
  includeTraces: true,
  includeRecommendations: true,
});

await report.save('./reports/equivalence-report.html');
```

## Installation

```bash
pnpm add @migrationpilot/testing
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Type check
pnpm typecheck
```

## License

MIT
