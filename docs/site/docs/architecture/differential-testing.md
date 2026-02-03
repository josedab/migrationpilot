---
sidebar_position: 5
---

# Differential Testing

The differential testing engine is the core component that proves behavioral equivalence between legacy and migrated code. It automatically generates test inputs, executes both versions, and compares results to identify discrepancies.

## Overview

Differential testing answers the critical question: "Does the migrated code behave exactly like the original?" The engine:

1. **Generates diverse test inputs** using multiple strategies
2. **Executes both legacy and migrated code** with identical inputs
3. **Compares outputs, side effects, and execution traces**
4. **Reports differences** with severity ratings and fix suggestions

```typescript
import { createDifferentialEngine } from '@migrationpilot/differential';

const engine = createDifferentialEngine({
  maxIterations: 1000,
  numericTolerance: 0.0001,
});

const comparison = await engine.analyze(legacyCode, migratedCode);

console.log(`Equivalence rate: ${comparison.summary.equivalenceRate * 100}%`);
console.log(`Critical differences: ${comparison.summary.criticalDifferences}`);
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                 Behavioral Differential Engine                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │ Input Fuzzer │───▶│  Execution   │───▶│  Comparators │       │
│  │              │    │   Engine     │    │              │       │
│  └──────────────┘    └──────────────┘    └──────────────┘       │
│         │                   │                   │                │
│         ▼                   ▼                   ▼                │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐       │
│  │  Boundary    │    │   Legacy     │    │   Output     │       │
│  │  Random      │    │   Migrated   │    │   Trace      │       │
│  │  Symbolic    │    │   (parallel) │    │   SideEffect │       │
│  └──────────────┘    └──────────────┘    └──────────────┘       │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    Report Generator                          ││
│  │  Summary │ Differences │ Recommendations │ Metrics           ││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
```

## Configuration

```typescript
interface DifferentialConfig {
  maxIterations?: number;        // Maximum test cases to generate (default: 1000)
  timeout?: number;              // Execution timeout per test (ms, default: 30000)
  fuzzingDepth?: number;         // Depth of input exploration (default: 3)
  numericTolerance?: number;     // Tolerance for numeric comparisons (default: 0.0001)
  stringComparison?: 'exact' | 'normalized' | 'fuzzy';  // String comparison mode
  dateFormatTolerance?: boolean; // Allow different date formats (default: true)
  nullEquivalents?: string[];    // Values treated as null equivalents
  ignoreWhitespace?: boolean;    // Ignore whitespace differences (default: true)
  parallelExecution?: boolean;   // Run tests in parallel (default: true)
  maxConcurrency?: number;       // Max parallel executions (default: 4)
}
```

### Configuration Examples

**High-precision financial calculations:**
```typescript
const engine = createDifferentialEngine({
  numericTolerance: 0,          // Exact match required
  stringComparison: 'exact',
  maxIterations: 5000,          // More thorough testing
});
```

**Legacy systems with messy data:**
```typescript
const engine = createDifferentialEngine({
  numericTolerance: 0.01,       // Allow small rounding differences
  stringComparison: 'fuzzy',    // Handle encoding variations
  nullEquivalents: ['', 'null', 'NULL', 'N/A', '0', 'SPACES'],
  ignoreWhitespace: true,
});
```

## Input Generation Strategies

The engine uses multiple strategies to maximize test coverage:

### Boundary Value Testing
Tests edge cases at data type boundaries:

```typescript
// For a numeric field with range 0-999:
// Generated: 0, 1, 499, 500, 998, 999, -1, 1000
```

### Equivalence Partitioning
Tests representative values from each valid partition:

```typescript
// For account type field (A, B, C):
// Generated: 'A', 'B', 'C', '', null, 'X' (invalid)
```

### Random Fuzzing
Generates random values within constraints:

```typescript
const fuzzer = createInputFuzzer('random');
fuzzer.setConstraints([
  { field: 'amount', type: 'decimal', constraints: { min: 0, max: 10000, precision: 2 } },
  { field: 'name', type: 'string', constraints: { length: { min: 1, max: 50 } } },
]);
```

### Symbolic Execution
Explores code paths systematically:

```typescript
const fuzzer = createInputFuzzer('symbolic');
// Generates inputs to cover all branches
```

### Property-Based Testing
Tests invariants that should always hold:

```typescript
// Property: calculate(a) + calculate(b) == calculate(a + b)
// Engine generates many (a, b) pairs to verify
```

### Historic Data
Uses production data patterns (sanitized):

```typescript
const fuzzer = createInputFuzzer('historic');
fuzzer.loadPatterns('./production-patterns.json');
```

## Execution Engine

### Supported Languages

| Language | Execution Method |
|----------|-----------------|
| COBOL | GnuCOBOL interpreter |
| Java | JVM with instrumentation |
| Python | exec() with trace hooks |
| TypeScript | VM with instrumentation |
| C# | .NET runtime |
| Go | Native execution |

### Execution Context

```typescript
interface ExecutionContext {
  inputs: Record<string, DataValue>;      // Input parameters
  environment?: Record<string, string>;   // Environment variables
  mockServices?: MockServiceDefinition[]; // Mocked external services
  timeout?: number;                       // Execution timeout
}
```

### Execution Tracing

Every execution captures:

```typescript
interface ExecutionTrace {
  steps: TraceStep[];      // Line-by-line execution
  branches: BranchInfo[];  // Conditional branch decisions
  calls: CallInfo[];       // Function/procedure calls
  memory: MemorySnapshot[];// Variable state snapshots
}
```

## Comparators

### Output Comparator

Compares final output values:

```typescript
const comparator = createOutputComparator({
  numericTolerance: 0.0001,
  stringComparison: 'normalized',
});

const diffs = comparator.compare(legacyOutputs, migratedOutputs);
// Returns: OutputDifference[]
```

**Difference Types:**
- `missing`: Output exists in legacy but not migrated
- `added`: Output exists in migrated but not legacy
- `changed`: Values differ
- `type-mismatch`: Same value but different types

### Trace Comparator

Compares execution paths:

```typescript
const analyzer = createTraceAnalyzer();
const traceDiffs = analyzer.compare(legacyTrace, migratedTrace);

// Detects:
// - Different branch decisions
// - Different call sequences
// - Different loop iterations
```

### Side Effect Comparator

Compares external interactions:

```typescript
const comparator = createSideEffectComparator();
const diffs = comparator.compare(legacySideEffects, migratedSideEffects);

// Tracks:
// - Database operations (INSERT, UPDATE, DELETE)
// - File operations (read, write)
// - Network calls
// - Queue messages
// - Log outputs
```

## Analysis Results

### BehaviorComparison

```typescript
interface BehaviorComparison {
  id: string;
  timestamp: Date;
  legacyCode: ExecutableCode;
  migratedCode: ExecutableCode;
  testCases: TestCaseResult[];
  summary: ComparisonSummary;
  differences: BehaviorDifference[];
}
```

### ComparisonSummary

```typescript
interface ComparisonSummary {
  totalTestCases: number;
  equivalentCases: number;
  failedCases: number;
  warningCases: number;
  equivalenceRate: number;        // 0.0 - 1.0
  criticalDifferences: number;
  warningDifferences: number;
  averagePerformanceRatio: number;
}
```

### BehaviorDifference

```typescript
interface BehaviorDifference {
  id: string;
  type: 'output' | 'trace' | 'side-effect' | 'performance';
  severity: 'critical' | 'warning' | 'info';
  testCaseId: string;
  description: string;
  legacy: unknown;
  migrated: unknown;
  possibleCauses: string[];
  suggestedFixes: string[];
}
```

## Report Generation

Generate reports in multiple formats:

```typescript
const report = engine.generateReport(comparison, 'markdown');
// Also supports: 'json', 'html'
```

### Report Sections

1. **Executive Summary**: High-level pass/fail with confidence score
2. **Test Coverage**: Input space coverage metrics
3. **Differences**: Detailed breakdown of each discrepancy
4. **Recommendations**: Prioritized list of fixes
5. **Risk Assessment**: Overall migration risk level

### Example Report

```markdown
# Differential Analysis Report

## Summary
- **Equivalence Rate**: 98.5%
- **Total Test Cases**: 1,000
- **Passed**: 985
- **Failed**: 10 (critical), 5 (warning)

## Critical Differences

### DIFF-001: Interest Calculation Mismatch
- **Test Case**: boundary_max_rate
- **Input**: { principal: 999999.99, rate: 0.9999, years: 30 }
- **Legacy Output**: 29999969.70
- **Migrated Output**: 29999969.71
- **Cause**: Rounding difference in intermediate calculation
- **Suggested Fix**: Use BigDecimal.ROUND_HALF_EVEN in Java

## Recommendations
1. Fix decimal rounding (10 test cases affected)
2. Review null handling in ACCOUNT-TYPE (3 test cases)
3. Add missing validation for negative amounts (2 test cases)
```

## Event Handling

Subscribe to real-time analysis events:

```typescript
engine.onEvent((event) => {
  switch (event.type) {
    case 'analysis-started':
      console.log('Starting analysis...');
      break;
    case 'test-case-completed':
      const { testCase } = event;
      console.log(`Test ${testCase.id}: ${testCase.comparison.equivalent ? 'PASS' : 'FAIL'}`);
      break;
    case 'difference-detected':
      console.log(`Difference: ${event.difference.description}`);
      break;
    case 'analysis-completed':
      console.log(`Done. Equivalence: ${event.comparison.summary.equivalenceRate * 100}%`);
      break;
  }
});
```

## Integration with Validator Agent

The Validator Agent uses the differential engine internally:

```typescript
import { ValidatorAgent } from '@migrationpilot/agents';

const validator = new ValidatorAgent();
const result = await validator.validate(context, {
  legacyCode: cobolSource,
  migratedCode: javaSource,
  businessRules: extractedRules,
  testConfig: {
    strategy: 'boundary',
    maxCases: 500,
  },
});

// Result includes differential analysis
console.log(result.data.confidenceScore);
console.log(result.data.testResults);
```

## Best Practices

### 1. Start with Known Inputs
Before fuzzing, test with known input/output pairs:

```typescript
const knownCases = [
  { inputs: { amount: 100, rate: 0.05 }, expected: { interest: 5.00 } },
  { inputs: { amount: 0, rate: 0.05 }, expected: { interest: 0 } },
];

const comparison = await engine.analyze(legacy, migrated, knownCases);
```

### 2. Configure Tolerance Appropriately
Different domains need different tolerances:

```typescript
// Financial: Zero tolerance
numericTolerance: 0

// Scientific: Allow floating point variance
numericTolerance: 1e-10

// Analytics: Allow small differences
numericTolerance: 0.01
```

### 3. Mock External Dependencies
Isolate the code under test:

```typescript
const context: ExecutionContext = {
  inputs: testInputs,
  mockServices: [
    {
      name: 'DATABASE',
      type: 'database',
      responses: [
        { pattern: 'SELECT.*FROM ACCOUNTS', response: mockAccountData },
      ],
    },
  ],
};
```

### 4. Investigate All Critical Differences
Never ignore critical differences - they indicate real bugs:

```typescript
for (const diff of comparison.differences) {
  if (diff.severity === 'critical') {
    console.log(`MUST FIX: ${diff.description}`);
    console.log(`Causes: ${diff.possibleCauses.join(', ')}`);
  }
}
```

### 5. Track Equivalence Over Time
Monitor equivalence rate as you fix issues:

```
Iteration 1: 85% equivalence (150 failures)
Iteration 2: 92% equivalence (80 failures)  [Fixed rounding]
Iteration 3: 98% equivalence (20 failures)  [Fixed nulls]
Iteration 4: 99.8% equivalence (2 failures) [Fixed edge case]
```

## API Reference

### createDifferentialEngine

```typescript
function createDifferentialEngine(
  config?: Partial<DifferentialConfig>
): BehavioralDifferentialEngine;
```

### BehavioralDifferentialEngine.analyze

```typescript
async analyze(
  legacyCode: ExecutableCode,
  migratedCode: ExecutableCode,
  testInputs?: GeneratedInput[]
): Promise<BehaviorComparison>;
```

### BehavioralDifferentialEngine.generateReport

```typescript
generateReport(
  comparison: BehaviorComparison,
  format: 'json' | 'html' | 'markdown'
): string;
```

### BehavioralDifferentialEngine.onEvent

```typescript
onEvent(
  handler: (event: DifferentialEvent) => void
): () => void;  // Returns unsubscribe function
```
