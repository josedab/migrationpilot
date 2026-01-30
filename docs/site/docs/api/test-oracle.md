---
sidebar_position: 8
---

# Test Oracle API

The Test Oracle module provides AI-powered behavioral learning and validation for ensuring migrated code maintains the same behavior as legacy systems.

## Overview

The Test Oracle module includes:
- **BehavioralLearningEngine**: Learns expected behavior from historical execution data
- **TestOracle**: Validates actual outputs against learned behavior
- **TestCaseSynthesizer**: Generates test cases from learned models
- **ContinuousValidationPipeline**: Real-time validation with drift detection

## BehavioralLearningEngine

Learns behavioral models from historical execution data by discovering invariants, patterns, and constraints.

### Constructor

```typescript
import { BehavioralLearningEngine } from '@migrationpilot/testing';

const engine = new BehavioralLearningEngine({
  projectId: 'my-project',
  minSamplesForModel: 50,        // Minimum samples required
  confidenceThreshold: 0.8,       // Threshold for invariants
  maxInvariantsPerModel: 100,     // Limit on discovered invariants
  strictMode: false,              // Strict validation mode
  enableContinuousLearning: true, // Allow model updates
});
```

### Training Models

```typescript
// Prepare historical execution data
const executions = [
  {
    id: 'exec-1',
    ruleId: 'interest-calc',
    ruleName: 'Interest Calculator',
    timestamp: new Date('2024-01-15'),
    inputs: { principal: 10000, rate: 0.05, years: 5 },
    outputs: { interest: 2500, total: 12500 },
    metadata: { source: 'mainframe', success: true },
  },
  // ... more executions (minimum 50 required by default)
];

// Train a behavioral model
const model = await engine.trainModel(
  executions,
  'Interest Calculation Model',  // Model name
  'interest-calc'                 // Optional rule ID
);

console.log(model.id);           // Unique model ID
console.log(model.version);      // Model version
console.log(model.invariants);   // Discovered invariants
console.log(model.constraints);  // Learned constraints
console.log(model.quality);      // Model quality metrics
```

### Model Output Structure

```typescript
interface BehavioralModel {
  id: string;
  projectId: string;
  ruleId?: string;
  name: string;
  description: string;
  version: number;
  
  // Learned signatures
  inputSignature: Record<string, FieldSignature>;
  outputSignature: Record<string, FieldSignature>;
  
  // Statistical model
  statistics: ModelStatistics;
  
  // Discovered behaviors
  invariants: LearnedInvariant[];
  constraints: LearnedConstraint[];
  edgeCases: DiscoveredEdgeCase[];
  
  // Quality assessment
  quality: ModelQuality;
  
  // Training metadata
  trainingInfo: TrainingInfo;
  
  createdAt: Date;
  updatedAt: Date;
}
```

### Updating Models

```typescript
// Add new execution data to existing model
const updatedModel = await engine.updateModel(model.id, newExecutions);

console.log(updatedModel.version);  // Incremented version
```

### Model Retrieval

```typescript
// Get model by ID
const model = engine.getModel(modelId);

// Get all models for a project
const models = engine.getModels(projectId);
```

## Invariant Types

The engine discovers several types of invariants:

| Type | Description | Example |
|------|-------------|---------|
| `range` | Value falls within observed range | `0 < interest < principal` |
| `constant` | Value is always the same | `currency = 'USD'` |
| `ratio` | Consistent ratio between values | `total = principal + interest` |
| `correlation` | Correlated input-output relationship | `rate ↑ → interest ↑` |
| `conditional` | Rule applies under conditions | `if premium: discount > 10%` |
| `format` | String format pattern | `accountId matches /ACC-\d{6}/` |
| `cardinality` | Set membership constraints | `status in ['ACTIVE', 'PENDING']` |

## TestOracle

Validates actual execution outputs against learned behavioral models.

### Constructor

```typescript
import { TestOracle } from '@migrationpilot/testing';

const oracle = new TestOracle(model, {
  confidenceThreshold: 0.7,
  divergenceThreshold: 0.1,
  strictMode: false,
  enableStatisticalValidation: true,
});
```

### Prediction

```typescript
// Predict expected output for given input
const prediction = await oracle.predict({
  principal: 15000,
  rate: 0.06,
  years: 3,
});

console.log(prediction.output);      // Predicted output values
console.log(prediction.confidence);  // Prediction confidence (0-1)
```

### Validation

```typescript
// Validate actual output against expected behavior
const result = await oracle.validate({
  input: { principal: 15000, rate: 0.06, years: 3 },
  actualOutput: { interest: 2700, total: 17700 },
  ruleId: 'interest-calc',
});

console.log(result.passed);       // true/false
console.log(result.confidence);   // Confidence in result
console.log(result.divergences);  // List of detected divergences
```

### Batch Validation

```typescript
// Validate multiple test cases
const results = await oracle.validateBatch([
  { input: {...}, actualOutput: {...}, ruleId: '...' },
  { input: {...}, actualOutput: {...}, ruleId: '...' },
]);

const passRate = results.filter(r => r.passed).length / results.length;
```

### Invariant Checking

```typescript
// Check specific invariants
const violations = await oracle.checkInvariants(
  { principal: 15000, rate: 0.06, years: 3 },
  { interest: 2700, total: 17700 }
);

violations.forEach(v => {
  console.log(`Violated: ${v.invariantId}`);
  console.log(`  Expected: ${v.expected}`);
  console.log(`  Actual: ${v.actual}`);
});
```

## TestCaseSynthesizer

Generates test cases from behavioral models to maximize coverage.

### Constructor

```typescript
import { TestCaseSynthesizer } from '@migrationpilot/testing';

const synthesizer = new TestCaseSynthesizer(model, {
  targetCoverage: 0.9,
  maxTestCases: 1000,
  includeBoundary: true,
  includeEdgeCases: true,
  includeNegative: true,
});
```

### Test Case Generation

```typescript
// Generate test cases
const testCases = await synthesizer.generateTestCases(100);

testCases.forEach(tc => {
  console.log(`Test: ${tc.id}`);
  console.log(`  Category: ${tc.category}`);  // 'normal' | 'boundary' | 'edge' | 'negative'
  console.log(`  Input: ${JSON.stringify(tc.input)}`);
  console.log(`  Expected: ${JSON.stringify(tc.expectedOutput)}`);
  console.log(`  Priority: ${tc.priority}`);
});
```

### Mutation Testing

```typescript
// Generate mutations of a base test case
const baseCase = {
  id: 'base-1',
  input: { principal: 10000, rate: 0.05, term: 12 },
  expectedOutput: { payment: 856.07 },
};

const mutations = await synthesizer.generateMutations(baseCase, 10);

mutations.forEach(m => {
  console.log(`Mutation: ${m.mutationType}`);
  console.log(`  Modified: ${JSON.stringify(m.input)}`);
});
```

### Coverage Analysis

```typescript
// Analyze coverage of existing test cases
const coverage = await synthesizer.analyzeCoverage(testCases);

console.log(`Input coverage: ${coverage.inputCoverage}%`);
console.log(`Pattern coverage: ${coverage.patternCoverage}%`);
console.log(`Invariant coverage: ${coverage.invariantCoverage}%`);
console.log(`Overall coverage: ${coverage.overallCoverage}%`);

// Get suggestions to improve coverage
const suggestions = await synthesizer.suggestTestsForCoverage(testCases);
```

### Negative Test Cases

```typescript
// Generate test cases that should fail
const negatives = await synthesizer.generateNegativeTestCases(20);

negatives.forEach(tc => {
  console.log(`Negative test: ${tc.id}`);
  console.log(`  Reason: ${tc.expectedOutput.failureReason}`);
});
```

## ContinuousValidationPipeline

Real-time validation pipeline with drift detection and alerting.

### Constructor

```typescript
import { ContinuousValidationPipeline } from '@migrationpilot/testing';

const pipeline = new ContinuousValidationPipeline(model, {
  batchSize: 100,
  validationIntervalMs: 5000,
  driftThreshold: 0.15,
  alertThreshold: 0.25,
  enableAutoRetraining: true,
  maxHistorySize: 10000,
});
```

### Validation

```typescript
// Validate a single execution
const result = await pipeline.validate({
  id: 'execution-123',
  input: { principal: 10000, rate: 0.05, years: 5 },
  output: { interest: 2500, total: 12500 },
  timestamp: new Date(),
});

console.log(result.passed);
console.log(result.confidence);
console.log(result.divergences);
```

### Monitoring

```typescript
// Get pipeline status
const status = pipeline.getStatus();
console.log(`Validations: ${status.validationCount}`);
console.log(`Pass rate: ${status.passRate * 100}%`);
console.log(`Is paused: ${status.isPaused}`);

// Get current drift score
const drift = pipeline.getCurrentDrift();
console.log(`Current drift: ${drift * 100}%`);

// Get detailed statistics
const stats = pipeline.getStatistics();
console.log(`Total: ${stats.totalValidations}`);
console.log(`Passed: ${stats.passedValidations}`);
console.log(`Failed: ${stats.failedValidations}`);
console.log(`Avg confidence: ${stats.averageConfidence}`);
```

### Alerts

```typescript
// Get active alerts
const alerts = pipeline.getAlerts();

alerts.forEach(alert => {
  console.log(`Alert: ${alert.type}`);
  console.log(`  Severity: ${alert.severity}`);
  console.log(`  Message: ${alert.message}`);
  console.log(`  Time: ${alert.timestamp}`);
});

// Acknowledge an alert
pipeline.acknowledgeAlert(alertId);
```

### Lifecycle Control

```typescript
// Pause validation
pipeline.pause();

// Resume validation
pipeline.resume();

// Reset statistics (keeps model)
pipeline.reset();
```

## TypeScript Types

Key types for the Test Oracle module:

```typescript
interface HistoricalExecution {
  id: string;
  ruleId: string;
  ruleName: string;
  timestamp: Date;
  inputs: Record<string, unknown>;
  outputs: Record<string, unknown>;
  metadata?: Record<string, unknown>;
}

interface LearnedInvariant {
  id: string;
  type: InvariantType;
  description: string;
  expression: string;
  confidence: number;
  supportCount: number;
  coverage: number;
}

interface ValidationResult {
  passed: boolean;
  confidence: number;
  divergences: Divergence[];
  checkedInvariants: InvariantCheckResult[];
  timestamp: Date;
}

interface SynthesizedTestCase {
  id: string;
  category: 'normal' | 'boundary' | 'edge' | 'negative';
  input: Record<string, unknown>;
  expectedOutput: Record<string, unknown>;
  priority: 'high' | 'medium' | 'low';
  reason: string;
}
```

## Example: Complete Workflow

```typescript
import {
  BehavioralLearningEngine,
  TestOracle,
  TestCaseSynthesizer,
  ContinuousValidationPipeline,
} from '@migrationpilot/testing';

// 1. Gather historical execution data
const historicalData = await fetchHistoricalExecutions('interest-calc');

// 2. Train behavioral model
const engine = new BehavioralLearningEngine({ projectId: 'my-project' });
const model = await engine.trainModel(historicalData, 'Interest Calculator');

console.log(`Discovered ${model.invariants.length} invariants`);
console.log(`Model confidence: ${model.quality.overallConfidence}`);

// 3. Create test oracle
const oracle = new TestOracle(model);

// 4. Generate test cases
const synthesizer = new TestCaseSynthesizer(model);
const testCases = await synthesizer.generateTestCases(100);

// 5. Validate migrated code
for (const testCase of testCases) {
  const actualOutput = await runMigratedCode(testCase.input);
  const result = await oracle.validate({
    input: testCase.input,
    actualOutput,
    ruleId: 'interest-calc',
  });
  
  if (!result.passed) {
    console.log(`FAILED: ${testCase.id}`);
    console.log(`  Expected: ${JSON.stringify(testCase.expectedOutput)}`);
    console.log(`  Actual: ${JSON.stringify(actualOutput)}`);
    console.log(`  Divergences: ${result.divergences.map(d => d.description)}`);
  }
}

// 6. Set up continuous validation
const pipeline = new ContinuousValidationPipeline(model, {
  driftThreshold: 0.1,
  alertThreshold: 0.2,
});

// Monitor in production
productionExecutions.subscribe(async (execution) => {
  const result = await pipeline.validate(execution);
  if (!result.passed) {
    logDivergence(execution, result);
  }
});

// Check for drift periodically
setInterval(() => {
  const drift = pipeline.getCurrentDrift();
  if (drift > 0.1) {
    alertTeam(`Behavioral drift detected: ${drift * 100}%`);
  }
}, 60000);
```
