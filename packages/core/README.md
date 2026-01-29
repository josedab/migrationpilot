# @migrationpilot/core

Shared types, utilities, and core functionality for MigrationPilot.

## Overview

This package provides the foundational types, utilities, and shared functionality used across all MigrationPilot packages.

## Features

- **Type Definitions**: Core types for migrations, business rules, and analysis
- **Utilities**: ID generation, validation, formatting
- **Knowledge Graph**: Entity and relationship management
- **Security**: Encryption and compliance utilities
- **Cost Estimation**: Migration effort calculation
- **Semantic Diff**: Intelligent code comparison
- **Audit Logging**: Comprehensive action tracking

## Usage

### Types

```typescript
import type {
  SourceLanguage,
  TargetLanguage,
  BusinessRule,
  DataStructure,
  Procedure,
  TestCase,
  MigrationProject,
} from '@migrationpilot/core';

const rule: BusinessRule = {
  id: 'BR-001',
  name: 'Calculate Overtime Pay',
  category: 'calculation',
  inputs: [
    { name: 'hoursWorked', type: 'decimal' },
    { name: 'hourlyRate', type: 'decimal' },
  ],
  outputs: [
    { name: 'overtimePay', type: 'decimal' },
  ],
  logic: 'If hours > 40, overtime = (hours - 40) * rate * 1.5',
  confidence: 0.92,
  sourceLocation: { file: 'PAYROLL.cbl', startLine: 150, endLine: 165 },
};
```

### Utilities

```typescript
import { generateId, formatDuration, slugify } from '@migrationpilot/core';

const id = generateId();           // 'mp_1234567890abcdef'
const duration = formatDuration(3725000);  // '1h 2m 5s'
const slug = slugify('My Project'); // 'my-project'
```

### Knowledge Graph

```typescript
import { KnowledgeGraph } from '@migrationpilot/core';

const graph = new KnowledgeGraph();

// Add entities
graph.addEntity({
  id: 'proc-1',
  type: 'procedure',
  name: 'CALC-PAY',
  properties: { language: 'cobol' },
});

// Add relationships
graph.addRelationship({
  source: 'proc-1',
  target: 'data-1',
  type: 'reads',
});

// Query
const dependencies = graph.getDependencies('proc-1');
```

### Cost Estimator

```typescript
import { CostEstimator } from '@migrationpilot/core';

const estimator = new CostEstimator();
const estimate = estimator.estimate(analysis, {
  targetLanguage: 'java',
  complexity: 'high',
  teamSize: 5,
});

// Returns:
// - effortHours: total hours
// - costRange: { min, max }
// - breakdown: by phase
// - risks: potential issues
```

### Semantic Diff

```typescript
import { SemanticDiff } from '@migrationpilot/core';

const diff = new SemanticDiff();
const changes = diff.compare(legacyCode, modernCode, {
  ignoreFormatting: true,
  ignoreComments: true,
});

// Returns semantically meaningful differences
// - renamed variables
// - restructured logic
// - equivalent transformations
```

### Compliance Engine

```typescript
import { ComplianceEngine } from '@migrationpilot/core';

const compliance = new ComplianceEngine({
  standards: ['SOC2', 'GDPR'],
});

const report = compliance.audit(project);
// Returns compliance status, violations, recommendations
```

## Type Reference

### Languages

```typescript
type SourceLanguage = 'cobol' | 'fortran' | 'vb6' | 'java-legacy' | 'rpg' | 'pli' | 'natural';
type TargetLanguage = 'java' | 'python' | 'typescript' | 'go' | 'csharp';
```

### Business Rule

```typescript
interface BusinessRule {
  id: string;
  name: string;
  category: 'calculation' | 'validation' | 'decision' | 'transformation' | 'workflow';
  description?: string;
  inputs: Parameter[];
  outputs: Parameter[];
  logic: string;
  formula?: string;
  edgeCases?: string[];
  confidence: number;
  sourceLocation: SourceLocation;
  smeReview?: {
    status: 'pending' | 'approved' | 'rejected' | 'modified';
    comments?: string;
    reviewedBy?: string;
    reviewedAt?: Date;
  };
}
```

### Test Case

```typescript
interface TestCase {
  id: string;
  ruleId: string;
  name: string;
  description: string;
  category: 'boundary' | 'partition' | 'edge' | 'happy' | 'historical';
  inputs: Record<string, unknown>;
  expectedOutputs?: Record<string, unknown>;
  priority: 'high' | 'medium' | 'low';
}
```

### Migration Project

```typescript
interface MigrationProject {
  id: string;
  name: string;
  description?: string;
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  targetFramework: string;
  status: 'created' | 'analyzing' | 'designing' | 'generating' | 'validating' | 'complete';
  confidence: number;
  createdAt: Date;
  updatedAt: Date;
}
```

## Installation

```bash
pnpm add @migrationpilot/core
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
