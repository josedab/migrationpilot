# @migrationpilot/agents

AI agents for legacy code modernization powered by GitHub Copilot SDK.

## Overview

This package provides the core AI agents that power MigrationPilot's multi-agent architecture. Each agent specializes in a specific phase of the migration pipeline.

## Agents

### 🏺 Archeologist

Analyzes legacy code and extracts business rules with confidence scoring.

```typescript
import { ArcheologistAgent } from '@migrationpilot/agents/archeologist';

const archeologist = new ArcheologistAgent();
const analysis = await archeologist.analyzeFile(context, sourceCode, 'cobol', 'PAYROLL.cbl');

// Returns:
// - Business rules with confidence scores
// - Data structures
// - Procedures and control flow
// - External dependencies
// - Complexity metrics
```

**Capabilities:**
- AST parsing and structure analysis
- Business rule extraction with confidence scores (0.0-1.0)
- Data structure mapping
- Dependency identification
- Complexity metrics calculation

### 📐 Architect

Designs modern architectures from legacy analysis.

```typescript
import { ArchitectAgent } from '@migrationpilot/agents/architect';

const architect = new ArchitectAgent();
const design = await architect.designArchitecture(
  context,
  analysis,
  'java',
  'spring-boot'
);

// Returns:
// - Service definitions
// - API contracts
// - Database schema
// - Migration strategy
```

**Capabilities:**
- Service boundary design (DDD principles)
- API contract generation (OpenAPI)
- Database schema design
- Migration strategy planning (strangler fig, parallel run)
- Risk assessment

### 🔨 Builder

Generates modern, idiomatic code from architecture designs.

```typescript
import { BuilderAgent } from '@migrationpilot/agents/builder';

const builder = new BuilderAgent();
const result = await builder.generateService(
  context,
  serviceDefinition,
  apis,
  {
    targetLanguage: 'java',
    framework: 'spring-boot',
    includeTests: true,
    includeDocumentation: true,
  }
);

// Returns:
// - Source files
// - Test files
// - Configuration files
// - Documentation
```

**Capabilities:**
- Multi-language code generation
- Comprehensive test generation
- Full traceability to source
- Framework-specific best practices

### ✅ Validator

Validates behavioral equivalence between legacy and modern code.

```typescript
import { ValidatorAgent } from '@migrationpilot/agents/validator';

const validator = new ValidatorAgent();

// Generate test cases
const testCases = await validator.generateTestCases(context, businessRules, {
  strategies: ['boundary', 'partition', 'property'],
  maxTestsPerRule: 10,
  includeEdgeCases: true,
});

// Validate equivalence
const validation = await validator.validateEquivalence(
  context,
  testCases,
  legacyResults,
  modernResults
);
```

**Capabilities:**
- Test case generation (boundary, partition, property-based)
- Output comparison with tolerance
- Root cause analysis
- Confidence scoring

## Orchestrator

The `MigrationOrchestrator` coordinates all agents through the complete pipeline:

```typescript
import { MigrationOrchestrator } from '@migrationpilot/agents';

const orchestrator = new MigrationOrchestrator();

const result = await orchestrator.migrate(sourceCode, 'PAYROLL.cbl', {
  projectId: 'proj-123',
  userId: 'user-456',
  sourceLanguage: 'cobol',
  targetLanguage: 'java',
  targetFramework: 'spring-boot',
  options: {
    enableStranglerFig: true,
    generateTests: true,
    generateDocumentation: true,
    humanReviewRequired: true,
    confidenceThreshold: 0.9,
  },
  callbacks: {
    onProgress: (phase, progress, message) => console.log(`${phase}: ${progress}% - ${message}`),
    onHumanReviewRequired: async (rules) => {
      // Present low-confidence rules for SME review
      return reviewedRules;
    },
  },
});
```

## Configuration

Each agent accepts configuration options:

| Option | Default | Description |
|--------|---------|-------------|
| `model` | `gpt-4` | LLM model to use |
| `temperature` | varies | Creativity (0.1 for analysis, 0.3 for design) |
| `maxTokens` | `8000` | Maximum tokens per request |

## Installation

```bash
pnpm add @migrationpilot/agents
```

## Dependencies

- `@copilot-extensions/preview-sdk` - GitHub Copilot SDK
- `@migrationpilot/core` - Shared types and utilities

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
