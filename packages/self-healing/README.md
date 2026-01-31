# @migrationpilot/self-healing

Self-healing migration pipeline with automated error recovery.

## Overview

This package provides self-healing capabilities for the migration pipeline, automatically detecting, diagnosing, and recovering from common errors during code analysis, generation, and validation.

## Features

- **Error Detection**: Identify issues in generated code
- **Root Cause Analysis**: AI-powered diagnosis
- **Automatic Fixes**: Apply corrections without human intervention
- **Retry Strategies**: Intelligent retry with backoff
- **Recovery Patterns**: Handle transient and persistent failures
- **Learning**: Improve from past failures

## Usage

### Self-Healing Pipeline

```typescript
import { SelfHealingPipeline } from '@migrationpilot/self-healing';

const pipeline = new SelfHealingPipeline({
  maxRetries: 3,
  healingStrategies: ['syntax', 'type', 'semantic', 'dependency'],
});

// Run with self-healing
const result = await pipeline.run(async () => {
  return await generateCode(specification);
}, {
  onError: async (error, attempt) => {
    console.log(`Attempt ${attempt} failed: ${error.message}`);
  },
  onHealed: async (issue, fix) => {
    console.log(`Auto-fixed: ${issue.type}`);
  },
});
```

### Error Detection

```typescript
import { ErrorDetector } from '@migrationpilot/self-healing';

const detector = new ErrorDetector({
  languages: ['java', 'python', 'typescript'],
});

// Detect issues in generated code
const issues = await detector.detect(generatedCode, {
  language: 'java',
  framework: 'spring-boot',
});

for (const issue of issues) {
  console.log(`
    Type: ${issue.type}
    Severity: ${issue.severity}
    Location: ${issue.location.file}:${issue.location.line}
    Message: ${issue.message}
    Suggestion: ${issue.suggestion}
  `);
}
```

### Automatic Fixing

```typescript
import { AutoFixer } from '@migrationpilot/self-healing';

const fixer = new AutoFixer({
  strategies: {
    syntax: true,       // Fix syntax errors
    types: true,        // Fix type errors
    imports: true,      // Fix missing imports
    nullSafety: true,   // Add null checks
    exceptions: true,   // Add error handling
  },
});

// Automatically fix issues
const fixed = await fixer.fix(generatedCode, issues, {
  maxChanges: 10,
  preserveSemantics: true,
});

console.log(`Fixed ${fixed.changesApplied} issues`);
console.log(fixed.code);
```

### Root Cause Analysis

```typescript
import { RootCauseAnalyzer } from '@migrationpilot/self-healing';

const analyzer = new RootCauseAnalyzer();

// Analyze failure
const analysis = await analyzer.analyze({
  error: compilationError,
  generatedCode,
  specification,
  businessRule,
});

console.log(`
  Root Cause: ${analysis.rootCause}
  Category: ${analysis.category}
  Confidence: ${analysis.confidence}
  
  Explanation: ${analysis.explanation}
  
  Recommended Fixes:
  ${analysis.fixes.map(f => `- ${f.description}`).join('\n')}
`);
```

### Recovery Strategies

```typescript
import { RecoveryManager } from '@migrationpilot/self-healing';

const recovery = new RecoveryManager();

// Define recovery strategies
recovery.addStrategy('compilation-error', {
  detect: (error) => error.type === 'CompilationError',
  recover: async (error, context) => {
    // Try to fix the code
    const fixed = await fixer.fix(context.code, [error]);
    if (fixed.success) return fixed.code;
    
    // Fall back to regeneration
    return await regenerateCode(context.specification);
  },
  maxAttempts: 3,
});

recovery.addStrategy('timeout', {
  detect: (error) => error.type === 'TimeoutError',
  recover: async (error, context) => {
    // Retry with longer timeout
    return await context.retry({ timeout: context.timeout * 2 });
  },
  backoff: 'exponential',
});

// Execute with recovery
const result = await recovery.execute(
  () => generateAndCompile(specification),
  { timeout: 30000 }
);
```

### Learning from Failures

```typescript
import { FailureLearner } from '@migrationpilot/self-healing';

const learner = new FailureLearner({
  storage: 'database',
  minSamples: 10,
});

// Record failure and fix
await learner.record({
  error,
  context: { language: 'java', framework: 'spring-boot' },
  fix: appliedFix,
  success: true,
});

// Get learned fixes for similar errors
const suggestions = await learner.suggest(newError, {
  maxSuggestions: 5,
  minConfidence: 0.8,
});
```

## Error Types

| Type | Description | Auto-Fixable |
|------|-------------|--------------|
| `syntax` | Syntax errors (missing semicolons, brackets) | ✅ |
| `type` | Type mismatches and inference failures | ✅ |
| `import` | Missing or incorrect imports | ✅ |
| `null-safety` | Potential null pointer issues | ✅ |
| `dependency` | Missing dependencies | ✅ |
| `semantic` | Logic errors detected by analysis | ⚠️ Partial |
| `behavioral` | Equivalence test failures | ❌ Requires review |

## Recovery Flow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Execute   │────▶│   Detect    │────▶│   Analyze   │
└─────────────┘     │   Error     │     │   Root      │
                    └─────────────┘     │   Cause     │
                                        └─────────────┘
                                               │
                    ┌──────────────────────────┘
                    │
                    ▼
            ┌─────────────┐
            │   Select    │
            │   Strategy  │
            └─────────────┘
                    │
        ┌───────────┼───────────┐
        │           │           │
        ▼           ▼           ▼
   ┌─────────┐ ┌─────────┐ ┌─────────┐
   │  Retry  │ │  Fix &  │ │ Regen-  │
   │         │ │  Retry  │ │ erate   │
   └─────────┘ └─────────┘ └─────────┘
        │           │           │
        └───────────┼───────────┘
                    │
                    ▼
            ┌─────────────┐
            │   Record    │
            │   & Learn   │
            └─────────────┘
```

## Configuration

```typescript
interface SelfHealingConfig {
  // Retry settings
  retry: {
    maxAttempts: number;
    backoff: 'constant' | 'linear' | 'exponential';
    initialDelay: number;
    maxDelay: number;
  };
  
  // Auto-fix settings
  autoFix: {
    enabled: boolean;
    maxChanges: number;
    requireApproval: boolean;
  };
  
  // Learning
  learning: {
    enabled: boolean;
    minConfidence: number;
  };
  
  // Alerting
  alerting: {
    onUnrecoverable: boolean;
    channels: string[];
  };
}
```

## Installation

```bash
pnpm add @migrationpilot/self-healing
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Watch tests
pnpm test:watch
```

## License

MIT
