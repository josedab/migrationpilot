# @migrationpilot/finetuning

Custom LLM fine-tuning pipeline for domain-specific migration.

## Overview

This package provides tools for fine-tuning language models on your organization's specific legacy codebase and domain knowledge, improving migration accuracy for your unique systems.

## Features

- **Data Preparation**: Create training datasets from migrations
- **Model Training**: Fine-tune models on your domain
- **Evaluation**: Measure improvement on your codebase
- **Deployment**: Deploy custom models to agents
- **Continuous Learning**: Improve from production feedback

## Usage

```typescript
import { FinetuningPipeline } from '@migrationpilot/finetuning';

const pipeline = new FinetuningPipeline({
  baseModel: 'gpt-4',
  provider: 'openai', // or 'anthropic', 'local'
});

// Prepare training data
const dataset = await pipeline.prepareDataset({
  source: 'migrations',
  projects: completedProjects,
  includeReviews: true,  // Include SME corrections
});

// Train custom model
const model = await pipeline.train({
  dataset,
  epochs: 3,
  learningRate: 1e-5,
  validationSplit: 0.1,
});

// Evaluate
const evaluation = await pipeline.evaluate(model, testCases);
console.log(`Accuracy improvement: ${evaluation.improvement}%`);

// Deploy
await pipeline.deploy(model, {
  agents: ['archeologist', 'builder'],
  environment: 'production',
});
```

### Data Collection

```typescript
import { DataCollector } from '@migrationpilot/finetuning';

const collector = new DataCollector();

// Collect examples from reviewed rules
collector.collect({
  type: 'rule-extraction',
  input: legacyCode,
  output: extractedRules,
  corrections: smeCorrections,
});

// Collect from code generation
collector.collect({
  type: 'code-generation',
  input: { rule, specification },
  output: generatedCode,
  rating: qualityRating,
});
```

## Training Strategies

### Domain Adaptation

Fine-tune on your organization's specific terminology, patterns, and business rules.

### Task-Specific

Train separate models for analysis, generation, and validation tasks.

### Continuous Learning

Improve models based on ongoing SME feedback and production metrics.

## Installation

```bash
pnpm add @migrationpilot/finetuning
```

## License

MIT
