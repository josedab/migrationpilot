# @migrationpilot/planner

AI-driven incremental migration planning with dependency analysis, risk scoring, and timeline estimation.

## Overview

This package provides intelligent migration planning, helping you determine the optimal order and approach for migrating complex legacy systems incrementally.

## Features

- **Dependency Analysis**: Map system dependencies
- **Risk Scoring**: Identify high-risk components
- **Timeline Estimation**: Realistic effort estimates
- **Wave Planning**: Group components into migration waves
- **Critical Path**: Identify blocking dependencies

## Usage

### Create Migration Plan

```typescript
import { MigrationPlanner } from '@migrationpilot/planner';

const planner = new MigrationPlanner();

// Analyze dependencies
const dependencies = await planner.analyzeDependencies(project);

// Generate migration plan
const plan = await planner.createPlan({
  project,
  dependencies,
  constraints: {
    maxWaveSize: 5,
    minConfidence: 0.9,
    teamCapacity: 10, // developers
  },
});

console.log(`Total waves: ${plan.waves.length}`);
console.log(`Estimated duration: ${plan.estimatedDuration}`);
console.log(`Total effort: ${plan.totalEffort} developer-days`);
```

### Risk Assessment

```typescript
import { RiskAssessor } from '@migrationpilot/planner';

const assessor = new RiskAssessor();

const risks = await assessor.assess(project, {
  factors: ['complexity', 'dependencies', 'testCoverage', 'smeAvailability'],
});

for (const risk of risks) {
  console.log(`
    Component: ${risk.component}
    Risk Level: ${risk.level}
    Factors: ${risk.factors.join(', ')}
    Mitigation: ${risk.mitigation}
  `);
}
```

### Wave Planning

```typescript
import { WavePlanner } from '@migrationpilot/planner';

const wavePlanner = new WavePlanner();

const waves = await wavePlanner.plan(components, {
  strategy: 'low-risk-first', // or 'high-value-first', 'dependency-order'
  maxParallel: 3,
});

for (const wave of waves) {
  console.log(`
    Wave ${wave.number}: ${wave.name}
    Components: ${wave.components.map(c => c.name).join(', ')}
    Duration: ${wave.estimatedDuration}
    Risk: ${wave.aggregateRisk}
  `);
}
```

### Timeline Visualization

```typescript
import { TimelineGenerator } from '@migrationpilot/planner';

const timeline = new TimelineGenerator();

// Generate Gantt chart
const gantt = timeline.generateGantt(plan, {
  format: 'mermaid',
  includeRisks: true,
});

// Generate roadmap
const roadmap = timeline.generateRoadmap(plan, {
  granularity: 'month',
  milestones: true,
});
```

## Planning Strategies

| Strategy | Description | Best For |
|----------|-------------|----------|
| `low-risk-first` | Migrate lowest risk components first | Risk-averse organizations |
| `high-value-first` | Prioritize highest business value | Quick wins |
| `dependency-order` | Follow dependency graph | Technical correctness |
| `strangler-fig` | Incremental replacement | Production systems |

## Installation

```bash
pnpm add @migrationpilot/planner
```

## License

MIT
