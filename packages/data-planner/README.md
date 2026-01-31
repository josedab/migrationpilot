# @migrationpilot/data-planner

AI-Powered data migration planner for legacy database modernization.

## Overview

This package provides intelligent planning for database migrations, handling schema transformation, data type mapping, and migration sequence optimization.

## Features

- **Schema Analysis**: Understand legacy database structures
- **Type Mapping**: Map legacy data types to modern equivalents
- **Migration Planning**: Optimal sequence for data migration
- **ETL Generation**: Generate extract-transform-load scripts
- **Data Validation**: Verify data integrity post-migration

## Usage

```typescript
import { DataPlanner } from '@migrationpilot/data-planner';

const planner = new DataPlanner();

// Analyze legacy schema
const schema = await planner.analyzeSchema({
  source: 'vsam',       // VSAM, IMS, DB2, etc.
  files: legacyFiles,
});

// Create migration plan
const plan = await planner.createPlan({
  source: schema,
  target: 'postgresql',
  options: {
    preserveHistory: true,
    batchSize: 10000,
  },
});

// Generate ETL scripts
const scripts = await planner.generateETL(plan);
```

## Installation

```bash
pnpm add @migrationpilot/data-planner
```

## License

MIT
