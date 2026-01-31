# @migrationpilot/differential

Behavioral differential analysis engine for legacy code migration.

## Overview

This package provides tools for comparing the behavioral differences between legacy and modern systems, identifying discrepancies and their root causes.

## Features

- **Behavioral Comparison**: Compare outputs across systems
- **Discrepancy Detection**: Identify and classify differences
- **Root Cause Analysis**: Determine why systems differ
- **Tolerance Handling**: Configure acceptable variations
- **Report Generation**: Detailed differential reports

## Usage

```typescript
import { DifferentialAnalyzer } from '@migrationpilot/differential';

const analyzer = new DifferentialAnalyzer({
  tolerances: {
    numeric: 0.001,
    date: '1s',
  },
});

// Compare system outputs
const result = await analyzer.compare({
  legacy: legacyOutputs,
  modern: modernOutputs,
  testCases: testCases,
});

// Analyze discrepancies
for (const diff of result.discrepancies) {
  console.log(`
    Test: ${diff.testCase.name}
    Field: ${diff.field}
    Legacy: ${diff.legacyValue}
    Modern: ${diff.modernValue}
    Cause: ${diff.analysis.rootCause}
  `);
}
```

## Installation

```bash
pnpm add @migrationpilot/differential
```

## License

MIT
