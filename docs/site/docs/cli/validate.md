# validate

Run equivalence validation between legacy and modern code to verify business logic is preserved.

## Usage

```bash
migrationpilot validate [options]
```

## Options

| Option | Description | Default |
|--------|-------------|---------|
| `-p, --project <name>` | Project name | - |
| `--legacy <endpoint>` | Legacy system endpoint | - |
| `--modern <endpoint>` | Modern system endpoint | - |
| `-c, --coverage <percent>` | Target coverage percentage | `95` |
| `-n, --iterations <count>` | Number of test iterations | `1000` |
| `-r, --report <file>` | Output report file | - |
| `--verbose` | Show detailed test output | `false` |

## Test Categories

The validation runs tests across multiple categories:

| Category | Description |
|----------|-------------|
| **Boundary Values** | Tests edge cases at data type boundaries |
| **Equivalence Classes** | Tests representative values from input partitions |
| **Random Inputs** | Fuzz testing with random valid inputs |
| **Edge Cases** | Domain-specific edge cases (dates, currencies, etc.) |

## Examples

### Basic validation

```bash
migrationpilot validate --project my-migration
```

### With detailed output

```bash
migrationpilot validate --project banking-system --verbose
```

### Generate a report

```bash
migrationpilot validate --project banking-system --report validation-report.json
```

### Custom coverage target

```bash
migrationpilot validate --project critical-system --coverage 99 --iterations 5000
```

### With endpoints

```bash
migrationpilot validate \
  --project banking-system \
  --legacy http://legacy.example.com/api \
  --modern http://modern.example.com/api
```

## Output

### Results Table

```
📊 Validation Results
──────────────────────────────────────────────────
┌──────────────────────┬────────┬────────┬──────────┐
│ Category             │ Passed │ Failed │ Coverage │
├──────────────────────┼────────┼────────┼──────────┤
│ Boundary Values      │ 45     │ 2      │ 96%      │
│ Equivalence Classes  │ 52     │ 1      │ 98%      │
│ Random Inputs        │ 48     │ 2      │ 94%      │
│ Edge Cases           │ 38     │ 5      │ 88%      │
├──────────────────────┼────────┼────────┼──────────┤
│ Total                │ 183    │ 10     │ 95%      │
└──────────────────────┴────────┴────────┴──────────┘

🎯 Confidence Score: 94.8%
```

### Verbose Output (failures)

```
❌ Failed Tests
──────────────────────────────────────────────────
  ✗ boundary_max_int
    Reason: Overflow handling differs
    Legacy: 2147483647
    Modern: ERROR

  ✗ date_leap_year
    Reason: Date parsing differs
    Legacy: 02/29/2000
    Modern: 2000-02-29
```

### Recommendations

```
💡 Recommendations
──────────────────────────────────────────────────
  ! Review integer overflow handling in boundary tests
  ! Standardize date format handling
  ✓ Core business logic equivalence is strong (98% pass rate)
```

## Report Format

When using `--report`, the output JSON contains:

```json
{
  "timestamp": "2024-01-28T15:30:00.000Z",
  "project": "banking-system",
  "summary": {
    "total": 193,
    "passed": 183,
    "failed": 10,
    "confidence": 94.8
  },
  "categories": [
    { "name": "Boundary Values", "passed": 45, "failed": 2 },
    { "name": "Equivalence Classes", "passed": 52, "failed": 1 },
    { "name": "Random Inputs", "passed": 48, "failed": 2 },
    { "name": "Edge Cases", "passed": 38, "failed": 5 }
  ]
}
```

## Confidence Score

The confidence score indicates how confident we are that the modern code behaves identically to the legacy code:

| Score | Interpretation |
|-------|----------------|
| ≥ 98% | Excellent - Ready for production |
| 90-97% | Good - Minor differences to review |
| 80-89% | Fair - Significant review needed |
| < 80% | Poor - Major issues detected |

## Notes

- Higher iteration counts provide more thorough testing but take longer
- Use `--verbose` to identify specific failing test cases
- The report file is useful for CI/CD integration and audit trails
