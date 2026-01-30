# Equivalence Testing

Equivalence testing validates that migrated code behaves identically to the original legacy system. This is the critical step that ensures business logic is preserved during modernization.

## Why Equivalence Testing?

Traditional testing approaches fall short for migrations:

| Approach | Problem |
|----------|---------|
| Unit tests | Only test what developers think to test |
| Integration tests | Don't guarantee behavioral equivalence |
| Manual testing | Too slow, incomplete coverage |
| Code review | Can't catch subtle behavioral differences |

Equivalence testing systematically proves that for any input, the legacy and modern systems produce the same output.

## Testing Strategies

MigrationPilot employs multiple testing strategies:

### 1. Boundary Value Testing

Tests values at the edges of input domains:

```typescript
// For a loan amount field (0 - 1,000,000)
const boundaryTests = [
  { input: 0, description: 'Minimum value' },
  { input: 1, description: 'Just above minimum' },
  { input: 999999, description: 'Just below maximum' },
  { input: 1000000, description: 'Maximum value' },
];
```

### 2. Equivalence Partitioning

Divides input space into classes that should behave similarly:

```typescript
// Credit score partitions
const partitions = [
  { range: [0, 579], class: 'poor' },
  { range: [580, 669], class: 'fair' },
  { range: [670, 739], class: 'good' },
  { range: [740, 799], class: 'very_good' },
  { range: [800, 850], class: 'excellent' },
];

// Test one value from each partition
```

### 3. Property-Based Testing

Defines properties that should always hold:

```typescript
// Property: Total should always equal sum of line items
property('invoice_total', (lineItems: LineItem[]) => {
  const legacyTotal = legacySystem.calculateTotal(lineItems);
  const modernTotal = modernSystem.calculateTotal(lineItems);
  return legacyTotal === modernTotal;
});
```

### 4. Differential Testing

Runs both systems with identical inputs and compares outputs:

```
Input → Legacy System  → Output A
     → Modern System → Output B
     
Compare(Output A, Output B) → Pass/Fail
```

### 5. Shadow Testing

Routes production traffic to both systems:

```
Production Request → Legacy (serves response)
                  → Modern (records only)
                  
Compare responses offline
```

## Test Case Generation

### From Business Rules

Each extracted business rule generates test cases:

```typescript
// Rule: Interest rate = base rate + 0.5% for savings accounts
const testCases = [
  {
    name: 'Savings account gets bonus',
    inputs: { accountType: 'SAVINGS', baseRate: 5.0 },
    expected: { rate: 5.5 },
  },
  {
    name: 'Checking account no bonus',
    inputs: { accountType: 'CHECKING', baseRate: 5.0 },
    expected: { rate: 5.0 },
  },
];
```

### From Historical Data

Extract test cases from production logs:

```sql
-- Sample real transactions for test cases
SELECT DISTINCT ON (transaction_type, amount_range)
  input_data,
  output_data
FROM transaction_logs
WHERE timestamp > NOW() - INTERVAL '30 days';
```

### AI-Generated

The Validator agent generates edge cases:

```typescript
const aiGeneratedTests = await validator.generateEdgeCases({
  rule: interestCalculationRule,
  strategies: ['boundary', 'null_handling', 'overflow'],
  count: 50,
});
```

## Comparison Logic

### Numeric Comparison

Handles floating-point precision:

```typescript
function compareNumeric(legacy: number, modern: number): boolean {
  const tolerance = 0.0001; // Configurable
  return Math.abs(legacy - modern) < tolerance;
}
```

### Date Comparison

Handles format differences:

```typescript
function compareDates(legacy: string, modern: string): boolean {
  // Legacy: "01/28/2024"
  // Modern: "2024-01-28"
  const legacyDate = parseDate(legacy, 'MM/DD/YYYY');
  const modernDate = parseDate(modern, 'YYYY-MM-DD');
  return legacyDate.isSame(modernDate, 'day');
}
```

### Object Comparison

Deep comparison with field mapping:

```typescript
function compareObjects(legacy: any, modern: any): ComparisonResult {
  const differences: Difference[] = [];
  
  for (const [legacyKey, modernKey] of fieldMappings) {
    if (!isEquivalent(legacy[legacyKey], modern[modernKey])) {
      differences.push({
        field: modernKey,
        legacy: legacy[legacyKey],
        modern: modern[modernKey],
      });
    }
  }
  
  return { equivalent: differences.length === 0, differences };
}
```

## Confidence Score

The equivalence confidence score is calculated:

```
Confidence = (Passed Tests / Total Tests) × 100

Weighted by:
- Critical path tests: 2x weight
- Edge case tests: 1.5x weight
- Standard tests: 1x weight
```

### Score Interpretation

| Score | Meaning | Recommendation |
|-------|---------|----------------|
| ≥99% | Excellent | Ready for production |
| 95-98% | Good | Review failing tests |
| 90-94% | Fair | Investigate differences |
| <90% | Poor | Major issues detected |

## Running Validation

### CLI

```bash
# Run all validation tests
migrationpilot validate --project my-project

# With verbose output
migrationpilot validate --project my-project --verbose

# Generate report
migrationpilot validate --project my-project --report report.json
```

### API

```bash
curl -X POST http://localhost:4000/api/projects/PROJECT_ID/validate \
  -H "Content-Type: application/json" \
  -d '{
    "strategies": ["boundary", "partition", "property"],
    "maxTests": 1000,
    "tolerance": 0.0001
  }'
```

## Handling Failures

### 1. Analyze the Difference

```bash
migrationpilot validate --project my-project --verbose
```

Output shows:
```
❌ test_boundary_max_amount
   Legacy:  $999,999.99
   Modern:  $999,999.989
   Diff:    $0.001 (rounding difference)
```

### 2. Categorize the Cause

| Cause | Action |
|-------|--------|
| Rounding difference | Adjust tolerance or fix precision |
| Data type mismatch | Update type mapping |
| Missing edge case | Add handling in modern code |
| Incorrect rule extraction | Re-analyze and regenerate |

### 3. Fix and Re-validate

```bash
# After fixing
migrationpilot validate --project my-project --rerun-failed
```

## Best Practices

### 1. Test Early and Often

Run validation after each migration phase, not just at the end.

### 2. Use Production-Like Data

Sanitized production data provides realistic test cases.

### 3. Automate Comparison

Never manually compare outputs—automate everything.

### 4. Track Confidence Over Time

Monitor confidence score trends:

```
Week 1: 85% → Week 2: 92% → Week 3: 97% → Week 4: 99.2%
```

### 5. Document Accepted Differences

Some differences may be intentional:

```json
{
  "acceptedDifferences": [
    {
      "field": "timestamp",
      "reason": "Modern uses ISO 8601 format"
    },
    {
      "field": "precision",
      "reason": "Modern uses 4 decimal places vs legacy 2"
    }
  ]
}
```

## Related Topics

- [Business Rules](./business-rules.md) - Source of test cases
- [Validator Agent](./ai-agents.md#validator) - AI-powered test generation
- [Validate Command](../cli/validate.md) - CLI reference
