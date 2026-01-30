# Validation API

The Validation API runs equivalence testing to verify generated code matches original behavior.

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| POST | `/api/projects/:id/validate` | Start validation |
| GET | `/api/projects/:id/validation` | Get validation status |
| GET | `/api/projects/:id/validation/results` | Get validation results |
| GET | `/api/projects/:id/validation/report` | Get validation report |
| POST | `/api/projects/:id/validation/cancel` | Cancel validation |

## Start Validation

```bash
POST /api/projects/:id/validate
Content-Type: application/json
```

### Request Body

```json
{
  "options": {
    "testMode": "comprehensive",
    "includeEdgeCases": true,
    "includeFuzzTests": true,
    "parallelExecutions": 4,
    "timeout": 300,
    "coverageThreshold": 80
  }
}
```

### Parameters

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `testMode` | string | `comprehensive` | `quick`, `standard`, `comprehensive` |
| `includeEdgeCases` | boolean | true | Test boundary conditions |
| `includeFuzzTests` | boolean | false | Include fuzz testing |
| `parallelExecutions` | number | 4 | Parallel test execution |
| `timeout` | number | 300 | Test timeout in seconds |
| `coverageThreshold` | number | 80 | Minimum coverage % |

### Response

```json
{
  "data": {
    "jobId": "val_abc123",
    "status": "running",
    "startedAt": "2024-01-28T18:00:00Z",
    "progress": {
      "currentPhase": "test_generation",
      "percentage": 0
    }
  }
}
```

## Get Validation Status

```bash
GET /api/projects/:id/validation
```

### Response (In Progress)

```json
{
  "data": {
    "jobId": "val_abc123",
    "status": "running",
    "startedAt": "2024-01-28T18:00:00Z",
    "progress": {
      "currentPhase": "test_execution",
      "totalTests": 312,
      "executedTests": 156,
      "passedTests": 152,
      "failedTests": 4,
      "percentage": 50
    }
  }
}
```

### Response (Completed)

```json
{
  "data": {
    "jobId": "val_abc123",
    "status": "completed",
    "startedAt": "2024-01-28T18:00:00Z",
    "completedAt": "2024-01-28T18:15:00Z",
    "duration": 900,
    "progress": {
      "currentPhase": "completed",
      "percentage": 100
    },
    "summary": {
      "totalTests": 312,
      "passedTests": 304,
      "failedTests": 8,
      "skippedTests": 0,
      "equivalenceScore": 0.974,
      "codeCoverage": 0.87,
      "branchCoverage": 0.82
    }
  }
}
```

## Get Validation Results

```bash
GET /api/projects/:id/validation/results
```

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `status` | string | Filter: `all`, `passed`, `failed` |
| `ruleId` | string | Filter by rule ID |
| `page` | number | Page number |
| `limit` | number | Results per page |

### Response

```json
{
  "data": {
    "summary": {
      "totalTests": 312,
      "passedTests": 304,
      "failedTests": 8,
      "equivalenceScore": 0.974,
      "byCategory": {
        "calculation": { "total": 120, "passed": 118, "failed": 2 },
        "validation": { "total": 90, "passed": 89, "failed": 1 },
        "decision": { "total": 64, "passed": 59, "failed": 5 },
        "transformation": { "total": 38, "passed": 38, "failed": 0 }
      }
    },
    "tests": [
      {
        "id": "test_001",
        "ruleId": "rule_001",
        "ruleName": "Calculate Loan Interest",
        "status": "passed",
        "input": {
          "principal": 10000,
          "rate": 5.5,
          "term": 12
        },
        "expectedOutput": {
          "interest": 550.00
        },
        "actualOutput": {
          "interest": 550.00
        },
        "executionTime": 12,
        "coverage": 0.95
      },
      {
        "id": "test_045",
        "ruleId": "rule_012",
        "ruleName": "Currency Rounding",
        "status": "failed",
        "input": {
          "amount": 100.555,
          "roundingMode": "HALF_UP"
        },
        "expectedOutput": {
          "result": 100.56
        },
        "actualOutput": {
          "result": 100.55
        },
        "diff": {
          "field": "result",
          "expected": 100.56,
          "actual": 100.55,
          "difference": 0.01
        },
        "executionTime": 8,
        "coverage": 0.90
      }
    ],
    "pagination": {
      "page": 1,
      "limit": 20,
      "total": 312,
      "totalPages": 16
    }
  }
}
```

## Get Validation Report

```bash
GET /api/projects/:id/validation/report
```

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `format` | string | `json`, `html`, `pdf` |

### Response (JSON)

```json
{
  "data": {
    "projectId": "proj_xyz789",
    "projectName": "Loan System Migration",
    "generatedAt": "2024-01-28T18:15:00Z",
    "summary": {
      "equivalenceScore": 0.974,
      "totalRules": 156,
      "testedRules": 156,
      "passedRules": 148,
      "partiallyPassedRules": 6,
      "failedRules": 2,
      "recommendation": "READY_FOR_REVIEW"
    },
    "metrics": {
      "codeCoverage": 0.87,
      "branchCoverage": 0.82,
      "lineCoverage": 0.91,
      "mutationScore": 0.78
    },
    "issues": [
      {
        "severity": "high",
        "ruleId": "rule_012",
        "ruleName": "Currency Rounding",
        "description": "Rounding behavior differs from original",
        "impact": "Financial calculations may be off by pennies",
        "recommendation": "Review rounding implementation in CurrencyUtils.java"
      },
      {
        "severity": "medium",
        "ruleId": "rule_045",
        "ruleName": "Date Boundary Check",
        "description": "Edge case handling differs at month boundaries",
        "impact": "Date calculations may differ on last day of month",
        "recommendation": "Add explicit month-end handling"
      }
    ],
    "recommendations": [
      "Fix high-severity currency rounding issue before deployment",
      "Add additional test cases for date boundary conditions",
      "Consider manual review of partially passed rules"
    ],
    "certification": {
      "ready": false,
      "blockers": ["High severity issue: Currency Rounding"],
      "requirements": [
        { "name": "Equivalence Score > 95%", "met": true },
        { "name": "No High Severity Issues", "met": false },
        { "name": "Code Coverage > 80%", "met": true }
      ]
    }
  }
}
```

## Cancel Validation

```bash
POST /api/projects/:id/validation/cancel
```

### Response

```json
{
  "data": {
    "jobId": "val_abc123",
    "status": "cancelled",
    "cancelledAt": "2024-01-28T18:05:00Z"
  }
}
```

## Validation Phases

| Phase | Description |
|-------|-------------|
| `test_generation` | Generating test cases |
| `test_execution` | Running tests |
| `coverage_analysis` | Analyzing coverage |
| `report_generation` | Generating reports |
| `completed` | Validation complete |

## Test Modes

### Quick Mode

Fast validation focusing on critical paths:

```json
{
  "testMode": "quick",
  "includeEdgeCases": false,
  "includeFuzzTests": false
}
```

- ~100 core test cases
- ~2 minutes execution
- Basic equivalence check

### Standard Mode

Balanced validation:

```json
{
  "testMode": "standard",
  "includeEdgeCases": true,
  "includeFuzzTests": false
}
```

- ~300 test cases
- ~10 minutes execution
- Edge case coverage

### Comprehensive Mode

Full validation with fuzzing:

```json
{
  "testMode": "comprehensive",
  "includeEdgeCases": true,
  "includeFuzzTests": true
}
```

- ~500+ test cases
- ~30 minutes execution
- Fuzz testing included

## Equivalence Score

The equivalence score measures behavioral equivalence:

| Score | Rating | Description |
|-------|--------|-------------|
| 95-100% | Excellent | Ready for production |
| 90-94% | Good | Minor issues to review |
| 80-89% | Fair | Significant review needed |
| < 80% | Poor | Major issues present |

## Error Responses

### 400 Bad Request

```json
{
  "error": "Cannot start validation",
  "message": "Migration must be completed first"
}
```

### 409 Conflict

```json
{
  "error": "Validation already running",
  "jobId": "val_abc123"
}
```

## CLI Usage

```bash
# Start validation
migrationpilot validate --project proj_xyz789

# With options
migrationpilot validate --project proj_xyz789 \
  --mode comprehensive \
  --coverage 90

# Check status
migrationpilot validate --project proj_xyz789 --status

# Get report
migrationpilot validate --project proj_xyz789 --report --format pdf
```

## Related Topics

- [Equivalence Testing](../concepts/equivalence-testing.md) - Testing concepts
- [CLI Validate Command](../cli/validate.md) - CLI reference
- [Testing Package](../architecture/testing.md) - Testing architecture
