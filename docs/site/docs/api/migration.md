# Migration API

The Migration API manages code generation and transformation processes.

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| POST | `/api/projects/:id/migrate` | Start migration |
| GET | `/api/projects/:id/migration` | Get migration status |
| GET | `/api/projects/:id/migration/output` | Get generated code |
| POST | `/api/projects/:id/migration/cancel` | Cancel migration |
| POST | `/api/projects/:id/rules/:ruleId/approve` | Approve a rule |
| POST | `/api/projects/:id/rules/:ruleId/reject` | Reject a rule |
| PATCH | `/api/projects/:id/rules/:ruleId` | Modify a rule |

## Start Migration

```bash
POST /api/projects/:id/migrate
Content-Type: application/json
```

### Request Body

```json
{
  "targetLanguage": "java",
  "targetFramework": "Spring Boot",
  "options": {
    "generateTests": true,
    "generateDocumentation": true,
    "generateOpenAPI": true,
    "namingConvention": "camelCase",
    "packageStructure": "feature",
    "outputFormat": "zip"
  }
}
```

### Parameters

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `targetLanguage` | string | - | Target language |
| `targetFramework` | string | - | Target framework |
| `generateTests` | boolean | true | Generate unit tests |
| `generateDocumentation` | boolean | true | Generate docs |
| `generateOpenAPI` | boolean | false | Generate OpenAPI spec |
| `namingConvention` | string | `camelCase` | `camelCase`, `snake_case`, `PascalCase` |
| `packageStructure` | string | `feature` | `feature`, `layer`, `hexagonal` |
| `outputFormat` | string | `zip` | `zip`, `tar`, `files` |

### Response

```json
{
  "data": {
    "jobId": "mig_abc123",
    "status": "running",
    "startedAt": "2024-01-28T17:00:00Z",
    "progress": {
      "currentPhase": "architecture_design",
      "percentage": 0
    }
  }
}
```

## Get Migration Status

```bash
GET /api/projects/:id/migration
```

### Response (In Progress)

```json
{
  "data": {
    "jobId": "mig_abc123",
    "status": "running",
    "startedAt": "2024-01-28T17:00:00Z",
    "progress": {
      "currentPhase": "code_generation",
      "totalRules": 156,
      "processedRules": 89,
      "percentage": 57,
      "currentFile": "LoanCalculator.java"
    },
    "metrics": {
      "generatedFiles": 45,
      "generatedLines": 12500,
      "generatedTests": 89
    }
  }
}
```

### Response (Awaiting Review)

```json
{
  "data": {
    "jobId": "mig_abc123",
    "status": "awaiting_review",
    "pendingReviews": [
      {
        "ruleId": "rule_023",
        "name": "Complex Date Calculation",
        "confidence": 0.72,
        "reason": "Below confidence threshold",
        "requiresAction": true
      },
      {
        "ruleId": "rule_045",
        "name": "Currency Rounding Logic",
        "confidence": 0.68,
        "reason": "Multiple interpretations possible",
        "requiresAction": true
      }
    ]
  }
}
```

### Response (Completed)

```json
{
  "data": {
    "jobId": "mig_abc123",
    "status": "completed",
    "startedAt": "2024-01-28T17:00:00Z",
    "completedAt": "2024-01-28T17:35:00Z",
    "duration": 2100,
    "progress": {
      "currentPhase": "completed",
      "percentage": 100
    },
    "metrics": {
      "generatedFiles": 156,
      "generatedLines": 45000,
      "generatedTests": 312,
      "generatedDocs": 45,
      "codeQualityScore": 0.92
    },
    "output": {
      "downloadUrl": "/api/projects/proj_xyz789/migration/download",
      "expiresAt": "2024-01-29T17:35:00Z",
      "format": "zip",
      "size": 2457600
    }
  }
}
```

## Get Generated Code

```bash
GET /api/projects/:id/migration/output
```

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `format` | string | Output format: `zip`, `tar`, `json` |
| `include` | string | What to include: `code,tests,docs,all` |

### Response (JSON format)

```json
{
  "data": {
    "structure": {
      "src/main/java/com/example/loan": {
        "LoanCalculator.java": "package com.example.loan;\n\nimport...",
        "LoanValidator.java": "package com.example.loan;\n\nimport...",
        "model/Loan.java": "package com.example.loan.model;\n\n..."
      },
      "src/test/java/com/example/loan": {
        "LoanCalculatorTest.java": "package com.example.loan;\n\nimport..."
      }
    },
    "files": [
      {
        "path": "src/main/java/com/example/loan/LoanCalculator.java",
        "lines": 245,
        "sourceRules": ["rule_001", "rule_002", "rule_005"],
        "content": "package com.example.loan;\n\nimport..."
      }
    ]
  }
}
```

### Download ZIP

```bash
GET /api/projects/:id/migration/download
```

Returns the generated code as a downloadable ZIP file.

## Cancel Migration

```bash
POST /api/projects/:id/migration/cancel
```

### Response

```json
{
  "data": {
    "jobId": "mig_abc123",
    "status": "cancelled",
    "cancelledAt": "2024-01-28T17:15:00Z"
  }
}
```

## Rule Review

When rules require human review, use these endpoints.

### Approve Rule

```bash
POST /api/projects/:id/rules/:ruleId/approve
Content-Type: application/json
```

```json
{
  "comment": "Verified with business analyst"
}
```

### Reject Rule

```bash
POST /api/projects/:id/rules/:ruleId/reject
Content-Type: application/json
```

```json
{
  "reason": "Incorrect interpretation of date handling",
  "alternativeDescription": "Dates should be converted to UTC before calculation"
}
```

### Modify Rule

```bash
PATCH /api/projects/:id/rules/:ruleId
Content-Type: application/json
```

```json
{
  "description": "Updated rule description",
  "pseudocode": "interest = (principal * rate / 100) * (term / 12)",
  "testCases": [
    { "input": { "principal": 10000, "rate": 5, "term": 12 }, "expected": 500 }
  ]
}
```

### Response

```json
{
  "data": {
    "ruleId": "rule_023",
    "status": "approved",
    "reviewedAt": "2024-01-28T17:20:00Z",
    "reviewedBy": "user@example.com"
  }
}
```

## Migration Phases

| Phase | Description |
|-------|-------------|
| `architecture_design` | Designing target architecture |
| `api_design` | Designing API contracts |
| `code_generation` | Generating source code |
| `test_generation` | Generating unit tests |
| `doc_generation` | Generating documentation |
| `quality_check` | Running code quality checks |
| `packaging` | Packaging output |
| `awaiting_review` | Waiting for rule reviews |
| `completed` | Migration complete |

## Supported Targets

### Java/Spring Boot

```json
{
  "targetLanguage": "java",
  "targetFramework": "Spring Boot",
  "options": {
    "javaVersion": "17",
    "springVersion": "3.2",
    "buildTool": "maven"
  }
}
```

### Python/FastAPI

```json
{
  "targetLanguage": "python",
  "targetFramework": "FastAPI",
  "options": {
    "pythonVersion": "3.11",
    "packageManager": "poetry"
  }
}
```

### TypeScript/NestJS

```json
{
  "targetLanguage": "typescript",
  "targetFramework": "NestJS",
  "options": {
    "nodeVersion": "20",
    "packageManager": "pnpm"
  }
}
```

### Go/Gin

```json
{
  "targetLanguage": "go",
  "targetFramework": "Gin",
  "options": {
    "goVersion": "1.21",
    "useModules": true
  }
}
```

### C#/.NET Core

```json
{
  "targetLanguage": "csharp",
  "targetFramework": ".NET Core",
  "options": {
    "dotnetVersion": "8.0",
    "projectType": "webapi"
  }
}
```

## Error Responses

### 400 Bad Request

```json
{
  "error": "Cannot start migration",
  "message": "Analysis must be completed first"
}
```

### 409 Conflict

```json
{
  "error": "Migration already running",
  "jobId": "mig_abc123"
}
```

### 422 Unprocessable Entity

```json
{
  "error": "Rules require review",
  "pendingRules": 5
}
```

## CLI Usage

```bash
# Start migration
migrationpilot migrate --project proj_xyz789 --target java

# With options
migrationpilot migrate --project proj_xyz789 \
  --target java \
  --framework "Spring Boot" \
  --tests \
  --docs

# Check status
migrationpilot migrate --project proj_xyz789 --status

# Download output
migrationpilot migrate --project proj_xyz789 --download ./output
```

## Related Topics

- [CLI Migrate Command](../cli/migrate.md) - CLI reference
- [Validation API](./validation.md) - Validating generated code
- [Code Generators](../architecture/generators.md) - Generator architecture
