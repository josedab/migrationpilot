# Analysis API

The Analysis API triggers and monitors code analysis jobs.

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| POST | `/api/projects/:id/analyze` | Start analysis |
| GET | `/api/projects/:id/analysis` | Get analysis status |
| GET | `/api/projects/:id/analysis/results` | Get analysis results |
| POST | `/api/projects/:id/analysis/cancel` | Cancel analysis |

## Start Analysis

```bash
POST /api/projects/:id/analyze
Content-Type: application/json
```

### Request Body

```json
{
  "options": {
    "extractBusinessRules": true,
    "extractDataFlows": true,
    "extractCallGraphs": true,
    "extractDependencies": true,
    "maxParallelFiles": 5
  }
}
```

### Parameters

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `extractBusinessRules` | boolean | true | Extract business logic rules |
| `extractDataFlows` | boolean | true | Analyze data flow patterns |
| `extractCallGraphs` | boolean | true | Build call dependency graphs |
| `extractDependencies` | boolean | true | Identify external dependencies |
| `maxParallelFiles` | number | 5 | Parallel file processing limit |

### Response

```json
{
  "data": {
    "jobId": "job_abc123",
    "status": "running",
    "startedAt": "2024-01-28T16:45:00Z",
    "progress": {
      "currentPhase": "parsing",
      "totalFiles": 42,
      "processedFiles": 0,
      "percentage": 0
    }
  }
}
```

## Get Analysis Status

```bash
GET /api/projects/:id/analysis
```

### Response (In Progress)

```json
{
  "data": {
    "jobId": "job_abc123",
    "status": "running",
    "startedAt": "2024-01-28T16:45:00Z",
    "progress": {
      "currentPhase": "rule_extraction",
      "totalFiles": 42,
      "processedFiles": 38,
      "percentage": 90,
      "currentFile": "LOANCALC.cbl"
    },
    "metrics": {
      "parsedFiles": 42,
      "extractedRules": 134,
      "warnings": 3,
      "errors": 0
    }
  }
}
```

### Response (Completed)

```json
{
  "data": {
    "jobId": "job_abc123",
    "status": "completed",
    "startedAt": "2024-01-28T16:45:00Z",
    "completedAt": "2024-01-28T16:52:00Z",
    "duration": 420,
    "progress": {
      "currentPhase": "completed",
      "totalFiles": 42,
      "processedFiles": 42,
      "percentage": 100
    },
    "metrics": {
      "parsedFiles": 42,
      "totalLines": 125000,
      "extractedRules": 156,
      "dataFlows": 89,
      "callGraphNodes": 234,
      "dependencies": 12,
      "warnings": 3,
      "errors": 0
    }
  }
}
```

## Get Analysis Results

```bash
GET /api/projects/:id/analysis/results
```

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `include` | string | Comma-separated: `rules,dataflows,callgraph,dependencies` |
| `page` | number | Page number (for rules) |
| `limit` | number | Results per page |

### Response

```json
{
  "data": {
    "summary": {
      "totalFiles": 42,
      "totalLines": 125000,
      "extractedRules": 156,
      "rulesByConfidence": {
        "high": 98,
        "medium": 45,
        "low": 13
      },
      "rulesByCategory": {
        "calculation": 67,
        "validation": 45,
        "decision": 32,
        "transformation": 12
      }
    },
    "rules": [
      {
        "id": "rule_001",
        "name": "Calculate Loan Interest",
        "category": "calculation",
        "confidence": 0.95,
        "description": "Calculates monthly interest based on principal, rate, and term",
        "sourceFile": "LOANCALC.cbl",
        "sourceLines": [120, 145],
        "pseudocode": "interest = (principal * rate * term) / 12",
        "inputs": ["principal", "rate", "term"],
        "outputs": ["interest"],
        "status": "pending"
      },
      {
        "id": "rule_002",
        "name": "Validate Loan Amount",
        "category": "validation",
        "confidence": 0.88,
        "description": "Validates loan amount is within allowed limits",
        "sourceFile": "LOANVAL.cbl",
        "sourceLines": [50, 78],
        "pseudocode": "if loan_amount < MIN_LOAN or loan_amount > MAX_LOAN then reject",
        "inputs": ["loan_amount", "loan_type"],
        "outputs": ["is_valid", "error_message"],
        "status": "pending"
      }
    ],
    "dataFlows": [
      {
        "id": "flow_001",
        "name": "Customer Data Flow",
        "sources": ["CUSTMAST.dat", "CUSTINP.cbl"],
        "transformations": ["CUSTPROC.cbl", "CUSTVAL.cbl"],
        "sinks": ["CUSTOUT.dat", "CUSTRPT.cbl"]
      }
    ],
    "callGraph": {
      "nodes": [
        { "id": "MAINPROG", "type": "program", "calls": ["LOANCALC", "LOANVAL"] },
        { "id": "LOANCALC", "type": "program", "calls": ["INTCALC", "DATECALC"] },
        { "id": "LOANVAL", "type": "program", "calls": ["CUSTVAL", "AMTVAL"] }
      ]
    },
    "dependencies": {
      "external": [
        { "name": "DB2", "type": "database", "usedBy": ["CUSTMAST", "LOANMAST"] },
        { "name": "MQ Series", "type": "messaging", "usedBy": ["LOANOUT"] }
      ],
      "copybooks": ["CUSTCOPY.cpy", "LOANCOPY.cpy", "ERRCOPY.cpy"]
    }
  }
}
```

## Cancel Analysis

```bash
POST /api/projects/:id/analysis/cancel
```

### Response

```json
{
  "data": {
    "jobId": "job_abc123",
    "status": "cancelled",
    "cancelledAt": "2024-01-28T16:48:00Z"
  }
}
```

## Analysis Phases

| Phase | Description |
|-------|-------------|
| `parsing` | Parsing source files into AST |
| `rule_extraction` | Extracting business rules |
| `dataflow_analysis` | Analyzing data flows |
| `callgraph_building` | Building call graphs |
| `dependency_mapping` | Mapping dependencies |
| `report_generation` | Generating reports |
| `completed` | Analysis complete |

## Webhooks

Configure webhooks to receive analysis status updates:

```bash
POST /api/projects/:id/webhooks
Content-Type: application/json
```

```json
{
  "url": "https://your-server.com/webhook",
  "events": ["analysis.started", "analysis.progress", "analysis.completed", "analysis.failed"]
}
```

### Webhook Payload

```json
{
  "event": "analysis.completed",
  "projectId": "proj_xyz789",
  "jobId": "job_abc123",
  "timestamp": "2024-01-28T16:52:00Z",
  "data": {
    "status": "completed",
    "duration": 420,
    "metrics": {
      "parsedFiles": 42,
      "extractedRules": 156
    }
  }
}
```

## Error Responses

### 400 Bad Request

```json
{
  "error": "Cannot start analysis",
  "message": "Project has no uploaded files"
}
```

### 409 Conflict

```json
{
  "error": "Analysis already running",
  "jobId": "job_abc123"
}
```

## CLI Usage

```bash
# Start analysis
migrationpilot analyze --project proj_xyz789

# Start with options
migrationpilot analyze --project proj_xyz789 --parallel 10

# Check status
migrationpilot analyze --project proj_xyz789 --status

# Wait for completion
migrationpilot analyze --project proj_xyz789 --wait
```

## Related Topics

- [Business Rules](../concepts/business-rules.md) - Rule extraction concepts
- [CLI Analyze Command](../cli/analyze.md) - CLI reference
- [Migration API](./migration.md) - Running migrations
