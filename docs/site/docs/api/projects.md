# Projects API

The Projects API allows you to create and manage migration projects.

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/api/projects` | List all projects |
| POST | `/api/projects` | Create a project |
| GET | `/api/projects/:id` | Get project details |
| PATCH | `/api/projects/:id` | Update a project |
| DELETE | `/api/projects/:id` | Delete a project |
| POST | `/api/projects/:id/files` | Upload source files |
| GET | `/api/projects/:id/files` | List project files |
| GET | `/api/projects/:id/files/:fileId` | Get file content |
| DELETE | `/api/projects/:id/files/:fileId` | Delete a file |

## List Projects

```bash
GET /api/projects
```

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `status` | string | Filter by status |
| `page` | number | Page number (default: 1) |
| `limit` | number | Results per page (default: 20) |

### Response

```json
{
  "data": [
    {
      "id": "proj_abc123",
      "name": "Banking Modernization",
      "status": "analyzing",
      "sourceLanguage": "cobol",
      "targetLanguage": "java",
      "statistics": {
        "totalFiles": 42,
        "totalLines": 125000,
        "extractedRules": 156
      },
      "createdAt": "2024-01-15T10:00:00Z",
      "updatedAt": "2024-01-28T15:30:00Z"
    }
  ],
  "total": 5,
  "page": 1,
  "limit": 20
}
```

## Create Project

```bash
POST /api/projects
Content-Type: application/json
```

### Request Body

```json
{
  "name": "Loan System Migration",
  "description": "Modernize COBOL loan processing system",
  "sourceLanguage": "cobol",
  "targetLanguage": "java",
  "targetFramework": "Spring Boot",
  "settings": {
    "enableStranglerFig": false,
    "generateTests": true,
    "generateDocumentation": true,
    "humanReviewRequired": true,
    "confidenceThreshold": 0.8
  }
}
```

### Parameters

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Project name (1-100 chars) |
| `description` | string | No | Project description |
| `sourceLanguage` | enum | Yes | `cobol`, `fortran`, `vb6`, `vba`, `java-legacy` |
| `targetLanguage` | enum | Yes | `java`, `python`, `typescript`, `go`, `csharp` |
| `targetFramework` | string | No | Target framework name |
| `settings` | object | No | Project settings |

### Response

```json
{
  "data": {
    "id": "proj_xyz789",
    "name": "Loan System Migration",
    "description": "Modernize COBOL loan processing system",
    "sourceLanguage": "cobol",
    "targetLanguage": "java",
    "targetFramework": "Spring Boot",
    "status": "draft",
    "settings": {
      "enableStranglerFig": false,
      "generateTests": true,
      "generateDocumentation": true,
      "humanReviewRequired": true,
      "confidenceThreshold": 0.8
    },
    "statistics": {
      "totalFiles": 0,
      "totalLines": 0,
      "analyzedFiles": 0,
      "extractedRules": 0
    },
    "createdAt": "2024-01-28T16:00:00Z",
    "updatedAt": "2024-01-28T16:00:00Z"
  }
}
```

## Get Project

```bash
GET /api/projects/:id
```

### Response

```json
{
  "data": {
    "id": "proj_xyz789",
    "name": "Loan System Migration",
    "description": "Modernize COBOL loan processing system",
    "sourceLanguage": "cobol",
    "targetLanguage": "java",
    "targetFramework": "Spring Boot",
    "status": "analysis_complete",
    "settings": { ... },
    "statistics": {
      "totalFiles": 42,
      "totalLines": 125000,
      "analyzedFiles": 42,
      "analyzedLines": 125000,
      "extractedRules": 156,
      "generatedFiles": 0,
      "testsGenerated": 0,
      "equivalenceScore": 0
    },
    "createdAt": "2024-01-28T16:00:00Z",
    "updatedAt": "2024-01-28T18:30:00Z"
  }
}
```

## Update Project

```bash
PATCH /api/projects/:id
Content-Type: application/json
```

### Request Body

```json
{
  "name": "Updated Project Name",
  "description": "Updated description",
  "settings": {
    "confidenceThreshold": 0.9
  }
}
```

### Response

Returns the updated project.

## Delete Project

```bash
DELETE /api/projects/:id
```

### Response

```json
{
  "success": true
}
```

## Upload Files

Upload source files to a project.

```bash
POST /api/projects/:id/files
Content-Type: application/json
```

### Single File (JSON)

```json
{
  "filename": "LOANCALC.cbl",
  "content": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. LOANCALC.\n..."
}
```

### Multiple Files (JSON Array)

```json
[
  { "filename": "LOANCALC.cbl", "content": "..." },
  { "filename": "LOANVAL.cbl", "content": "..." },
  { "filename": "COPYBOOK.cpy", "content": "..." }
]
```

### Multipart Form Data

```bash
curl -X POST http://localhost:4000/api/projects/proj_xyz789/files \
  -F "files=@LOANCALC.cbl" \
  -F "files=@LOANVAL.cbl"
```

### Response

```json
{
  "data": {
    "files": [
      {
        "id": "file_abc123",
        "filename": "LOANCALC.cbl",
        "lines": 450,
        "language": "cobol"
      },
      {
        "id": "file_def456",
        "filename": "LOANVAL.cbl",
        "lines": 230,
        "language": "cobol"
      }
    ],
    "totalUploaded": 2,
    "projectStatistics": {
      "totalFiles": 2,
      "totalLines": 680
    }
  }
}
```

## List Files

```bash
GET /api/projects/:id/files
```

### Response

```json
{
  "data": [
    {
      "id": "file_abc123",
      "filename": "LOANCALC.cbl",
      "lines": 450,
      "uploadedAt": "2024-01-28T16:30:00Z"
    },
    {
      "id": "file_def456",
      "filename": "LOANVAL.cbl",
      "lines": 230,
      "uploadedAt": "2024-01-28T16:30:00Z"
    }
  ],
  "total": 2
}
```

## Get File Content

```bash
GET /api/projects/:id/files/:fileId
```

### Response

```json
{
  "data": {
    "id": "file_abc123",
    "filename": "LOANCALC.cbl",
    "content": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. LOANCALC.\n...",
    "lines": 450,
    "uploadedAt": "2024-01-28T16:30:00Z"
  }
}
```

## Delete File

```bash
DELETE /api/projects/:id/files/:fileId
```

### Response

```json
{
  "success": true
}
```

## Project Status Values

| Status | Description |
|--------|-------------|
| `draft` | Project created, no analysis started |
| `analyzing` | Code analysis in progress |
| `analysis_complete` | Analysis finished |
| `designing` | Architecture design in progress |
| `design_complete` | Architecture design finished |
| `generating` | Code generation in progress |
| `generation_complete` | Code generation finished |
| `validating` | Equivalence testing in progress |
| `validation_complete` | Validation finished |
| `completed` | Migration completed successfully |
| `failed` | Migration failed |

## Error Responses

### 400 Bad Request

```json
{
  "error": "Validation error",
  "details": [
    {
      "field": "sourceLanguage",
      "message": "Invalid enum value"
    }
  ]
}
```

### 404 Not Found

```json
{
  "error": "Project not found"
}
```

## Related Topics

- [Authentication](./authentication.md) - API authentication
- [Analysis API](./analysis.md) - Running analysis
- [CLI Project Command](../cli/project.md) - CLI reference
