---
sidebar_position: 1
---

# API Overview

The MigrationPilot API provides programmatic access to all platform features.

## Base URL

```
Production: https://api.migrationpilot.dev/v1
Local:      http://localhost:3001/api
```

## Authentication

### API Keys

Include your API key in the `X-API-Key` header:

```bash
curl -H "X-API-Key: mp_your_api_key" \
  https://api.migrationpilot.dev/v1/projects
```

### JWT Tokens

For user-authenticated requests, use Bearer tokens:

```bash
curl -H "Authorization: Bearer your_jwt_token" \
  https://api.migrationpilot.dev/v1/projects
```

## Common Response Format

All responses follow this structure:

```json
{
  "success": true,
  "data": { ... },
  "error": null
}
```

Error responses:

```json
{
  "success": false,
  "data": null,
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid source language",
    "details": { "field": "sourceLanguage" }
  }
}
```

## Rate Limiting

| Tier | Requests/minute | Requests/day |
|------|-----------------|--------------|
| Free | 60 | 1,000 |
| Starter | 300 | 10,000 |
| Professional | 1,000 | 100,000 |
| Enterprise | Unlimited | Unlimited |

Rate limit headers:

```
X-RateLimit-Limit: 60
X-RateLimit-Remaining: 45
X-RateLimit-Reset: 1706450400
```

## Endpoints Overview

The API is organized into 14 route groups:

### Core Endpoints

#### Projects

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/projects` | List all projects |
| `POST` | `/projects` | Create a project |
| `GET` | `/projects/:id` | Get project details |
| `PATCH` | `/projects/:id` | Update a project |
| `DELETE` | `/projects/:id` | Delete a project |
| `GET` | `/projects/:id/files` | List project source files |
| `POST` | `/projects/:id/files` | Upload source files |

#### Analysis

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/analysis/parse` | Parse source code |
| `POST` | `/analysis/analyze` | AI-powered analysis |
| `GET` | `/analysis/:projectId/rules` | Get business rules |
| `GET` | `/analysis/:projectId/structures` | Get data structures |
| `GET` | `/analysis/:projectId/complexity` | Get complexity metrics |

#### Migration

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/migration/start` | Start migration |
| `GET` | `/migration/:id/status` | Get status |
| `GET` | `/migration/:id/stream` | Stream progress (SSE) |
| `POST` | `/migration/:id/cancel` | Cancel migration |
| `GET` | `/migration/:id/generated` | Get generated files |

#### Validation

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/validation/run` | Run validation tests |
| `GET` | `/validation/:projectId/results` | Get test results |
| `GET` | `/validation/:projectId/report` | Get confidence report |
| `POST` | `/validation/:projectId/test-cases` | Generate test cases |

### Advanced Endpoints

#### Dashboard

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/dashboard/stats` | Get dashboard statistics |
| `GET` | `/dashboard/activity` | Get recent activity feed |
| `GET` | `/dashboard/projects` | Get projects summary |

#### Tests

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/tests/generate` | Generate test cases for rules |
| `GET` | `/tests/:projectId` | List test cases |
| `POST` | `/tests/:projectId/run` | Execute test suite |
| `GET` | `/tests/:projectId/coverage` | Get test coverage |

#### Explainer

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/explainer/code` | Explain code section |
| `POST` | `/explainer/rule` | Explain business rule |
| `POST` | `/explainer/diff` | Explain migration differences |

#### Export

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/export/:projectId` | Export migration artifacts |
| `GET` | `/export/:projectId/code` | Export generated code as ZIP |
| `GET` | `/export/:projectId/docs` | Export documentation |
| `GET` | `/export/:projectId/mappings` | Export source mappings |

### Infrastructure Endpoints

#### Routing (Strangler Fig)

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/routing/:projectId/rules` | Get routing rules |
| `POST` | `/routing/:projectId/rules` | Create routing rule |
| `PATCH` | `/routing/:projectId/rules/:id` | Update routing rule |
| `DELETE` | `/routing/:projectId/rules/:id` | Delete routing rule |
| `POST` | `/routing/:projectId/rollout` | Start rollout |
| `GET` | `/routing/:projectId/rollout/status` | Get rollout status |

#### Webhooks

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/webhooks` | List webhooks |
| `POST` | `/webhooks` | Create webhook |
| `PATCH` | `/webhooks/:id` | Update webhook |
| `DELETE` | `/webhooks/:id` | Delete webhook |
| `GET` | `/webhooks/:id/deliveries` | Get delivery history |
| `POST` | `/webhooks/:id/test` | Send test webhook |

### Observability Endpoints

#### Health

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/health` | Basic health check |
| `GET` | `/health/ready` | Readiness probe |
| `GET` | `/health/live` | Liveness probe |

#### Metrics

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/metrics` | Prometheus metrics |

### Query Endpoints

#### GraphQL

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/graphql` | GraphQL endpoint |
| `GET` | `/graphql` | GraphQL Playground (dev only) |

GraphQL provides flexible querying for complex data requirements:

```graphql
query GetProjectWithRules($id: ID!) {
  project(id: $id) {
    id
    name
    status
    businessRules {
      id
      name
      confidence
      reviewStatus
    }
    statistics {
      totalFiles
      totalRules
      completedRules
    }
  }
}
```

## Pagination

List endpoints support pagination:

```bash
GET /projects?page=1&pageSize=20
```

Response includes pagination info:

```json
{
  "data": [...],
  "total": 100,
  "page": 1,
  "pageSize": 20,
  "hasMore": true
}
```

## Filtering & Sorting

Use query parameters:

```bash
# Filter by status
GET /projects?status=in-progress

# Sort by date
GET /projects?sort=createdAt&order=desc

# Multiple filters
GET /projects?sourceLanguage=cobol&targetLanguage=java
```

## Streaming Responses

For long-running operations, use Server-Sent Events:

```bash
curl -N -H "X-API-Key: mp_your_api_key" \
  https://api.migrationpilot.dev/v1/migration/123/stream
```

Events:

```
event: progress
data: {"phase": "analysis", "progress": 45}

event: rule
data: {"id": "BR-001", "name": "Interest Calculation"}

event: complete
data: {"success": true, "duration": 120000}
```

## SDKs

### JavaScript/TypeScript

```bash
npm install @migrationpilot/sdk
```

```typescript
import { MigrationPilot } from '@migrationpilot/sdk';

const client = new MigrationPilot({ apiKey: 'mp_...' });

const project = await client.projects.create({
  name: 'Banking Migration',
  sourceLanguage: 'cobol',
  targetLanguage: 'java',
});

for await (const event of client.migration.start(project.id)) {
  console.log(event.phase, event.progress);
}
```

### Python

```bash
pip install migrationpilot
```

```python
from migrationpilot import MigrationPilot

client = MigrationPilot(api_key="mp_...")

project = client.projects.create(
    name="Banking Migration",
    source_language="cobol",
    target_language="java"
)

for event in client.migration.start(project.id):
    print(event.phase, event.progress)
```

## Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `UNAUTHORIZED` | 401 | Invalid or missing credentials |
| `FORBIDDEN` | 403 | Insufficient permissions |
| `NOT_FOUND` | 404 | Resource not found |
| `VALIDATION_ERROR` | 400 | Invalid request parameters |
| `RATE_LIMITED` | 429 | Too many requests |
| `INTERNAL_ERROR` | 500 | Server error |

## Webhooks

Configure webhooks to receive notifications:

```bash
POST /webhooks
{
  "url": "https://your-server.com/webhook",
  "events": ["migration.completed", "rule.needs_review"]
}
```

Webhook payload:

```json
{
  "event": "migration.completed",
  "timestamp": "2024-01-28T10:30:00Z",
  "data": {
    "projectId": "proj_123",
    "success": true,
    "duration": 120000
  }
}
```

## Next Steps

- [Authentication details](/docs/api/authentication)
- [Projects API](/docs/api/projects)
- [Migration API](/docs/api/migration)
