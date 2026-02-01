# @migrationpilot/api

MigrationPilot REST API server built with Hono.

## Overview

This is the main API server for MigrationPilot, providing REST and GraphQL endpoints for all migration operations. It coordinates with the AI agents, database, and storage services.

## Features

- **REST API**: Complete CRUD operations for projects, rules, and migrations
- **GraphQL**: Flexible queries for complex data requirements
- **Authentication**: JWT tokens and API key support
- **Authorization**: Role-based access control (RBAC)
- **Rate Limiting**: Protect against abuse
- **Audit Logging**: Complete action trail for compliance

## Quick Start

```bash
# Development
pnpm dev

# Production
pnpm build
pnpm start
```

The server runs on `http://localhost:3001` by default.

## API Endpoints

### Projects

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/projects` | List all projects |
| POST | `/api/projects` | Create a new project |
| GET | `/api/projects/:id` | Get project details |
| PUT | `/api/projects/:id` | Update project |
| DELETE | `/api/projects/:id` | Delete project |

### Analysis

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/analysis/analyze` | Analyze source code |
| GET | `/api/analysis/:projectId/rules` | Get extracted business rules |
| GET | `/api/analysis/:projectId/structures` | Get data structures |

### Migration

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/migration/start` | Start migration pipeline |
| GET | `/api/migration/:id/status` | Get migration status |
| POST | `/api/migration/:id/cancel` | Cancel migration |
| GET | `/api/migration/:id/artifacts` | Download generated code |

### Validation

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/validation/generate-tests` | Generate test cases |
| POST | `/api/validation/run` | Run equivalence tests |
| GET | `/api/validation/:id/report` | Get validation report |

### Dashboard

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/dashboard/stats` | Get dashboard statistics |
| GET | `/api/dashboard/recent` | Get recent activity |

### Health & Metrics

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check |
| GET | `/metrics` | Prometheus metrics |

## Authentication

### JWT Token

```bash
curl -X POST http://localhost:3001/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email": "user@example.com", "password": "secret"}'

# Response: { "token": "eyJ..." }

# Use token
curl http://localhost:3001/api/projects \
  -H "Authorization: Bearer eyJ..."
```

### API Key

```bash
curl http://localhost:3001/api/projects \
  -H "X-API-Key: mp_live_abc123..."
```

## GraphQL

The GraphQL endpoint is available at `/graphql`:

```graphql
query GetProject($id: ID!) {
  project(id: $id) {
    id
    name
    status
    confidence
    businessRules {
      id
      name
      confidence
      reviewStatus
    }
    generatedFiles {
      path
      language
      linesOfCode
    }
  }
}

mutation StartMigration($input: StartMigrationInput!) {
  startMigration(input: $input) {
    id
    status
  }
}
```

## Webhooks

Configure webhooks for real-time notifications:

```bash
curl -X POST http://localhost:3001/api/webhooks \
  -H "Authorization: Bearer ..." \
  -H "Content-Type: application/json" \
  -d '{
    "url": "https://your-server.com/webhook",
    "events": ["migration.complete", "validation.failed"],
    "secret": "your-webhook-secret"
  }'
```

Webhook events:
- `project.created` / `project.updated` / `project.deleted`
- `analysis.complete` / `analysis.failed`
- `migration.started` / `migration.complete` / `migration.failed`
- `validation.complete` / `validation.failed`
- `rule.review.requested` / `rule.review.complete`

## Environment Variables

```env
# Server
PORT=3001
NODE_ENV=development

# Database
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/migrationpilot

# Redis
REDIS_URL=redis://localhost:6379

# Authentication
JWT_SECRET=your-jwt-secret
JWT_EXPIRY=24h

# GitHub Copilot SDK
GITHUB_TOKEN=your-github-token

# Storage
S3_ENDPOINT=http://localhost:9000
S3_ACCESS_KEY=minioadmin
S3_SECRET_KEY=minioadmin
S3_BUCKET=migrationpilot

# Rate Limiting
RATE_LIMIT_MAX=100
RATE_LIMIT_WINDOW=60000
```

## Project Structure

```
src/
├── index.ts              # Entry point
├── routes/
│   ├── analysis.ts       # Analysis endpoints
│   ├── dashboard.ts      # Dashboard endpoints
│   ├── health.ts         # Health checks
│   ├── migration.ts      # Migration endpoints
│   ├── projects.ts       # Project CRUD
│   ├── validation.ts     # Validation endpoints
│   └── webhooks.ts       # Webhook management
├── middleware/
│   ├── auth.ts           # Authentication
│   ├── rate-limit.ts     # Rate limiting
│   └── error-handler.ts  # Error handling
├── services/
│   ├── analysis.ts       # Analysis service
│   ├── migration.ts      # Migration service
│   └── validation.ts     # Validation service
└── graphql/
    ├── schema.ts         # GraphQL schema
    └── resolvers.ts      # GraphQL resolvers
```

## Error Handling

All errors follow a consistent format:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid project configuration",
    "details": [
      { "field": "targetLanguage", "message": "Must be one of: java, python, typescript, go, csharp" }
    ]
  }
}
```

## Development

```bash
# Run with hot reload
pnpm dev

# Run tests
pnpm test

# Type check
pnpm typecheck

# Lint
pnpm lint
```

## Docker

```bash
# Build image
docker build -t migrationpilot-api .

# Run container
docker run -p 3001:3001 --env-file .env migrationpilot-api
```

## License

MIT
