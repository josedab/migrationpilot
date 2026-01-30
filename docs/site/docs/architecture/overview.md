---
sidebar_position: 1
---

# Architecture Overview

MigrationPilot is built as a monorepo with multiple packages and applications working together to provide a complete legacy code modernization platform.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           MigrationPilot                                 │
├─────────────────────────────────────────────────────────────────────────┤
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                     Frontend (Next.js)                             │  │
│  │  Dashboard │ Code Explorer │ Rule Review │ Test Results            │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                    │                                     │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                       API Layer (Hono)                             │  │
│  │                 REST + SSE for streaming                           │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                    │                                     │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                Agent Orchestrator (Copilot SDK)                    │  │
│  │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌──────────┐  │  │
│  │  │ Archeologist │ │  Architect   │ │   Builder    │ │Validator │  │  │
│  │  │    Agent     │ │    Agent     │ │    Agent     │ │  Agent   │  │  │
│  │  └──────────────┘ └──────────────┘ └──────────────┘ └──────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                    │                                     │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                      Parser Engine                                 │  │
│  │  ┌─────────┐  ┌──────────┐  ┌───────┐  ┌──────────────┐           │  │
│  │  │  COBOL  │  │ Fortran  │  │  VB6  │  │ Legacy Java  │           │  │
│  │  └─────────┘  └──────────┘  └───────┘  └──────────────┘           │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                    │                                     │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                      Infrastructure                                │  │
│  │  ┌────────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐            │  │
│  │  │ PostgreSQL │  │  Redis  │  │  MinIO  │  │  Kafka  │            │  │
│  │  └────────────┘  └─────────┘  └─────────┘  └─────────┘            │  │
│  └───────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

## Repository Structure

```
migrationpilot/
├── apps/
│   ├── api/            # Hono API server
│   ├── web/            # Next.js frontend
│   └── cli/            # Command-line tool
│
├── packages/
│   ├── agents/         # AI agent implementations
│   ├── parsers/        # Language parsers
│   ├── core/           # Shared types and utilities
│   ├── database/       # Database schema (Drizzle)
│   └── ui/             # Shared UI components
│
├── infrastructure/
│   ├── docker/         # Docker configurations
│   ├── terraform/      # Cloud infrastructure
│   ├── helm/           # Kubernetes charts
│   └── on-premises/    # On-prem deployment
│
├── docs/               # Documentation
└── examples/           # Sample legacy code
```

## Technology Stack

| Layer | Technology | Purpose |
|-------|------------|---------|
| Frontend | Next.js 14 | Web dashboard and SSR |
| API | Hono | Fast, lightweight API server |
| AI | Copilot SDK | Multi-agent orchestration |
| Parsers | ANTLR/Tree-sitter concepts | Legacy language parsing |
| Database | PostgreSQL + Drizzle | Persistent storage |
| Cache | Redis | Session state and caching |
| Storage | MinIO/S3 | Code artifact storage |
| Queue | Kafka/Redis Streams | Job processing |

## Package Details

### @migrationpilot/core

Shared types, utilities, and constants:

```typescript
// Types for the entire platform
export type SourceLanguage = 'cobol' | 'fortran' | 'vb6' | 'java-legacy';
export type TargetLanguage = 'java' | 'python' | 'typescript' | 'go' | 'csharp';

export interface Project { ... }
export interface BusinessRule { ... }
export interface TestResult { ... }
```

### @migrationpilot/agents

AI agent implementations using Copilot SDK:

```typescript
// Base agent class
abstract class BaseAgent {
  abstract analyze(context: AgentContext): Promise<AgentResult>;
}

// Specialized agents
class ArcheologistAgent extends BaseAgent { ... }
class ArchitectAgent extends BaseAgent { ... }
class BuilderAgent extends BaseAgent { ... }
class ValidatorAgent extends BaseAgent { ... }

// Orchestrator coordinates all agents
class MigrationOrchestrator {
  async migrate(project: Project): AsyncGenerator<MigrationProgress>;
}
```

### @migrationpilot/parsers

Language-specific parsers:

```typescript
// Common interface
abstract class BaseParser {
  abstract parse(source: string, filename: string): ParseResult;
  abstract getSupportedDialects(): string[];
}

// Language implementations
class CobolParser extends BaseParser { ... }
class FortranParser extends BaseParser { ... }
class VB6Parser extends BaseParser { ... }
class JavaLegacyParser extends BaseParser { ... }

// Factory function
function createParser(language: SourceLanguage): BaseParser;
```

### @migrationpilot/database

Drizzle ORM schema and migrations:

```typescript
// Tables
export const projects = pgTable('projects', { ... });
export const sourceFiles = pgTable('source_files', { ... });
export const businessRules = pgTable('business_rules', { ... });
export const generatedFiles = pgTable('generated_files', { ... });
export const testResults = pgTable('test_results', { ... });
```

### @migrationpilot/ui

Shared React components:

```typescript
// Core components
export { Button, Card, Badge, Progress, Tabs };

// Migration-specific components
export { CodeViewer, CodeComparison, RuleCard };
export { MigrationProgress, TestResultsTable };
```

## Data Flow

### Migration Request Flow

```
1. User creates project (Web/CLI/API)
2. API validates and stores project
3. API triggers migration job
4. Orchestrator coordinates agents:
   a. Archeologist analyzes code
   b. (Optional) Human reviews rules
   c. Architect designs structure
   d. Builder generates code
   e. Validator runs tests
5. Results streamed back via SSE
6. User reviews in dashboard
```

### Agent Communication

```
                    ┌─────────────────┐
                    │   Orchestrator  │
                    └────────┬────────┘
                             │
         ┌───────────────────┼───────────────────┐
         │                   │                   │
         ▼                   ▼                   ▼
    ┌─────────┐        ┌─────────┐        ┌─────────┐
    │ Agent 1 │───────▶│ Agent 2 │───────▶│ Agent 3 │
    └─────────┘        └─────────┘        └─────────┘
         │                   │                   │
         └───────────────────┼───────────────────┘
                             │
                             ▼
                      ┌──────────────┐
                      │ Copilot API  │
                      └──────────────┘
```

## Security Architecture

### Authentication

- JWT tokens for user authentication
- API keys for programmatic access
- OAuth2/OIDC for enterprise SSO

### Authorization

- Role-based access control (RBAC)
- Permission-based fine-grained control
- Tenant isolation for multi-tenancy

### Data Security

- TLS 1.3 in transit
- AES-256 encryption at rest
- Audit logging for compliance

## Scalability

### Horizontal Scaling

- API servers are stateless
- Load balancer distributes requests
- Redis for shared session state

### Vertical Scaling

- Worker pools for parallel parsing
- GPU support for local LLMs
- Connection pooling for databases

### Job Processing

- Kafka/Redis Streams for job queue
- At-least-once delivery
- Dead letter queue for failures

## Next Steps

- [Deep dive into Agents](/docs/architecture/agents)
- [Parser implementation details](/docs/architecture/parsers)
- [Database schema](/docs/architecture/database)
