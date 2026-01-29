# @migrationpilot/database

Database schema and migrations for MigrationPilot using Drizzle ORM.

## Overview

This package defines the database schema, migrations, and data access layer for MigrationPilot. It uses Drizzle ORM with PostgreSQL for type-safe database operations.

## Schema

### Core Tables

| Table | Description |
|-------|-------------|
| `organizations` | Multi-tenant organization accounts |
| `users` | User accounts and profiles |
| `projects` | Migration projects |
| `source_files` | Uploaded legacy source files |
| `business_rules` | Extracted business rules |
| `rule_reviews` | SME review comments and approvals |
| `architectures` | Generated architecture designs |
| `generated_files` | Output code files |
| `test_cases` | Equivalence test cases |
| `test_results` | Test execution results |
| `audit_logs` | Compliance audit trail |

### Entity Relationships

```
organizations
    └── users
    └── projects
            └── source_files
            └── business_rules
                    └── rule_reviews
            └── architectures
            └── generated_files
            └── test_cases
                    └── test_results
            └── audit_logs
```

## Usage

### Basic Queries

```typescript
import { db } from '@migrationpilot/database';
import { projects, businessRules, users } from '@migrationpilot/database/schema';
import { eq, and, gt } from 'drizzle-orm';

// Find project by ID
const project = await db.query.projects.findFirst({
  where: eq(projects.id, projectId),
  with: {
    sourceFiles: true,
    businessRules: true,
  },
});

// Get high-confidence business rules
const rules = await db
  .select()
  .from(businessRules)
  .where(
    and(
      eq(businessRules.projectId, projectId),
      gt(businessRules.confidence, 0.9)
    )
  );
```

### Transactions

```typescript
import { db } from '@migrationpilot/database';

await db.transaction(async (tx) => {
  const project = await tx.insert(projects).values({
    name: 'Payroll Migration',
    sourceLanguage: 'cobol',
    targetLanguage: 'java',
  }).returning();

  await tx.insert(auditLogs).values({
    projectId: project[0].id,
    action: 'project.created',
    userId: currentUser.id,
  });
});
```

### Migrations

```bash
# Generate migration from schema changes
pnpm generate

# Run migrations
pnpm migrate

# Push schema directly (development only)
pnpm push

# Seed with sample data
pnpm seed
```

## Schema Definitions

### Projects Table

```typescript
export const projects = pgTable('projects', {
  id: uuid('id').primaryKey().defaultRandom(),
  organizationId: uuid('organization_id').references(() => organizations.id),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description'),
  sourceLanguage: varchar('source_language', { length: 50 }).notNull(),
  targetLanguage: varchar('target_language', { length: 50 }).notNull(),
  targetFramework: varchar('target_framework', { length: 100 }),
  status: varchar('status', { length: 50 }).default('created'),
  confidence: real('confidence').default(0),
  createdAt: timestamp('created_at').defaultNow(),
  updatedAt: timestamp('updated_at').defaultNow(),
});
```

### Business Rules Table

```typescript
export const businessRules = pgTable('business_rules', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id),
  name: varchar('name', { length: 255 }).notNull(),
  category: varchar('category', { length: 50 }),
  description: text('description'),
  logic: text('logic').notNull(),
  formula: text('formula'),
  inputs: jsonb('inputs').$type<Parameter[]>(),
  outputs: jsonb('outputs').$type<Parameter[]>(),
  edgeCases: jsonb('edge_cases').$type<string[]>(),
  confidence: real('confidence').notNull(),
  sourceFile: varchar('source_file', { length: 500 }),
  startLine: integer('start_line'),
  endLine: integer('end_line'),
  reviewStatus: varchar('review_status', { length: 50 }).default('pending'),
  createdAt: timestamp('created_at').defaultNow(),
});
```

## Configuration

```typescript
// drizzle.config.ts
import type { Config } from 'drizzle-kit';

export default {
  schema: './src/schema.ts',
  out: './drizzle',
  driver: 'pg',
  dbCredentials: {
    connectionString: process.env.DATABASE_URL!,
  },
} satisfies Config;
```

## Environment Variables

```env
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/migrationpilot
```

## Installation

```bash
pnpm add @migrationpilot/database
```

## Development

```bash
# Build
pnpm build

# Generate migrations
pnpm generate

# Run migrations
pnpm migrate

# Seed database
pnpm seed
```

## License

MIT
