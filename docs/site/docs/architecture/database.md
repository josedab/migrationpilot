# Database Architecture

MigrationPilot uses PostgreSQL with Drizzle ORM for type-safe database access. The schema supports multi-tenancy, audit logging, and comprehensive tracking of migration projects.

## Schema Overview

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   tenants       │     │  organizations  │     │     users       │
│                 │     │                 │     │                 │
│  id             │     │  id             │     │  id             │
│  name           │◄────│  tenant_id      │◄────│  organization_id│
│  subscription   │     │  name           │     │  email          │
│  settings       │     │  slug           │     │  role           │
└─────────────────┘     └─────────────────┘     └────────┬────────┘
                                                         │
                        ┌────────────────────────────────┘
                        │
┌───────────────────────▼─────────────────────────────────────────┐
│                         projects                                 │
│  id | name | source_language | target_language | status | ...   │
└───────────────────────┬─────────────────────────────────────────┘
                        │
        ┌───────────────┼───────────────┬───────────────┐
        │               │               │               │
┌───────▼───────┐ ┌─────▼─────┐ ┌───────▼───────┐ ┌─────▼─────┐
│ source_files  │ │business_  │ │generated_files│ │test_cases │
│               │ │rules      │ │               │ │           │
│ id            │ │           │ │ id            │ │ id        │
│ project_id    │ │ id        │ │ project_id    │ │ project_id│
│ path          │ │ project_id│ │ path          │ │ rule_id   │
│ content       │ │ name      │ │ content       │ │ inputs    │
│ language      │ │ confidence│ │ type          │ │ expected  │
└───────────────┘ │ status    │ └───────────────┘ └───────────┘
                  └───────────┘
```

## Core Tables

### Organizations & Users

```typescript
// Organizations table
export const organizations = pgTable('organizations', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  slug: varchar('slug', { length: 100 }).notNull().unique(),
  settings: jsonb('settings').default({}),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

// Users table
export const users = pgTable('users', {
  id: uuid('id').primaryKey().defaultRandom(),
  email: varchar('email', { length: 255 }).notNull().unique(),
  name: varchar('name', { length: 255 }).notNull(),
  organizationId: uuid('organization_id').references(() => organizations.id),
  role: varchar('role', { length: 50 }).notNull().default('developer'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});
```

### Projects

```typescript
export const projects = pgTable('projects', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description'),
  organizationId: uuid('organization_id').references(() => organizations.id),
  sourceLanguage: sourceLanguageEnum('source_language').notNull(),
  targetLanguage: targetLanguageEnum('target_language').notNull(),
  targetFramework: varchar('target_framework', { length: 100 }),
  status: projectStatusEnum('status').notNull().default('draft'),
  settings: jsonb('settings').default({}),
  statistics: jsonb('statistics').default({}),
  createdBy: uuid('created_by').references(() => users.id),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});
```

### Business Rules

```typescript
export const businessRules = pgTable('business_rules', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description').notNull(),
  category: ruleCategory('category').notNull(),
  
  // Source reference
  sourceFile: varchar('source_file', { length: 500 }).notNull(),
  sourceStartLine: integer('source_start_line').notNull(),
  sourceEndLine: integer('source_end_line').notNull(),
  sourceCode: text('source_code').notNull(),
  
  // Logic specification
  inputs: jsonb('inputs').default([]),
  outputs: jsonb('outputs').default([]),
  logic: text('logic').notNull(),
  formula: text('formula'),
  
  // Confidence and review
  confidence: decimal('confidence', { precision: 4, scale: 3 }).notNull(),
  reviewStatus: reviewStatusEnum('review_status').default('pending'),
  reviewedBy: uuid('reviewed_by').references(() => users.id),
  reviewComments: text('review_comments'),
  
  extractedAt: timestamp('extracted_at').defaultNow().notNull(),
  version: integer('version').notNull().default(1),
});
```

## Enums

```typescript
// Project status workflow
export const projectStatusEnum = pgEnum('project_status', [
  'draft',
  'analyzing',
  'analysis_complete',
  'designing',
  'design_complete',
  'generating',
  'generation_complete',
  'validating',
  'validation_complete',
  'completed',
  'failed',
]);

// Source languages
export const sourceLanguageEnum = pgEnum('source_language', [
  'cobol', 'fortran', 'vb6', 'vba', 'java-legacy',
]);

// Target languages
export const targetLanguageEnum = pgEnum('target_language', [
  'java', 'python', 'typescript', 'go', 'csharp',
]);

// Rule categories
export const ruleCategory = pgEnum('rule_category', [
  'calculation', 'validation', 'decision', 
  'transformation', 'workflow', 'constraint',
]);

// Review status
export const reviewStatusEnum = pgEnum('review_status', [
  'pending', 'approved', 'rejected', 'needs_clarification',
]);
```

## Audit & Versioning

### Audit Logs

All significant actions are logged:

```typescript
export const auditLogs = pgTable('audit_logs', {
  id: uuid('id').primaryKey().defaultRandom(),
  organizationId: uuid('organization_id').references(() => organizations.id),
  userId: uuid('user_id').references(() => users.id),
  action: varchar('action', { length: 100 }).notNull(),
  resourceType: varchar('resource_type', { length: 100 }).notNull(),
  resourceId: uuid('resource_id'),
  metadata: jsonb('metadata').default({}),
  ipAddress: varchar('ip_address', { length: 45 }),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});
```

### Rule Versions

Track all changes to business rules:

```typescript
export const ruleVersions = pgTable('rule_versions', {
  id: uuid('id').primaryKey().defaultRandom(),
  ruleId: uuid('rule_id').references(() => businessRules.id),
  version: integer('version').notNull(),
  snapshot: jsonb('snapshot').notNull(), // Full rule state
  changedBy: uuid('changed_by').references(() => users.id),
  changeReason: text('change_reason'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});
```

## Multi-Tenancy

### Tenant Configuration

```typescript
export const tenants = pgTable('tenants', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  slug: varchar('slug', { length: 100 }).notNull().unique(),
  subscription: varchar('subscription', { length: 50 }).default('free'),
  
  // Limits
  maxProjects: integer('max_projects').default(5),
  maxUsersPerProject: integer('max_users_per_project').default(10),
  maxLinesOfCode: integer('max_lines_of_code').default(100000),
  
  // Retention
  dataRetentionDays: integer('data_retention_days').default(90),
  
  createdAt: timestamp('created_at').defaultNow().notNull(),
});
```

### Data Isolation

Row-level security ensures tenant isolation:

```sql
-- Enable RLS
ALTER TABLE projects ENABLE ROW LEVEL SECURITY;

-- Policy: Users can only see their organization's projects
CREATE POLICY projects_isolation ON projects
  USING (organization_id IN (
    SELECT organization_id FROM users WHERE id = current_user_id()
  ));
```

## Strangler Fig Support

### Routing Rules

```typescript
export const routingRules = pgTable('routing_rules', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id),
  name: varchar('name', { length: 255 }).notNull(),
  pathPattern: varchar('path_pattern', { length: 500 }).notNull(),
  targetService: varchar('target_service', { length: 50 }).notNull(),
  trafficPercentage: integer('traffic_percentage').default(0),
  shadowMode: boolean('shadow_mode').default(false),
  enabled: boolean('enabled').default(false),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});
```

## Indexes

Key indexes for query performance:

```sql
-- Projects by organization
CREATE INDEX idx_projects_org ON projects(organization_id);

-- Business rules by project and status
CREATE INDEX idx_rules_project_status 
  ON business_rules(project_id, review_status);

-- Audit logs by date
CREATE INDEX idx_audit_created ON audit_logs(created_at DESC);

-- Source files by project
CREATE INDEX idx_source_files_project ON source_files(project_id);
```

## Migrations

Using Drizzle Kit for migrations:

```bash
# Generate migration
pnpm --filter @migrationpilot/database generate

# Run migrations
pnpm --filter @migrationpilot/database migrate

# Push schema (dev only)
pnpm --filter @migrationpilot/database db:push
```

## Querying with Drizzle

```typescript
import { db } from '@migrationpilot/database';
import { projects, businessRules } from '@migrationpilot/database/schema';
import { eq, and, gte } from 'drizzle-orm';

// Get project with rules
const projectWithRules = await db
  .select()
  .from(projects)
  .leftJoin(businessRules, eq(projects.id, businessRules.projectId))
  .where(eq(projects.id, projectId));

// Get pending rules with high confidence
const pendingRules = await db
  .select()
  .from(businessRules)
  .where(
    and(
      eq(businessRules.reviewStatus, 'pending'),
      gte(businessRules.confidence, 0.9)
    )
  );
```

## Related Topics

- [Deployment](../deployment/cloud.md) - Database deployment options
- [Security](../deployment/security.md) - Encryption and access control
- [API Reference](../api/overview.md) - Database-backed endpoints
