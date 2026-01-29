/**
 * Database Schema
 * 
 * Drizzle ORM schema definitions for MigrationPilot
 */

import { pgTable, uuid, varchar, text, timestamp, integer, decimal, boolean, jsonb, pgEnum } from 'drizzle-orm/pg-core';

// Enums
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

export const sourceLanguageEnum = pgEnum('source_language', [
  'cobol',
  'fortran',
  'vb6',
  'vba',
  'java-legacy',
]);

export const targetLanguageEnum = pgEnum('target_language', [
  'java',
  'python',
  'typescript',
  'go',
  'csharp',
]);

export const reviewStatusEnum = pgEnum('review_status', [
  'pending',
  'approved',
  'rejected',
  'needs_clarification',
]);

export const ruleCategory = pgEnum('rule_category', [
  'calculation',
  'validation',
  'decision',
  'transformation',
  'workflow',
  'constraint',
]);

// Tables
export const organizations = pgTable('organizations', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  slug: varchar('slug', { length: 100 }).notNull().unique(),
  settings: jsonb('settings').default({}),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

export const users = pgTable('users', {
  id: uuid('id').primaryKey().defaultRandom(),
  email: varchar('email', { length: 255 }).notNull().unique(),
  name: varchar('name', { length: 255 }).notNull(),
  avatarUrl: varchar('avatar_url', { length: 500 }),
  organizationId: uuid('organization_id').references(() => organizations.id).notNull(),
  role: varchar('role', { length: 50 }).notNull().default('developer'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

export const projects = pgTable('projects', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description'),
  organizationId: uuid('organization_id').references(() => organizations.id).notNull(),
  sourceLanguage: sourceLanguageEnum('source_language').notNull(),
  targetLanguage: targetLanguageEnum('target_language').notNull(),
  targetFramework: varchar('target_framework', { length: 100 }),
  status: projectStatusEnum('status').notNull().default('draft'),
  settings: jsonb('settings').default({}),
  statistics: jsonb('statistics').default({}),
  createdBy: uuid('created_by').references(() => users.id).notNull(),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

export const sourceFiles = pgTable('source_files', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  path: varchar('path', { length: 500 }).notNull(),
  name: varchar('name', { length: 255 }).notNull(),
  language: sourceLanguageEnum('language').notNull(),
  content: text('content').notNull(),
  hash: varchar('hash', { length: 64 }).notNull(),
  lines: integer('lines').notNull(),
  analysisStatus: varchar('analysis_status', { length: 50 }).notNull().default('pending'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

export const businessRules = pgTable('business_rules', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description').notNull(),
  category: ruleCategory('category').notNull(),
  sourceFile: varchar('source_file', { length: 500 }).notNull(),
  sourceStartLine: integer('source_start_line').notNull(),
  sourceEndLine: integer('source_end_line').notNull(),
  sourceCode: text('source_code').notNull(),
  inputs: jsonb('inputs').default([]),
  outputs: jsonb('outputs').default([]),
  logic: text('logic').notNull(),
  formula: text('formula'),
  edgeCases: jsonb('edge_cases').default([]),
  assumptions: jsonb('assumptions').default([]),
  notes: text('notes'),
  confidence: decimal('confidence', { precision: 4, scale: 3 }).notNull(),
  reviewStatus: reviewStatusEnum('review_status').notNull().default('pending'),
  reviewedBy: uuid('reviewed_by').references(() => users.id),
  reviewedAt: timestamp('reviewed_at'),
  reviewComments: text('review_comments'),
  extractedAt: timestamp('extracted_at').defaultNow().notNull(),
  extractedBy: varchar('extracted_by', { length: 100 }).notNull(),
  version: integer('version').notNull().default(1),
});

export const generatedFiles = pgTable('generated_files', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  path: varchar('path', { length: 500 }).notNull(),
  name: varchar('name', { length: 255 }).notNull(),
  language: targetLanguageEnum('language').notNull(),
  content: text('content').notNull(),
  type: varchar('type', { length: 50 }).notNull(),
  sourceMapping: jsonb('source_mapping').default([]),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

export const testCases = pgTable('test_cases', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  ruleId: uuid('rule_id').references(() => businessRules.id),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description').notNull(),
  inputs: jsonb('inputs').notNull(),
  expectedOutputs: jsonb('expected_outputs'),
  category: varchar('category', { length: 50 }).notNull(),
  priority: varchar('priority', { length: 20 }).notNull().default('medium'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});

export const testResults = pgTable('test_results', {
  id: uuid('id').primaryKey().defaultRandom(),
  testCaseId: uuid('test_case_id').references(() => testCases.id).notNull(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  legacyOutput: jsonb('legacy_output').notNull(),
  modernOutput: jsonb('modern_output').notNull(),
  equivalent: boolean('equivalent').notNull(),
  differences: jsonb('differences'),
  legacyDurationMs: integer('legacy_duration_ms').notNull(),
  modernDurationMs: integer('modern_duration_ms').notNull(),
  executedAt: timestamp('executed_at').defaultNow().notNull(),
});

export const agentSessions = pgTable('agent_sessions', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  agentType: varchar('agent_type', { length: 50 }).notNull(),
  status: varchar('status', { length: 50 }).notNull().default('active'),
  messages: jsonb('messages').default([]),
  result: jsonb('result'),
  error: text('error'),
  startedAt: timestamp('started_at').defaultNow().notNull(),
  completedAt: timestamp('completed_at'),
});

// Version history for business rules (audit trail)
export const ruleVersions = pgTable('rule_versions', {
  id: uuid('id').primaryKey().defaultRandom(),
  ruleId: uuid('rule_id').references(() => businessRules.id).notNull(),
  version: integer('version').notNull(),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description').notNull(),
  category: ruleCategory('category').notNull(),
  logic: text('logic').notNull(),
  formula: text('formula'),
  confidence: decimal('confidence', { precision: 4, scale: 3 }).notNull(),
  reviewStatus: reviewStatusEnum('review_status').notNull(),
  changedBy: uuid('changed_by').references(() => users.id),
  changeReason: text('change_reason'),
  snapshot: jsonb('snapshot').notNull(), // Full rule state at this version
  createdAt: timestamp('created_at').defaultNow().notNull(),
});

// Comments on rules (SME collaboration)
export const ruleComments = pgTable('rule_comments', {
  id: uuid('id').primaryKey().defaultRandom(),
  ruleId: uuid('rule_id').references(() => businessRules.id).notNull(),
  authorId: uuid('author_id').references(() => users.id).notNull(),
  content: text('content').notNull(),
  type: varchar('type', { length: 50 }).notNull().default('comment'), // comment, suggestion, correction
  resolved: boolean('resolved').notNull().default(false),
  resolvedBy: uuid('resolved_by').references(() => users.id),
  resolvedAt: timestamp('resolved_at'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

// Knowledge annotations (domain knowledge capture)
export const knowledgeAnnotations = pgTable('knowledge_annotations', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  ruleId: uuid('rule_id').references(() => businessRules.id),
  sourceFileId: uuid('source_file_id').references(() => sourceFiles.id),
  title: varchar('title', { length: 255 }).notNull(),
  content: text('content').notNull(),
  tags: jsonb('tags').default([]),
  authorId: uuid('author_id').references(() => users.id).notNull(),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

// Webhooks
export const webhooks = pgTable('webhooks', {
  id: uuid('id').primaryKey().defaultRandom(),
  organizationId: uuid('organization_id').references(() => organizations.id).notNull(),
  url: varchar('url', { length: 500 }).notNull(),
  secret: varchar('secret', { length: 100 }).notNull(),
  events: jsonb('events').notNull(),
  active: boolean('active').notNull().default(true),
  failureCount: integer('failure_count').notNull().default(0),
  lastTriggeredAt: timestamp('last_triggered_at'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

// Webhook delivery logs
export const webhookDeliveries = pgTable('webhook_deliveries', {
  id: uuid('id').primaryKey().defaultRandom(),
  webhookId: uuid('webhook_id').references(() => webhooks.id).notNull(),
  event: varchar('event', { length: 100 }).notNull(),
  payload: jsonb('payload').notNull(),
  statusCode: integer('status_code'),
  responseBody: text('response_body'),
  success: boolean('success').notNull(),
  error: text('error'),
  duration: integer('duration'),
  deliveredAt: timestamp('delivered_at').defaultNow().notNull(),
});

// Audit logs
export const auditLogs = pgTable('audit_logs', {
  id: uuid('id').primaryKey().defaultRandom(),
  organizationId: uuid('organization_id').references(() => organizations.id),
  userId: uuid('user_id').references(() => users.id),
  action: varchar('action', { length: 100 }).notNull(),
  resourceType: varchar('resource_type', { length: 100 }).notNull(),
  resourceId: uuid('resource_id'),
  metadata: jsonb('metadata').default({}),
  ipAddress: varchar('ip_address', { length: 45 }),
  userAgent: text('user_agent'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});

// API keys
export const apiKeys = pgTable('api_keys', {
  id: uuid('id').primaryKey().defaultRandom(),
  organizationId: uuid('organization_id').references(() => organizations.id).notNull(),
  name: varchar('name', { length: 100 }).notNull(),
  keyPrefix: varchar('key_prefix', { length: 10 }).notNull(),
  keyHash: varchar('key_hash', { length: 64 }).notNull(),
  permissions: jsonb('permissions').notNull(),
  lastUsedAt: timestamp('last_used_at'),
  expiresAt: timestamp('expires_at'),
  createdBy: uuid('created_by').references(() => users.id).notNull(),
  createdAt: timestamp('created_at').defaultNow().notNull(),
});

// Tenants (for multi-tenancy)
export const tenants = pgTable('tenants', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  slug: varchar('slug', { length: 100 }).notNull().unique(),
  subscription: varchar('subscription', { length: 50 }).notNull().default('free'),
  settings: jsonb('settings').default({}),
  customBranding: jsonb('custom_branding'),
  maxProjects: integer('max_projects').notNull().default(5),
  maxUsersPerProject: integer('max_users_per_project').notNull().default(10),
  maxLinesOfCode: integer('max_lines_of_code').notNull().default(100000),
  dataRetentionDays: integer('data_retention_days').notNull().default(90),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

// Feature flags (for gradual rollout)
export const featureFlags = pgTable('feature_flags', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 100 }).notNull().unique(),
  description: text('description'),
  enabled: boolean('enabled').notNull().default(false),
  rolloutPercentage: integer('rollout_percentage').notNull().default(0),
  allowedTenants: jsonb('allowed_tenants').default([]),
  allowedUsers: jsonb('allowed_users').default([]),
  metadata: jsonb('metadata').default({}),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});

// Strangler fig routing rules
export const routingRules = pgTable('routing_rules', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').references(() => projects.id).notNull(),
  name: varchar('name', { length: 255 }).notNull(),
  description: text('description'),
  pathPattern: varchar('path_pattern', { length: 500 }).notNull(),
  targetService: varchar('target_service', { length: 50 }).notNull(), // 'legacy' or 'modern'
  trafficPercentage: integer('traffic_percentage').notNull().default(0),
  shadowMode: boolean('shadow_mode').notNull().default(false),
  enabled: boolean('enabled').notNull().default(false),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
});
