/**
 * MigrationPilot Core Types
 * 
 * Central type definitions for the entire platform
 */

// ============================================================================
// PROJECT & MIGRATION TYPES
// ============================================================================

export type ProjectStatus = 
  | 'draft'
  | 'analyzing'
  | 'analysis_complete'
  | 'designing'
  | 'design_complete'
  | 'generating'
  | 'generation_complete'
  | 'validating'
  | 'validation_complete'
  | 'completed'
  | 'failed';

export type SourceLanguage = 'cobol' | 'fortran' | 'vb6' | 'vba' | 'java-legacy';
export type TargetLanguage = 'java' | 'python' | 'typescript' | 'go' | 'csharp';

export interface Project {
  id: string;
  name: string;
  description?: string;
  organizationId: string;
  sourceLanguage: SourceLanguage;
  targetLanguage: TargetLanguage;
  targetFramework?: string;
  status: ProjectStatus;
  settings: ProjectSettings;
  statistics: ProjectStatistics;
  createdAt: Date;
  updatedAt: Date;
  createdBy: string;
}

export interface ProjectSettings {
  enableStranglerFig: boolean;
  generateTests: boolean;
  generateDocumentation: boolean;
  humanReviewRequired: boolean;
  confidenceThreshold: number;
  targetFramework?: string;
  customPrompts?: Record<string, string>;
}

export interface ProjectStatistics {
  totalFiles: number;
  totalLines: number;
  analyzedFiles: number;
  analyzedLines: number;
  extractedRules: number;
  generatedFiles: number;
  generatedLines: number;
  testsGenerated: number;
  testsPassed: number;
  equivalenceScore: number;
}

// ============================================================================
// SOURCE CODE TYPES
// ============================================================================

export interface SourceFile {
  id: string;
  projectId: string;
  path: string;
  name: string;
  language: SourceLanguage;
  content: string;
  hash: string;
  lines: number;
  analysisStatus: 'pending' | 'analyzing' | 'complete' | 'failed';
  createdAt: Date;
  updatedAt: Date;
}

export interface SourceLocation {
  file: string;
  startLine: number;
  endLine: number;
  startColumn?: number;
  endColumn?: number;
}

// ============================================================================
// AST & SEMANTIC TYPES
// ============================================================================

export interface ASTNode {
  type: string;
  name?: string;
  location: SourceLocation;
  children: ASTNode[];
  metadata?: Record<string, unknown>;
}

export interface DataStructure {
  name: string;
  type: DataType;
  level?: number;
  picture?: string;
  usage?: string;
  value?: string;
  children?: DataStructure[];
  location: SourceLocation;
}

export type DataType = 
  | 'string'
  | 'integer'
  | 'decimal'
  | 'date'
  | 'boolean'
  | 'binary'
  | 'group'
  | 'array'
  | 'unknown';

export interface Procedure {
  name: string;
  type: 'program' | 'section' | 'paragraph' | 'function' | 'subroutine' | 'method';
  parameters: Parameter[];
  returnType?: DataType;
  localVariables: DataStructure[];
  calledProcedures: string[];
  location: SourceLocation;
  complexity: number;
}

export interface Parameter {
  name: string;
  type: DataType;
  direction: 'in' | 'out' | 'inout';
}

// ============================================================================
// BUSINESS RULE TYPES
// ============================================================================

export type BusinessRuleCategory = 
  | 'calculation'
  | 'validation'
  | 'decision'
  | 'transformation'
  | 'workflow'
  | 'constraint';

export type ReviewStatus = 'pending' | 'approved' | 'rejected' | 'needs_clarification';

export interface BusinessRule {
  id: string;
  projectId: string;
  name: string;
  description: string;
  category: BusinessRuleCategory;
  
  // Source information
  sourceFile: string;
  sourceLines: [number, number];
  sourceCode: string;
  
  // Extracted logic
  inputs: RuleInput[];
  outputs: RuleOutput[];
  logic: string;
  formula?: string;
  
  // Edge cases and notes
  edgeCases: string[];
  assumptions: string[];
  notes?: string;
  
  // Confidence and review
  confidence: number;
  reviewStatus: ReviewStatus;
  reviewedBy?: string;
  reviewedAt?: Date;
  reviewComments?: string;
  
  // Metadata
  extractedAt: Date;
  extractedBy: string;
  version: number;
}

export interface RuleInput {
  name: string;
  type: DataType;
  source: string;
  description?: string;
  constraints?: string[];
}

export interface RuleOutput {
  name: string;
  type: DataType;
  description?: string;
}

// ============================================================================
// GENERATED CODE TYPES
// ============================================================================

export interface GeneratedFile {
  id: string;
  projectId: string;
  path: string;
  name: string;
  language: TargetLanguage;
  content: string;
  type: 'source' | 'test' | 'config' | 'documentation';
  sourceMapping: SourceMapping[];
  createdAt: Date;
  updatedAt: Date;
}

export interface SourceMapping {
  generatedLines: [number, number];
  sourceFile: string;
  sourceLines: [number, number];
  ruleId?: string;
}

// ============================================================================
// EQUIVALENCE TESTING TYPES
// ============================================================================

export interface TestCase {
  id: string;
  projectId: string;
  ruleId?: string;
  name: string;
  description: string;
  inputs: Record<string, unknown>;
  expectedOutputs?: Record<string, unknown>;
  category: 'boundary' | 'partition' | 'historical' | 'property' | 'custom';
  priority: 'high' | 'medium' | 'low';
}

export interface TestResult {
  id: string;
  testCaseId: string;
  projectId: string;
  
  // Execution results
  legacyOutput: Record<string, unknown>;
  modernOutput: Record<string, unknown>;
  
  // Comparison
  equivalent: boolean;
  differences?: Difference[];
  
  // Timing
  legacyDurationMs: number;
  modernDurationMs: number;
  
  executedAt: Date;
}

export interface Difference {
  field: string;
  legacyValue: unknown;
  modernValue: unknown;
  tolerance?: number;
  significant: boolean;
}

export interface EquivalenceReport {
  projectId: string;
  totalTests: number;
  passed: number;
  failed: number;
  skipped: number;
  equivalenceScore: number;
  confidenceLevel: number;
  failedTests: TestResult[];
  generatedAt: Date;
}

// ============================================================================
// AGENT TYPES
// ============================================================================

export type AgentType = 'archeologist' | 'architect' | 'builder' | 'validator';

export interface AgentSession {
  id: string;
  projectId: string;
  agentType: AgentType;
  status: 'active' | 'completed' | 'failed';
  startedAt: Date;
  completedAt?: Date;
  messages: AgentMessage[];
  result?: unknown;
  error?: string;
}

export interface AgentMessage {
  role: 'user' | 'assistant' | 'system' | 'tool';
  content: string;
  toolCalls?: ToolCall[];
  timestamp: Date;
}

export interface ToolCall {
  id: string;
  name: string;
  arguments: Record<string, unknown>;
  result?: unknown;
}

export interface AgentTool {
  name: string;
  description: string;
  parameters: Record<string, ToolParameter>;
  execute: (args: Record<string, unknown>) => Promise<unknown>;
}

export interface ToolParameter {
  type: 'string' | 'number' | 'boolean' | 'object' | 'array';
  description: string;
  required?: boolean;
  enum?: string[];
}

// ============================================================================
// USER & ORGANIZATION TYPES
// ============================================================================

export type UserRole = 'admin' | 'architect' | 'developer' | 'viewer';

export interface User {
  id: string;
  email: string;
  name: string;
  avatarUrl?: string;
  organizationId: string;
  role: UserRole;
  createdAt: Date;
  updatedAt: Date;
}

export interface Organization {
  id: string;
  name: string;
  slug: string;
  settings: OrganizationSettings;
  createdAt: Date;
  updatedAt: Date;
}

export interface OrganizationSettings {
  allowedSourceLanguages: SourceLanguage[];
  allowedTargetLanguages: TargetLanguage[];
  defaultConfidenceThreshold: number;
  requireHumanReview: boolean;
  maxConcurrentProjects: number;
}

// ============================================================================
// API TYPES
// ============================================================================

export interface PaginatedResponse<T> {
  data: T[];
  total: number;
  page: number;
  pageSize: number;
  hasMore: boolean;
}

export interface ApiError {
  code: string;
  message: string;
  details?: Record<string, unknown>;
}

export interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: ApiError;
}

// ============================================================================
// AUDIT & SECURITY TYPES
// ============================================================================

export interface AuditLog {
  id: string;
  timestamp: string;
  action: string;
  userId: string;
  tenantId: string;
  resourceType: string;
  resourceId?: string;
  metadata: Record<string, unknown>;
}

export interface Tenant {
  id: string;
  name: string;
  slug: string;
  settings: TenantSettings;
  subscription: SubscriptionTier;
  createdAt: Date;
  updatedAt: Date;
}

export type SubscriptionTier = 'free' | 'starter' | 'professional' | 'enterprise';

export interface TenantSettings {
  maxProjects: number;
  maxUsersPerProject: number;
  maxLinesOfCode: number;
  allowedSourceLanguages: SourceLanguage[];
  allowedTargetLanguages: TargetLanguage[];
  dataRetentionDays: number;
  customBranding?: {
    logo?: string;
    primaryColor?: string;
    companyName?: string;
  };
}

export interface ApiKey {
  id: string;
  tenantId: string;
  name: string;
  keyPrefix: string;
  keyHash: string;
  permissions: string[];
  lastUsedAt?: Date;
  expiresAt?: Date;
  createdAt: Date;
  createdBy: string;
}

export interface SecurityEvent {
  id: string;
  type: 'login' | 'logout' | 'failed_login' | 'api_key_created' | 'permission_denied' | 'data_access';
  userId?: string;
  tenantId: string;
  ipAddress: string;
  userAgent: string;
  metadata: Record<string, unknown>;
  timestamp: Date;
}
