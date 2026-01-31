/**
 * GraphQL Schema
 * 
 * Type definitions for MigrationPilot GraphQL API
 */

export const typeDefs = /* GraphQL */ `
  scalar DateTime
  scalar JSON

  type Query {
    # Projects
    projects(status: ProjectStatus, limit: Int, offset: Int): ProjectConnection!
    project(id: ID!): Project
    
    # Business Rules
    businessRules(projectId: ID!, status: ReviewStatus, limit: Int, offset: Int): BusinessRuleConnection!
    businessRule(id: ID!): BusinessRule
    
    # Source Files
    sourceFiles(projectId: ID!, limit: Int, offset: Int): SourceFileConnection!
    sourceFile(id: ID!): SourceFile
    
    # Test Results
    testResults(projectId: ID!, status: TestStatus, limit: Int, offset: Int): TestResultConnection!
    testResult(id: ID!): TestResult
    
    # Equivalence Report
    equivalenceReport(projectId: ID!): EquivalenceReport
  }

  type Mutation {
    # Projects
    createProject(input: CreateProjectInput!): Project!
    updateProject(id: ID!, input: UpdateProjectInput!): Project!
    deleteProject(id: ID!): Boolean!
    
    # Start migration pipeline
    startMigration(projectId: ID!): MigrationJob!
    cancelMigration(jobId: ID!): Boolean!
    
    # Business Rules
    approveRule(id: ID!, comment: String): BusinessRule!
    rejectRule(id: ID!, reason: String!): BusinessRule!
    addRuleComment(ruleId: ID!, input: AddCommentInput!): Comment!
    
    # Webhooks
    createWebhook(input: CreateWebhookInput!): Webhook!
    deleteWebhook(id: ID!): Boolean!
    testWebhook(id: ID!): WebhookTestResult!
  }

  type Subscription {
    # Real-time migration progress
    migrationProgress(jobId: ID!): MigrationProgress!
    
    # Rule updates
    ruleUpdated(projectId: ID!): BusinessRule!
    
    # Test execution
    testExecuted(projectId: ID!): TestResult!
  }

  # Enums
  enum ProjectStatus {
    DRAFT
    ANALYZING
    ANALYSIS_COMPLETE
    DESIGNING
    DESIGN_COMPLETE
    GENERATING
    GENERATION_COMPLETE
    VALIDATING
    VALIDATION_COMPLETE
    COMPLETED
    FAILED
  }

  enum SourceLanguage {
    COBOL
    FORTRAN
    VB6
    VBA
    JAVA_LEGACY
  }

  enum TargetLanguage {
    JAVA
    PYTHON
    TYPESCRIPT
    GO
    CSHARP
  }

  enum ReviewStatus {
    PENDING
    APPROVED
    REJECTED
    NEEDS_CLARIFICATION
  }

  enum TestStatus {
    PASSED
    FAILED
    SKIPPED
  }

  enum RuleCategory {
    CALCULATION
    VALIDATION
    DECISION
    TRANSFORMATION
    WORKFLOW
    CONSTRAINT
  }

  # Types
  type Project {
    id: ID!
    name: String!
    description: String
    sourceLanguage: SourceLanguage!
    targetLanguage: TargetLanguage!
    status: ProjectStatus!
    statistics: ProjectStatistics!
    settings: ProjectSettings!
    sourceFiles: [SourceFile!]!
    businessRules: [BusinessRule!]!
    createdAt: DateTime!
    updatedAt: DateTime!
    createdBy: String!
  }

  type ProjectStatistics {
    totalFiles: Int!
    totalLines: Int!
    analyzedFiles: Int!
    extractedRules: Int!
    generatedFiles: Int!
    testsGenerated: Int!
    testsPassed: Int!
    equivalenceScore: Float!
  }

  type ProjectSettings {
    enableStranglerFig: Boolean!
    generateTests: Boolean!
    generateDocumentation: Boolean!
    humanReviewRequired: Boolean!
    confidenceThreshold: Float!
  }

  type SourceFile {
    id: ID!
    projectId: ID!
    path: String!
    name: String!
    language: SourceLanguage!
    lines: Int!
    analysisStatus: String!
    procedures: [Procedure!]!
    dataStructures: [DataStructure!]!
    createdAt: DateTime!
  }

  type Procedure {
    name: String!
    type: String!
    complexity: Int!
    startLine: Int!
    endLine: Int!
    calledProcedures: [String!]!
  }

  type DataStructure {
    name: String!
    type: String!
    level: Int
    children: [DataStructure!]
  }

  type BusinessRule {
    id: ID!
    projectId: ID!
    name: String!
    description: String!
    category: RuleCategory!
    confidence: Float!
    reviewStatus: ReviewStatus!
    sourceFile: String!
    sourceLines: [Int!]!
    sourceCode: String!
    formula: String
    inputs: [RuleInput!]!
    outputs: [RuleOutput!]!
    edgeCases: [String!]!
    comments: [Comment!]!
    reviewedBy: String
    reviewedAt: DateTime
    extractedAt: DateTime!
  }

  type RuleInput {
    name: String!
    type: String!
    description: String
  }

  type RuleOutput {
    name: String!
    type: String!
    description: String
  }

  type Comment {
    id: ID!
    author: String!
    content: String!
    type: String!
    createdAt: DateTime!
  }

  type TestResult {
    id: ID!
    projectId: ID!
    testCaseId: ID!
    name: String!
    category: String!
    status: TestStatus!
    legacyOutput: JSON
    modernOutput: JSON
    differences: [Difference!]
    duration: Int!
    executedAt: DateTime!
  }

  type Difference {
    field: String!
    legacyValue: JSON
    modernValue: JSON
    significant: Boolean!
  }

  type EquivalenceReport {
    projectId: ID!
    totalTests: Int!
    passed: Int!
    failed: Int!
    skipped: Int!
    equivalenceScore: Float!
    confidenceLevel: Float!
    failedTests: [TestResult!]!
    generatedAt: DateTime!
  }

  type MigrationJob {
    id: ID!
    projectId: ID!
    status: String!
    currentPhase: String!
    progress: Int!
    startedAt: DateTime!
    completedAt: DateTime
    error: String
  }

  type MigrationProgress {
    jobId: ID!
    phase: String!
    progress: Int!
    message: String
    data: JSON
  }

  type Webhook {
    id: ID!
    url: String!
    events: [String!]!
    active: Boolean!
    createdAt: DateTime!
  }

  type WebhookTestResult {
    success: Boolean!
    statusCode: Int
    error: String
  }

  # Connections (pagination)
  type ProjectConnection {
    nodes: [Project!]!
    totalCount: Int!
    hasMore: Boolean!
  }

  type BusinessRuleConnection {
    nodes: [BusinessRule!]!
    totalCount: Int!
    hasMore: Boolean!
  }

  type SourceFileConnection {
    nodes: [SourceFile!]!
    totalCount: Int!
    hasMore: Boolean!
  }

  type TestResultConnection {
    nodes: [TestResult!]!
    totalCount: Int!
    hasMore: Boolean!
  }

  # Inputs
  input CreateProjectInput {
    name: String!
    description: String
    sourceLanguage: SourceLanguage!
    targetLanguage: TargetLanguage!
    settings: ProjectSettingsInput
  }

  input UpdateProjectInput {
    name: String
    description: String
    settings: ProjectSettingsInput
  }

  input ProjectSettingsInput {
    enableStranglerFig: Boolean
    generateTests: Boolean
    generateDocumentation: Boolean
    humanReviewRequired: Boolean
    confidenceThreshold: Float
  }

  input AddCommentInput {
    content: String!
    type: String!
  }

  input CreateWebhookInput {
    url: String!
    events: [String!]!
  }
`;
