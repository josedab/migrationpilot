/**
 * Code Reviewer Types
 * Types for AI-powered code review against business rules
 */

import type { BusinessRule, SourceLocation } from '@migrationpilot/core';

// ============================================================================
// PR/CHANGE TYPES
// ============================================================================

export interface PullRequest {
  id: string;
  number: number;
  title: string;
  description: string;
  author: string;
  baseBranch: string;
  headBranch: string;
  repository: string;
  files: ChangedFile[];
  createdAt: string;
  updatedAt: string;
}

export interface ChangedFile {
  path: string;
  status: 'added' | 'modified' | 'deleted' | 'renamed';
  additions: number;
  deletions: number;
  patch?: string;
  hunks: DiffHunk[];
}

export interface DiffHunk {
  oldStart: number;
  oldLines: number;
  newStart: number;
  newLines: number;
  content: string;
  changes: DiffChange[];
}

export interface DiffChange {
  type: 'add' | 'delete' | 'context';
  lineNumber: number;
  content: string;
}

// ============================================================================
// REVIEW TYPES
// ============================================================================

export interface ReviewRequest {
  pr: PullRequest;
  projectId: string;
  options?: ReviewOptions;
}

export interface ReviewOptions {
  strictMode?: boolean;
  checkBusinessRules?: boolean;
  checkCodeQuality?: boolean;
  checkNamingConventions?: boolean;
  checkDataIntegrity?: boolean;
  minConfidenceThreshold?: number;
  maxComments?: number;
}

export interface ReviewResult {
  id: string;
  prId: string;
  projectId: string;
  timestamp: string;

  // Summary
  summary: ReviewSummary;

  // Detailed findings
  findings: ReviewFinding[];

  // Affected business rules
  affectedRules: AffectedRule[];

  // Recommendations
  recommendations: string[];

  // Metadata
  processingTimeMs: number;
  rulesChecked: number;
  filesReviewed: number;
}

export interface ReviewSummary {
  status: ReviewStatus;
  riskLevel: 'low' | 'medium' | 'high' | 'critical';
  totalFindings: number;
  criticalFindings: number;
  warningFindings: number;
  infoFindings: number;
  businessRuleViolations: number;
  message: string;
}

export type ReviewStatus = 'approved' | 'changes_requested' | 'needs_discussion';

export interface ReviewFinding {
  id: string;
  type: FindingType;
  severity: FindingSeverity;
  title: string;
  description: string;

  // Location
  file: string;
  startLine: number;
  endLine: number;

  // Evidence
  codeSnippet?: string;
  suggestedFix?: string;

  // Related rules
  relatedRuleIds: string[];

  // Metadata
  confidence: number;
  category: string;
}

export type FindingType =
  | 'business_rule_violation'
  | 'business_rule_change'
  | 'data_integrity'
  | 'calculation_change'
  | 'validation_change'
  | 'naming_convention'
  | 'code_quality'
  | 'potential_bug'
  | 'security'
  | 'performance';

export type FindingSeverity = 'critical' | 'warning' | 'info' | 'suggestion';

export interface AffectedRule {
  rule: BusinessRule;
  changeType: 'modified' | 'potentially_broken' | 'removed' | 'new_behavior';
  affectedFiles: string[];
  confidence: number;
  explanation: string;
}

// ============================================================================
// COMMENT TYPES (for PR comments)
// ============================================================================

export interface ReviewComment {
  id: string;
  path: string;
  line: number;
  body: string;
  type: 'file' | 'line' | 'general';
  severity: FindingSeverity;
}

export interface ReviewOutput {
  status: ReviewStatus;
  body: string;
  comments: ReviewComment[];
}

// ============================================================================
// RULE MATCHING TYPES
// ============================================================================

export interface RuleMatch {
  rule: BusinessRule;
  matchType: 'direct' | 'indirect' | 'potential';
  matchedCode: string;
  location: SourceLocation;
  confidence: number;
  reason: string;
}

export interface CodePattern {
  pattern: RegExp;
  ruleId: string;
  description: string;
  weight: number;
}

// ============================================================================
// CONFIGURATION
// ============================================================================

export interface ReviewerConfig {
  // Thresholds
  minConfidenceToReport: number;
  maxFindingsPerFile: number;
  maxTotalFindings: number;

  // Severity mappings
  businessRuleViolationSeverity: FindingSeverity;
  calculationChangeSeverity: FindingSeverity;
  namingConventionSeverity: FindingSeverity;

  // Comment formatting
  includeSuggestedFixes: boolean;
  includeRuleReferences: boolean;
  includeConfidenceScores: boolean;

  // Filtering
  ignoredPaths: string[];
  ignoredRuleCategories: string[];
}

export const DEFAULT_REVIEWER_CONFIG: ReviewerConfig = {
  minConfidenceToReport: 0.6,
  maxFindingsPerFile: 10,
  maxTotalFindings: 50,
  businessRuleViolationSeverity: 'critical',
  calculationChangeSeverity: 'warning',
  namingConventionSeverity: 'info',
  includeSuggestedFixes: true,
  includeRuleReferences: true,
  includeConfidenceScores: true,
  ignoredPaths: ['test/', 'tests/', '__tests__/', '*.test.*', '*.spec.*'],
  ignoredRuleCategories: [],
};

// ============================================================================
// SERVICE INTERFACE
// ============================================================================

export interface ICodeReviewer {
  review(request: ReviewRequest): Promise<ReviewResult>;
  formatForGitHub(result: ReviewResult): ReviewOutput;
  formatForGitLab(result: ReviewResult): ReviewOutput;
  findAffectedRules(pr: PullRequest, rules: BusinessRule[]): AffectedRule[];
}
