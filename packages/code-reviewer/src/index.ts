/**
 * Code Reviewer Package
 * AI-powered code review that validates changes against business rules
 */

// Main exports
export { CodeReviewer } from './code-reviewer.js';
export { RuleMatcher } from './rule-matcher.js';

// Type exports
export type {
  // PR/Change types
  PullRequest,
  ChangedFile,
  DiffHunk,
  DiffChange,
  // Review types
  ReviewRequest,
  ReviewOptions,
  ReviewResult,
  ReviewSummary,
  ReviewStatus,
  ReviewFinding,
  FindingType,
  FindingSeverity,
  AffectedRule,
  // Comment types
  ReviewComment,
  ReviewOutput,
  // Rule matching types
  RuleMatch,
  CodePattern,
  // Configuration
  ReviewerConfig,
  ICodeReviewer,
} from './types.js';

export { DEFAULT_REVIEWER_CONFIG } from './types.js';
