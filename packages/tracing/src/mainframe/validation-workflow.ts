/**
 * SME Validation Workflow
 * 
 * Enables Subject Matter Experts to review, confirm, correct, and annotate
 * inferred business rules with trace evidence presentation.
 */

import { generateId } from '@migrationpilot/core';
import type { BusinessRule, ReviewStatus } from '@migrationpilot/core';
import type { InferredBusinessRule, RuleEvidence, SuggestedTestCase } from './inference.js';

// ============================================================================
// VALIDATION WORKFLOW TYPES
// ============================================================================

export interface SMEValidationSession {
  id: string;
  projectId: string;
  smeUserId: string;
  smeName: string;
  
  // Rules to validate
  rules: RuleValidationItem[];
  
  // Progress
  totalRules: number;
  reviewedRules: number;
  approvedRules: number;
  rejectedRules: number;
  needsClarificationRules: number;
  
  // Timing
  startedAt: Date;
  lastActivityAt: Date;
  completedAt?: Date;
  
  // Session settings
  settings: ValidationSessionSettings;
}

export interface ValidationSessionSettings {
  showTraceEvidence: boolean;
  showConfidenceScores: boolean;
  requireComments: boolean;
  allowRuleEditing: boolean;
  suggestTestCases: boolean;
  groupByCategory: boolean;
}

export interface RuleValidationItem {
  id: string;
  rule: InferredBusinessRule;
  
  // Review state
  status: 'pending' | 'in_progress' | 'reviewed';
  reviewResult?: SMEReviewResult;
  
  // Evidence presentation
  evidenceViews: EvidenceView[];
  
  // Suggested actions
  suggestedActions: SuggestedAction[];
}

export interface SMEReviewResult {
  decision: 'approved' | 'rejected' | 'needs_clarification' | 'modified';
  
  // Comments and feedback
  comments?: string;
  clarificationQuestions?: string[];
  
  // Rule modifications (if decision is 'modified')
  modifications?: RuleModification;
  
  // Additional test cases suggested by SME
  additionalTestCases?: SuggestedTestCase[];
  
  // Metadata
  reviewedAt: Date;
  reviewedBy: string;
  reviewDurationMs: number;
}

export interface RuleModification {
  newName?: string;
  newDescription?: string;
  newLogic?: string;
  newFormula?: string;
  newEdgeCases?: string[];
  newAssumptions?: string[];
  confidenceAdjustment?: number;
}

export interface EvidenceView {
  type: 'trace' | 'input_output' | 'execution_path' | 'statistics';
  title: string;
  data: unknown;
  highlighted?: boolean;
}

export interface SuggestedAction {
  type: 'approve' | 'reject' | 'edit' | 'merge' | 'split' | 'investigate';
  label: string;
  description: string;
  priority: 'high' | 'medium' | 'low';
  automated?: boolean;
}

// ============================================================================
// SME VALIDATION WORKFLOW MANAGER
// ============================================================================

export class SMEValidationWorkflow {
  private sessions = new Map<string, SMEValidationSession>();
  private rules = new Map<string, InferredBusinessRule>();
  private reviewHistory: ReviewHistoryEntry[] = [];

  constructor(private projectId: string) {}

  /**
   * Create a new validation session for an SME
   */
  createSession(
    smeUserId: string,
    smeName: string,
    rules: InferredBusinessRule[],
    settings?: Partial<ValidationSessionSettings>
  ): SMEValidationSession {
    const sessionId = generateId();
    
    const validationItems: RuleValidationItem[] = rules.map(rule => ({
      id: generateId(),
      rule,
      status: 'pending',
      evidenceViews: this.generateEvidenceViews(rule),
      suggestedActions: this.generateSuggestedActions(rule),
    }));

    const session: SMEValidationSession = {
      id: sessionId,
      projectId: this.projectId,
      smeUserId,
      smeName,
      rules: validationItems,
      totalRules: rules.length,
      reviewedRules: 0,
      approvedRules: 0,
      rejectedRules: 0,
      needsClarificationRules: 0,
      startedAt: new Date(),
      lastActivityAt: new Date(),
      settings: {
        showTraceEvidence: true,
        showConfidenceScores: true,
        requireComments: false,
        allowRuleEditing: true,
        suggestTestCases: true,
        groupByCategory: true,
        ...settings,
      },
    };

    this.sessions.set(sessionId, session);

    // Store rules for reference
    for (const rule of rules) {
      this.rules.set(rule.id, rule);
    }

    return session;
  }

  /**
   * Get session by ID
   */
  getSession(sessionId: string): SMEValidationSession | undefined {
    return this.sessions.get(sessionId);
  }

  /**
   * Get next rule to review in session
   */
  getNextRuleForReview(sessionId: string): RuleValidationItem | undefined {
    const session = this.sessions.get(sessionId);
    if (!session) return undefined;

    // Prioritize rules needing clarification that were previously reviewed
    const needsClarification = session.rules.find(r => 
      r.status === 'reviewed' && 
      r.reviewResult?.decision === 'needs_clarification'
    );
    if (needsClarification) return needsClarification;

    // Then get next pending rule
    const pending = session.rules.find(r => r.status === 'pending');
    if (pending) {
      pending.status = 'in_progress';
      return pending;
    }

    return undefined;
  }

  /**
   * Submit review for a rule
   */
  submitReview(
    sessionId: string,
    ruleId: string,
    result: Omit<SMEReviewResult, 'reviewedAt' | 'reviewDurationMs'>
  ): { success: boolean; updatedRule?: BusinessRule; error?: string } {
    const session = this.sessions.get(sessionId);
    if (!session) {
      return { success: false, error: 'Session not found' };
    }

    const item = session.rules.find(r => r.id === ruleId || r.rule.id === ruleId);
    if (!item) {
      return { success: false, error: 'Rule not found in session' };
    }

    // Record review timing
    const reviewStartTime = item.status === 'in_progress' 
      ? session.lastActivityAt.getTime()
      : Date.now();

    const fullResult: SMEReviewResult = {
      ...result,
      reviewedAt: new Date(),
      reviewedBy: session.smeUserId,
      reviewDurationMs: Date.now() - reviewStartTime,
    };

    // Update item
    item.status = 'reviewed';
    item.reviewResult = fullResult;

    // Update session counters
    session.reviewedRules++;
    switch (result.decision) {
      case 'approved':
        session.approvedRules++;
        break;
      case 'rejected':
        session.rejectedRules++;
        break;
      case 'needs_clarification':
        session.needsClarificationRules++;
        break;
    }
    session.lastActivityAt = new Date();

    // Apply modifications if any
    let updatedRule: BusinessRule | undefined;
    if (result.decision === 'modified' || result.decision === 'approved') {
      updatedRule = this.applyReviewToRule(item.rule, fullResult);
    }

    // Record in history
    this.reviewHistory.push({
      sessionId,
      ruleId: item.rule.id,
      result: fullResult,
      timestamp: new Date(),
    });

    // Check if session is complete
    if (session.reviewedRules === session.totalRules) {
      session.completedAt = new Date();
    }

    return { success: true, updatedRule };
  }

  /**
   * Get rules grouped by SME decision
   */
  getReviewedRulesByDecision(sessionId: string): GroupedReviewResults {
    const session = this.sessions.get(sessionId);
    if (!session) {
      return { approved: [], rejected: [], needsClarification: [], modified: [] };
    }

    const reviewed = session.rules.filter(r => r.status === 'reviewed');

    return {
      approved: reviewed.filter(r => r.reviewResult?.decision === 'approved').map(r => r.rule),
      rejected: reviewed.filter(r => r.reviewResult?.decision === 'rejected').map(r => r.rule),
      needsClarification: reviewed.filter(r => r.reviewResult?.decision === 'needs_clarification').map(r => r.rule),
      modified: reviewed.filter(r => r.reviewResult?.decision === 'modified').map(r => r.rule),
    };
  }

  /**
   * Export validated rules
   */
  exportValidatedRules(sessionId: string): ExportedValidationResult {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error('Session not found');
    }

    const groupedResults = this.getReviewedRulesByDecision(sessionId);

    return {
      sessionId,
      projectId: this.projectId,
      exportedAt: new Date(),
      summary: {
        totalRules: session.totalRules,
        approved: session.approvedRules,
        rejected: session.rejectedRules,
        needsClarification: session.needsClarificationRules,
        completionRate: session.reviewedRules / session.totalRules,
      },
      approvedRules: groupedResults.approved.map(r => this.convertToBusinessRule(r, 'approved')),
      rejectedRules: groupedResults.rejected.map(r => ({
        rule: this.convertToBusinessRule(r, 'rejected'),
        rejectionReason: session.rules.find(i => i.rule.id === r.id)?.reviewResult?.comments || '',
      })),
      rulesNeedingClarification: groupedResults.needsClarification.map(r => ({
        rule: this.convertToBusinessRule(r, 'needs_clarification'),
        questions: session.rules.find(i => i.rule.id === r.id)?.reviewResult?.clarificationQuestions || [],
      })),
      suggestedTestCases: this.collectAllTestCases(session),
    };
  }

  // ============================================================================
  // EVIDENCE GENERATION
  // ============================================================================

  private generateEvidenceViews(rule: InferredBusinessRule): EvidenceView[] {
    const views: EvidenceView[] = [];

    // Input/Output examples view
    if (rule.evidence.length > 0) {
      views.push({
        type: 'input_output',
        title: 'Input/Output Examples',
        data: this.formatInputOutputExamples(rule.evidence),
        highlighted: true,
      });
    }

    // Execution path summary
    const pathSummary = this.summarizeExecutionPaths(rule.evidence);
    if (pathSummary) {
      views.push({
        type: 'execution_path',
        title: 'Execution Paths Observed',
        data: pathSummary,
      });
    }

    // Statistical summary
    views.push({
      type: 'statistics',
      title: 'Inference Statistics',
      data: {
        observationCount: rule.evidence.length,
        confidence: rule.confidence,
        inferenceMethod: rule.inferenceMethod,
        formula: rule.formula,
      },
    });

    // Trace links (first 3)
    for (let i = 0; i < Math.min(3, rule.evidence.length); i++) {
      const evidence = rule.evidence[i]!;
      views.push({
        type: 'trace',
        title: `Trace ${i + 1}`,
        data: {
          traceId: evidence.traceId,
          timestamp: evidence.timestamp,
          inputs: evidence.inputs,
          outputs: evidence.outputs,
          path: evidence.path.slice(0, 10),
        },
      });
    }

    return views;
  }

  private formatInputOutputExamples(evidence: RuleEvidence[]): InputOutputExample[] {
    return evidence.slice(0, 5).map((e, index) => ({
      example: index + 1,
      inputs: this.formatVariables(e.inputs),
      outputs: this.formatVariables(e.outputs),
      timestamp: e.timestamp,
    }));
  }

  private formatVariables(vars: Record<string, unknown>): FormattedVariable[] {
    return Object.entries(vars).map(([name, value]) => ({
      name,
      value: this.formatValue(value),
      type: typeof value,
    }));
  }

  private formatValue(value: unknown): string {
    if (value === null || value === undefined) return 'NULL';
    if (typeof value === 'string') return `"${value}"`;
    if (typeof value === 'number') return value.toString();
    if (typeof value === 'boolean') return value ? 'TRUE' : 'FALSE';
    return JSON.stringify(value);
  }

  private summarizeExecutionPaths(evidence: RuleEvidence[]): PathSummary | null {
    if (evidence.length === 0) return null;

    const pathCounts = new Map<string, number>();
    for (const e of evidence) {
      const pathKey = e.path.join(' → ');
      pathCounts.set(pathKey, (pathCounts.get(pathKey) || 0) + 1);
    }

    const uniquePaths = Array.from(pathCounts.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5);

    return {
      totalExecutions: evidence.length,
      uniquePathCount: pathCounts.size,
      topPaths: uniquePaths.map(([path, count]) => ({
        path,
        count,
        percentage: (count / evidence.length) * 100,
      })),
    };
  }

  // ============================================================================
  // ACTION SUGGESTIONS
  // ============================================================================

  private generateSuggestedActions(rule: InferredBusinessRule): SuggestedAction[] {
    const actions: SuggestedAction[] = [];

    // High confidence rules
    if (rule.confidence >= 0.95) {
      actions.push({
        type: 'approve',
        label: 'Approve',
        description: 'High confidence inference - recommend approval',
        priority: 'high',
        automated: true,
      });
    }

    // Low confidence rules
    if (rule.confidence < 0.5) {
      actions.push({
        type: 'investigate',
        label: 'Investigate',
        description: 'Low confidence - needs more evidence or SME input',
        priority: 'high',
      });
    }

    // Rules with formulas
    if (rule.formula) {
      actions.push({
        type: 'approve',
        label: 'Verify Formula',
        description: `Confirm formula: ${rule.formula}`,
        priority: 'medium',
      });
    }

    // Rules needing validation
    if (rule.needsValidation) {
      actions.push({
        type: 'edit',
        label: 'Review & Edit',
        description: 'This rule may need corrections based on domain knowledge',
        priority: 'medium',
      });
    }

    // Default actions
    actions.push(
      {
        type: 'approve',
        label: 'Approve As-Is',
        description: 'Accept the inferred rule',
        priority: 'low',
      },
      {
        type: 'reject',
        label: 'Reject',
        description: 'This inference is incorrect',
        priority: 'low',
      }
    );

    return actions;
  }

  // ============================================================================
  // RULE TRANSFORMATION
  // ============================================================================

  private applyReviewToRule(
    rule: InferredBusinessRule,
    review: SMEReviewResult
  ): BusinessRule {
    const modifications = review.modifications || {};

    return {
      ...rule,
      name: modifications.newName || rule.name,
      description: modifications.newDescription || rule.description,
      logic: modifications.newLogic || rule.logic,
      formula: modifications.newFormula || rule.formula,
      edgeCases: modifications.newEdgeCases || rule.edgeCases,
      assumptions: modifications.newAssumptions || rule.assumptions,
      confidence: modifications.confidenceAdjustment !== undefined
        ? rule.confidence + modifications.confidenceAdjustment
        : rule.confidence,
      reviewStatus: this.mapDecisionToStatus(review.decision),
      reviewedBy: review.reviewedBy,
      reviewedAt: review.reviewedAt,
      reviewComments: review.comments,
    };
  }

  private mapDecisionToStatus(decision: SMEReviewResult['decision']): ReviewStatus {
    switch (decision) {
      case 'approved':
      case 'modified':
        return 'approved';
      case 'rejected':
        return 'rejected';
      case 'needs_clarification':
        return 'needs_clarification';
      default:
        return 'pending';
    }
  }

  private convertToBusinessRule(
    rule: InferredBusinessRule,
    status: ReviewStatus
  ): BusinessRule {
    return {
      ...rule,
      reviewStatus: status,
      // Remove inference-specific fields
      evidence: undefined as unknown as undefined,
      inferenceMethod: undefined as unknown as undefined,
      needsValidation: undefined as unknown as undefined,
      suggestedTestCases: undefined as unknown as undefined,
    } as BusinessRule;
  }

  private collectAllTestCases(session: SMEValidationSession): SuggestedTestCase[] {
    const testCases: SuggestedTestCase[] = [];

    for (const item of session.rules) {
      // Add rule's suggested test cases
      if (item.rule.suggestedTestCases) {
        testCases.push(...item.rule.suggestedTestCases);
      }

      // Add SME's additional test cases
      if (item.reviewResult?.additionalTestCases) {
        testCases.push(...item.reviewResult.additionalTestCases);
      }
    }

    return testCases;
  }

  // ============================================================================
  // REPORTING
  // ============================================================================

  /**
   * Generate validation progress report
   */
  generateProgressReport(sessionId: string): ValidationProgressReport {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error('Session not found');
    }

    const elapsed = Date.now() - session.startedAt.getTime();
    const avgTimePerRule = session.reviewedRules > 0
      ? elapsed / session.reviewedRules
      : 0;
    const estimatedRemaining = avgTimePerRule * (session.totalRules - session.reviewedRules);

    return {
      sessionId,
      smeName: session.smeName,
      progress: {
        total: session.totalRules,
        reviewed: session.reviewedRules,
        remaining: session.totalRules - session.reviewedRules,
        percentComplete: (session.reviewedRules / session.totalRules) * 100,
      },
      decisions: {
        approved: session.approvedRules,
        rejected: session.rejectedRules,
        needsClarification: session.needsClarificationRules,
        modified: session.reviewedRules - session.approvedRules - session.rejectedRules - session.needsClarificationRules,
      },
      timing: {
        startedAt: session.startedAt,
        elapsedMs: elapsed,
        avgTimePerRuleMs: avgTimePerRule,
        estimatedRemainingMs: estimatedRemaining,
      },
      qualityMetrics: this.calculateQualityMetrics(session),
    };
  }

  private calculateQualityMetrics(session: SMEValidationSession): QualityMetrics {
    const reviewed = session.rules.filter(r => r.status === 'reviewed');
    if (reviewed.length === 0) {
      return {
        approvalRate: 0,
        rejectionRate: 0,
        modificationRate: 0,
        avgConfidenceApproved: 0,
        avgConfidenceRejected: 0,
      };
    }

    const approved = reviewed.filter(r => r.reviewResult?.decision === 'approved');
    const rejected = reviewed.filter(r => r.reviewResult?.decision === 'rejected');
    const modified = reviewed.filter(r => r.reviewResult?.decision === 'modified');

    const avgConfidenceApproved = approved.length > 0
      ? approved.reduce((sum, r) => sum + r.rule.confidence, 0) / approved.length
      : 0;
    
    const avgConfidenceRejected = rejected.length > 0
      ? rejected.reduce((sum, r) => sum + r.rule.confidence, 0) / rejected.length
      : 0;

    return {
      approvalRate: approved.length / reviewed.length,
      rejectionRate: rejected.length / reviewed.length,
      modificationRate: modified.length / reviewed.length,
      avgConfidenceApproved,
      avgConfidenceRejected,
    };
  }
}

// ============================================================================
// SUPPORTING INTERFACES
// ============================================================================

interface ReviewHistoryEntry {
  sessionId: string;
  ruleId: string;
  result: SMEReviewResult;
  timestamp: Date;
}

interface GroupedReviewResults {
  approved: InferredBusinessRule[];
  rejected: InferredBusinessRule[];
  needsClarification: InferredBusinessRule[];
  modified: InferredBusinessRule[];
}

interface InputOutputExample {
  example: number;
  inputs: FormattedVariable[];
  outputs: FormattedVariable[];
  timestamp: Date;
}

interface FormattedVariable {
  name: string;
  value: string;
  type: string;
}

interface PathSummary {
  totalExecutions: number;
  uniquePathCount: number;
  topPaths: {
    path: string;
    count: number;
    percentage: number;
  }[];
}

export interface ExportedValidationResult {
  sessionId: string;
  projectId: string;
  exportedAt: Date;
  summary: {
    totalRules: number;
    approved: number;
    rejected: number;
    needsClarification: number;
    completionRate: number;
  };
  approvedRules: BusinessRule[];
  rejectedRules: {
    rule: BusinessRule;
    rejectionReason: string;
  }[];
  rulesNeedingClarification: {
    rule: BusinessRule;
    questions: string[];
  }[];
  suggestedTestCases: SuggestedTestCase[];
}

export interface ValidationProgressReport {
  sessionId: string;
  smeName: string;
  progress: {
    total: number;
    reviewed: number;
    remaining: number;
    percentComplete: number;
  };
  decisions: {
    approved: number;
    rejected: number;
    needsClarification: number;
    modified: number;
  };
  timing: {
    startedAt: Date;
    elapsedMs: number;
    avgTimePerRuleMs: number;
    estimatedRemainingMs: number;
  };
  qualityMetrics: QualityMetrics;
}

interface QualityMetrics {
  approvalRate: number;
  rejectionRate: number;
  modificationRate: number;
  avgConfidenceApproved: number;
  avgConfidenceRejected: number;
}
