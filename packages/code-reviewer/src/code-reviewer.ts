/**
 * Code Reviewer
 * AI-powered code review that validates changes against business rules
 */

import type { BusinessRule } from '@migrationpilot/core';
import type {
  ReviewRequest,
  ReviewResult,
  ReviewSummary,
  ReviewFinding,
  AffectedRule,
  ReviewOutput,
  ReviewComment,
  ReviewerConfig,
  ReviewStatus,
  FindingSeverity,
  PullRequest,
  ChangedFile,
} from './types.js';
import { DEFAULT_REVIEWER_CONFIG } from './types.js';
import { RuleMatcher } from './rule-matcher.js';

export class CodeReviewer {
  private config: ReviewerConfig;
  private ruleMatcher: RuleMatcher;

  constructor(config: Partial<ReviewerConfig> = {}) {
    this.config = { ...DEFAULT_REVIEWER_CONFIG, ...config };
    this.ruleMatcher = new RuleMatcher();
  }

  /**
   * Review a pull request against business rules
   */
  async review(
    request: ReviewRequest,
    rules: BusinessRule[]
  ): Promise<ReviewResult> {
    const startTime = Date.now();

    // Filter files to review
    const filesToReview = this.filterFiles(request.pr.files);

    // Find affected rules
    const affectedRules = this.ruleMatcher.findAffectedRules(filesToReview, rules);

    // Generate findings
    const findings = this.generateFindings(request.pr, filesToReview, affectedRules, request.options);

    // Filter findings by confidence
    const filteredFindings = findings.filter(
      f => f.confidence >= this.config.minConfidenceToReport
    );

    // Limit findings
    const limitedFindings = filteredFindings.slice(0, this.config.maxTotalFindings);

    // Generate summary
    const summary = this.generateSummary(limitedFindings, affectedRules);

    // Generate recommendations
    const recommendations = this.generateRecommendations(limitedFindings, affectedRules);

    return {
      id: `review_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`,
      prId: request.pr.id,
      projectId: request.projectId,
      timestamp: new Date().toISOString(),
      summary,
      findings: limitedFindings,
      affectedRules,
      recommendations,
      processingTimeMs: Date.now() - startTime,
      rulesChecked: rules.length,
      filesReviewed: filesToReview.length,
    };
  }

  /**
   * Format review result for GitHub PR comment
   */
  formatForGitHub(result: ReviewResult): ReviewOutput {
    const comments = this.generateComments(result);

    const body = this.generateMarkdownBody(result, 'github');

    return {
      status: result.summary.status,
      body,
      comments,
    };
  }

  /**
   * Format review result for GitLab MR comment
   */
  formatForGitLab(result: ReviewResult): ReviewOutput {
    const comments = this.generateComments(result);

    const body = this.generateMarkdownBody(result, 'gitlab');

    return {
      status: result.summary.status,
      body,
      comments,
    };
  }

  /**
   * Find affected rules for a PR (public API for integration)
   */
  findAffectedRules(pr: PullRequest, rules: BusinessRule[]): AffectedRule[] {
    return this.ruleMatcher.findAffectedRules(pr.files, rules);
  }

  // ============================================================================
  // PRIVATE METHODS
  // ============================================================================

  private filterFiles(files: ChangedFile[]): ChangedFile[] {
    return files.filter(file => {
      for (const pattern of this.config.ignoredPaths) {
        const regex = new RegExp(pattern.replace('*', '.*'));
        if (regex.test(file.path)) {
          return false;
        }
      }
      return true;
    });
  }

  private generateFindings(
    _pr: PullRequest,
    files: ChangedFile[],
    affectedRules: AffectedRule[],
    options?: ReviewRequest['options']
  ): ReviewFinding[] {
    const findings: ReviewFinding[] = [];
    const checkBusinessRules = options?.checkBusinessRules !== false;
    const checkCodeQuality = options?.checkCodeQuality !== false;
    const strictMode = options?.strictMode === true;

    // Generate business rule findings
    if (checkBusinessRules) {
      for (const affected of affectedRules) {
        const ruleFinding = this.createRuleFinding(affected, files);
        if (ruleFinding) {
          findings.push(ruleFinding);
        }
      }
    }

    // Generate code quality findings
    if (checkCodeQuality) {
      for (const file of files) {
        const qualityFindings = this.analyzeCodeQuality(file, affectedRules, strictMode);
        findings.push(...qualityFindings);
      }
    }

    return findings;
  }

  private createRuleFinding(affected: AffectedRule, files: ChangedFile[]): ReviewFinding | null {
    const { rule, changeType, affectedFiles, confidence, explanation } = affected;

    if (confidence < this.config.minConfidenceToReport) {
      return null;
    }

    let severity: FindingSeverity;
    let title: string;
    let description: string;

    switch (changeType) {
      case 'potentially_broken':
        severity = 'critical';
        title = `Business Rule "${rule.name}" may be broken`;
        description = `This change may have broken the business rule. ${explanation}\n\n**Rule Description:** ${rule.description}`;
        break;

      case 'removed':
        severity = 'critical';
        title = `Business Rule "${rule.name}" implementation removed`;
        description = `The code implementing this business rule has been deleted. Verify this is intentional.\n\n**Rule Description:** ${rule.description}`;
        break;

      case 'modified':
        severity = 'warning';
        title = `Business Rule "${rule.name}" modified`;
        description = `Changes affect the implementation of this business rule. ${explanation}\n\n**Rule Description:** ${rule.description}`;
        break;

      case 'new_behavior':
        severity = 'info';
        title = `New behavior may affect "${rule.name}"`;
        description = `New code has been added that may interact with this business rule. ${explanation}`;
        break;

      default:
        return null;
    }

    // Find the first affected file for location
    const firstFile = files.find(f => affectedFiles.includes(f.path));
    const firstHunk = firstFile?.hunks[0];

    return {
      id: `finding_${rule.id}_${Date.now()}`,
      type: changeType === 'potentially_broken' ? 'business_rule_violation' : 'business_rule_change',
      severity,
      title,
      description,
      file: affectedFiles[0] || 'unknown',
      startLine: firstHunk?.newStart || 1,
      endLine: firstHunk ? firstHunk.newStart + firstHunk.newLines : 1,
      relatedRuleIds: [rule.id],
      confidence,
      category: rule.category,
      codeSnippet: firstHunk?.content.substring(0, 200),
    };
  }

  private analyzeCodeQuality(
    file: ChangedFile,
    affectedRules: AffectedRule[],
    strictMode: boolean
  ): ReviewFinding[] {
    const findings: ReviewFinding[] = [];

    for (const hunk of file.hunks) {
      const addedCode = hunk.changes
        .filter(c => c.type === 'add')
        .map(c => c.content)
        .join('\n');

      // Check for potential issues in calculation code
      if (affectedRules.some(r => r.rule.category === 'calculation')) {
        // Check for floating point comparisons
        if (/==\s*\d+\.\d+/.test(addedCode) || /!=\s*\d+\.\d+/.test(addedCode)) {
          findings.push({
            id: `finding_float_${Date.now()}`,
            type: 'potential_bug',
            severity: 'warning',
            title: 'Floating point comparison detected',
            description: 'Direct floating point comparisons can lead to precision issues. Consider using a tolerance-based comparison.',
            file: file.path,
            startLine: hunk.newStart,
            endLine: hunk.newStart + hunk.newLines,
            relatedRuleIds: affectedRules.filter(r => r.rule.category === 'calculation').map(r => r.rule.id),
            confidence: 0.8,
            category: 'calculation',
          });
        }

        // Check for division without zero check
        if (/\/\s*\w+/.test(addedCode) && !/if.*[!=]=\s*0/.test(addedCode)) {
          findings.push({
            id: `finding_div_${Date.now()}`,
            type: 'potential_bug',
            severity: strictMode ? 'warning' : 'info',
            title: 'Division without zero check',
            description: 'Division operation detected without an obvious zero check. Verify the divisor cannot be zero.',
            file: file.path,
            startLine: hunk.newStart,
            endLine: hunk.newStart + hunk.newLines,
            relatedRuleIds: [],
            confidence: 0.6,
            category: 'calculation',
          });
        }
      }

      // Check for magic numbers in business logic
      const magicNumbers = addedCode.match(/[^a-zA-Z_]\d{2,}[^a-zA-Z_0-9]/g);
      if (magicNumbers && magicNumbers.length > 2) {
        findings.push({
          id: `finding_magic_${Date.now()}`,
          type: 'code_quality',
          severity: 'info',
          title: 'Magic numbers detected',
          description: `Found ${magicNumbers.length} numeric literals. Consider extracting these as named constants for clarity.`,
          file: file.path,
          startLine: hunk.newStart,
          endLine: hunk.newStart + hunk.newLines,
          relatedRuleIds: [],
          confidence: 0.7,
          category: 'quality',
        });
      }
    }

    return findings;
  }

  private generateSummary(findings: ReviewFinding[], affectedRules: AffectedRule[]): ReviewSummary {
    const criticalFindings = findings.filter(f => f.severity === 'critical').length;
    const warningFindings = findings.filter(f => f.severity === 'warning').length;
    const infoFindings = findings.filter(f => f.severity === 'info' || f.severity === 'suggestion').length;
    const businessRuleViolations = findings.filter(f => f.type === 'business_rule_violation').length;

    let status: ReviewStatus;
    let riskLevel: ReviewSummary['riskLevel'];
    let message: string;

    if (criticalFindings > 0) {
      status = 'changes_requested';
      riskLevel = 'critical';
      message = `Found ${criticalFindings} critical issue(s) that require attention before merging.`;
    } else if (warningFindings > 3) {
      status = 'changes_requested';
      riskLevel = 'high';
      message = `Found ${warningFindings} warning(s) that should be reviewed.`;
    } else if (warningFindings > 0) {
      status = 'needs_discussion';
      riskLevel = 'medium';
      message = `Found ${warningFindings} warning(s). Consider reviewing before merging.`;
    } else if (affectedRules.length > 0) {
      status = 'approved';
      riskLevel = 'low';
      message = `Changes affect ${affectedRules.length} business rule(s) but no issues detected.`;
    } else {
      status = 'approved';
      riskLevel = 'low';
      message = 'No business rule concerns detected.';
    }

    return {
      status,
      riskLevel,
      totalFindings: findings.length,
      criticalFindings,
      warningFindings,
      infoFindings,
      businessRuleViolations,
      message,
    };
  }

  private generateRecommendations(
    findings: ReviewFinding[],
    affectedRules: AffectedRule[]
  ): string[] {
    const recommendations: string[] = [];

    // Business rule recommendations
    const modifiedRules = affectedRules.filter(r => r.changeType === 'modified');
    if (modifiedRules.length > 0) {
      recommendations.push(
        `Review the following business rules to ensure changes maintain expected behavior: ${modifiedRules.map(r => r.rule.name).join(', ')}`
      );
    }

    const brokenRules = affectedRules.filter(r => r.changeType === 'potentially_broken');
    if (brokenRules.length > 0) {
      recommendations.push(
        `Verify that the following rules still function correctly: ${brokenRules.map(r => r.rule.name).join(', ')}`
      );
    }

    // Testing recommendations
    if (affectedRules.some(r => r.rule.category === 'calculation')) {
      recommendations.push('Add unit tests covering boundary values for calculation changes.');
    }

    if (affectedRules.some(r => r.rule.category === 'validation')) {
      recommendations.push('Verify validation rules with both valid and invalid test cases.');
    }

    // Quality recommendations
    const qualityFindings = findings.filter(f => f.type === 'code_quality');
    if (qualityFindings.length > 0) {
      recommendations.push('Consider addressing code quality suggestions to improve maintainability.');
    }

    return recommendations;
  }

  private generateComments(result: ReviewResult): ReviewComment[] {
    const comments: ReviewComment[] = [];

    for (const finding of result.findings) {
      if (finding.severity === 'critical' || finding.severity === 'warning') {
        const body = this.formatFindingAsComment(finding);

        comments.push({
          id: finding.id,
          path: finding.file,
          line: finding.startLine,
          body,
          type: 'line',
          severity: finding.severity,
        });
      }
    }

    return comments.slice(0, this.config.maxFindingsPerFile * 10);
  }

  private formatFindingAsComment(finding: ReviewFinding): string {
    const severityEmoji = {
      critical: '🔴',
      warning: '🟡',
      info: '🔵',
      suggestion: '💡',
    };

    const parts = [
      `${severityEmoji[finding.severity]} **${finding.title}**`,
      '',
      finding.description,
    ];

    if (finding.suggestedFix && this.config.includeSuggestedFixes) {
      parts.push('', '**Suggested Fix:**', finding.suggestedFix);
    }

    if (finding.relatedRuleIds.length > 0 && this.config.includeRuleReferences) {
      parts.push('', `📋 Related Rules: ${finding.relatedRuleIds.join(', ')}`);
    }

    if (this.config.includeConfidenceScores) {
      parts.push('', `_Confidence: ${Math.round(finding.confidence * 100)}%_`);
    }

    return parts.join('\n');
  }

  private generateMarkdownBody(result: ReviewResult, _platform: 'github' | 'gitlab'): string {
    const { summary, findings, affectedRules, recommendations } = result;

    const statusEmoji = {
      approved: '✅',
      changes_requested: '❌',
      needs_discussion: '💬',
    };

    const parts = [
      `## ${statusEmoji[summary.status]} MigrationPilot Code Review`,
      '',
      `**Status:** ${summary.status.replace('_', ' ').toUpperCase()}`,
      `**Risk Level:** ${summary.riskLevel.toUpperCase()}`,
      '',
      summary.message,
      '',
    ];

    // Statistics
    parts.push('### 📊 Summary', '');
    parts.push(`| Metric | Count |`);
    parts.push(`|--------|-------|`);
    parts.push(`| Files Reviewed | ${result.filesReviewed} |`);
    parts.push(`| Rules Checked | ${result.rulesChecked} |`);
    parts.push(`| Critical Issues | ${summary.criticalFindings} |`);
    parts.push(`| Warnings | ${summary.warningFindings} |`);
    parts.push(`| Info | ${summary.infoFindings} |`);
    parts.push('');

    // Affected rules
    if (affectedRules.length > 0) {
      parts.push('### 📋 Affected Business Rules', '');
      for (const affected of affectedRules.slice(0, 10)) {
        const emoji = affected.changeType === 'potentially_broken' ? '⚠️' :
                      affected.changeType === 'removed' ? '🗑️' : '📝';
        parts.push(`- ${emoji} **${affected.rule.name}** (${affected.changeType})`);
        parts.push(`  - ${affected.explanation}`);
        parts.push(`  - Files: ${affected.affectedFiles.join(', ')}`);
      }
      parts.push('');
    }

    // Top findings
    const topFindings = findings.filter(f => f.severity === 'critical' || f.severity === 'warning').slice(0, 5);
    if (topFindings.length > 0) {
      parts.push('### 🔍 Key Findings', '');
      for (const finding of topFindings) {
        parts.push(`<details>`);
        parts.push(`<summary>${finding.severity === 'critical' ? '🔴' : '🟡'} ${finding.title}</summary>`);
        parts.push('');
        parts.push(`**File:** \`${finding.file}:${finding.startLine}\``);
        parts.push('');
        parts.push(finding.description);
        parts.push('');
        parts.push(`</details>`);
        parts.push('');
      }
    }

    // Recommendations
    if (recommendations.length > 0) {
      parts.push('### 💡 Recommendations', '');
      for (const rec of recommendations) {
        parts.push(`- ${rec}`);
      }
      parts.push('');
    }

    // Footer
    parts.push('---');
    parts.push(`_Review generated by MigrationPilot in ${result.processingTimeMs}ms_`);

    return parts.join('\n');
  }
}
