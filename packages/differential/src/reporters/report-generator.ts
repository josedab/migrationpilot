/**
 * Differential Report Generator
 */

import type {
  BehaviorComparison,
  DifferentialReport,
  ReportSection,
  ReportMetrics,
  CoverageMetrics,
  RiskAssessment,
  RiskFactor,
} from '../types.js';

// ============================================================================
// Report Generator
// ============================================================================

export class DifferentialReportGenerator {
  /**
   * Generate a comprehensive differential report
   */
  generate(
    comparison: BehaviorComparison,
    format: 'json' | 'html' | 'markdown' = 'markdown'
  ): DifferentialReport {
    const sections: ReportSection[] = [
      this.generateSummarySection(comparison),
      this.generateDetailsSection(comparison),
      this.generateMetricsSection(comparison),
      this.generateRecommendationsSection(comparison),
    ];

    return {
      id: `report_${Date.now()}`,
      timestamp: new Date(),
      comparison,
      format,
      sections,
    };
  }

  /**
   * Generate summary section
   */
  private generateSummarySection(comparison: BehaviorComparison): ReportSection {
    const { summary } = comparison;

    return {
      id: 'summary',
      title: 'Executive Summary',
      type: 'summary',
      content: {
        equivalenceRate: `${(summary.equivalenceRate * 100).toFixed(1)}%`,
        testCases: {
          total: summary.totalTestCases,
          equivalent: summary.equivalentCases,
          failed: summary.failedCases,
          warnings: summary.warningCases,
        },
        differences: {
          critical: summary.criticalDifferences,
          warning: summary.warningDifferences,
        },
        performanceRatio: summary.averagePerformanceRatio.toFixed(2),
        verdict: this.getVerdict(summary.equivalenceRate, summary.criticalDifferences),
      },
    };
  }

  /**
   * Generate details section
   */
  private generateDetailsSection(comparison: BehaviorComparison): ReportSection {
    const failedCases = comparison.testCases.filter(tc => !tc.comparison.equivalent);

    return {
      id: 'details',
      title: 'Detailed Findings',
      type: 'details',
      content: {
        failedTestCases: failedCases.map(tc => ({
          id: tc.id,
          name: tc.name,
          outputDiffs: tc.comparison.outputDiffs.map(d => ({
            path: d.path,
            type: d.diffType,
            severity: d.severity,
            legacy: JSON.stringify(d.legacyValue.value),
            migrated: JSON.stringify(d.migratedValue.value),
            explanation: d.explanation,
          })),
          traceDiffs: tc.comparison.traceDiffs.length,
          sideEffectDiffs: tc.comparison.sideEffectDiffs.length,
        })),
        allDifferences: comparison.differences.map(d => ({
          id: d.id,
          type: d.type,
          severity: d.severity,
          description: d.description,
          possibleCauses: d.possibleCauses,
          suggestedFixes: d.suggestedFixes,
        })),
      },
    };
  }

  /**
   * Generate metrics section
   */
  private generateMetricsSection(comparison: BehaviorComparison): ReportSection {
    const metrics = this.calculateMetrics(comparison);

    return {
      id: 'metrics',
      title: 'Quality Metrics',
      type: 'metrics',
      content: metrics,
    };
  }

  /**
   * Generate recommendations section
   */
  private generateRecommendationsSection(comparison: BehaviorComparison): ReportSection {
    const recommendations = this.generateRecommendations(comparison);

    return {
      id: 'recommendations',
      title: 'Recommendations',
      type: 'recommendations',
      content: recommendations,
    };
  }

  /**
   * Calculate quality metrics
   */
  private calculateMetrics(comparison: BehaviorComparison): ReportMetrics {
    const coverage = this.calculateCoverage(comparison);
    const risk = this.assessRisk(comparison);

    return {
      equivalenceScore: comparison.summary.equivalenceRate,
      confidenceLevel: this.calculateConfidence(comparison),
      coverageMetrics: coverage,
      riskAssessment: risk,
    };
  }

  /**
   * Calculate coverage metrics
   */
  private calculateCoverage(comparison: BehaviorComparison): CoverageMetrics {
    const testCases = comparison.testCases;
    let totalBranches = 0;
    let coveredBranches = 0;
    let totalPaths = 0;

    for (const tc of testCases) {
      const legacyBranches = tc.legacyResult.trace.branches.length;
      const coveredLegacy = tc.legacyResult.trace.branches.filter(
        b => b.coverage === 'both'
      ).length;

      totalBranches += legacyBranches;
      coveredBranches += coveredLegacy;
      totalPaths += tc.legacyResult.trace.steps.length;
    }

    return {
      lineCoverage: 0.8, // Placeholder - would need code analysis
      branchCoverage: totalBranches > 0 ? coveredBranches / totalBranches : 1,
      pathCoverage: Math.min(1, totalPaths / (comparison.testCases.length * 100)),
      inputSpaceCoverage: Math.min(1, comparison.testCases.length / 100),
    };
  }

  /**
   * Assess risk level
   */
  private assessRisk(comparison: BehaviorComparison): RiskAssessment {
    const factors: RiskFactor[] = [];
    const { summary, differences } = comparison;

    // Equivalence rate factor
    if (summary.equivalenceRate < 0.9) {
      factors.push({
        category: 'Behavioral Equivalence',
        description: `Only ${(summary.equivalenceRate * 100).toFixed(1)}% of test cases are equivalent`,
        severity: summary.equivalenceRate < 0.7 ? 'critical' : 'high',
        recommendation: 'Investigate failing test cases and fix discrepancies before deployment',
      });
    }

    // Critical differences factor
    if (summary.criticalDifferences > 0) {
      factors.push({
        category: 'Critical Differences',
        description: `${summary.criticalDifferences} critical differences detected`,
        severity: 'critical',
        recommendation: 'Address all critical differences immediately',
      });
    }

    // Performance factor
    if (summary.averagePerformanceRatio > 2) {
      factors.push({
        category: 'Performance Degradation',
        description: `Migrated code is ${summary.averagePerformanceRatio.toFixed(1)}x slower`,
        severity: summary.averagePerformanceRatio > 5 ? 'high' : 'medium',
        recommendation: 'Optimize performance-critical paths',
      });
    }

    // Side effect differences
    const sideEffectDiffs = differences.filter(d => d.type === 'side-effect');
    if (sideEffectDiffs.length > 0) {
      factors.push({
        category: 'Side Effect Changes',
        description: `${sideEffectDiffs.length} side effect differences detected`,
        severity: sideEffectDiffs.some(d => d.severity === 'critical') ? 'high' : 'medium',
        recommendation: 'Review all side effect changes for data integrity',
      });
    }

    // Determine overall risk
    let overallRisk: RiskAssessment['overallRisk'] = 'low';
    if (factors.some(f => f.severity === 'critical')) {
      overallRisk = 'critical';
    } else if (factors.some(f => f.severity === 'high')) {
      overallRisk = 'high';
    } else if (factors.length > 0) {
      overallRisk = 'medium';
    }

    return { overallRisk, factors };
  }

  /**
   * Calculate confidence level
   */
  private calculateConfidence(comparison: BehaviorComparison): number {
    const testCount = comparison.testCases.length;
    const equivalenceRate = comparison.summary.equivalenceRate;

    // Base confidence from test count (diminishing returns)
    const testConfidence = Math.min(1, Math.log10(testCount + 1) / 2);

    // Combine with equivalence rate
    return (testConfidence * 0.3) + (equivalenceRate * 0.7);
  }

  /**
   * Generate recommendations based on findings
   */
  private generateRecommendations(comparison: BehaviorComparison): string[] {
    const recommendations: string[] = [];
    const { summary, differences } = comparison;

    // General recommendation based on equivalence rate
    if (summary.equivalenceRate >= 0.99) {
      recommendations.push(
        '✅ Excellent behavioral equivalence achieved. Proceed with confidence.'
      );
    } else if (summary.equivalenceRate >= 0.95) {
      recommendations.push(
        '⚠️ Good equivalence but some differences remain. Review before production deployment.'
      );
    } else if (summary.equivalenceRate >= 0.8) {
      recommendations.push(
        '🔴 Significant differences detected. Requires thorough review and remediation.'
      );
    } else {
      recommendations.push(
        '🚨 Critical: Low equivalence rate. Major rework may be required.'
      );
    }

    // Specific recommendations based on difference types
    const outputDiffs = differences.filter(d => d.type === 'output');
    const traceDiffs = differences.filter(d => d.type === 'trace');
    const sideEffectDiffs = differences.filter(d => d.type === 'side-effect');

    if (outputDiffs.length > 0) {
      recommendations.push(
        `📊 Review ${outputDiffs.length} output differences - focus on business-critical values.`
      );
    }

    if (traceDiffs.length > 0) {
      recommendations.push(
        `🔍 ${traceDiffs.length} execution path differences detected - verify business logic preservation.`
      );
    }

    if (sideEffectDiffs.length > 0) {
      recommendations.push(
        `💾 ${sideEffectDiffs.length} side effect differences - critical for data integrity review.`
      );
    }

    // Test coverage recommendation
    if (summary.totalTestCases < 100) {
      recommendations.push(
        `📈 Consider adding more test cases (current: ${summary.totalTestCases}) for higher confidence.`
      );
    }

    // Performance recommendation
    if (summary.averagePerformanceRatio > 1.5) {
      recommendations.push(
        `⏱️ Performance optimization recommended - migrated code is ${summary.averagePerformanceRatio.toFixed(1)}x slower.`
      );
    }

    return recommendations;
  }

  /**
   * Get overall verdict
   */
  private getVerdict(equivalenceRate: number, criticalDiffs: number): string {
    if (criticalDiffs > 0) {
      return '🚨 NOT READY - Critical differences must be resolved';
    }
    if (equivalenceRate >= 0.99) {
      return '✅ READY FOR PRODUCTION';
    }
    if (equivalenceRate >= 0.95) {
      return '⚠️ READY WITH CAUTION - Minor differences to review';
    }
    if (equivalenceRate >= 0.8) {
      return '🔶 NEEDS WORK - Significant differences detected';
    }
    return '🚫 NOT READY - Major behavioral differences';
  }

  /**
   * Render report to string
   */
  render(report: DifferentialReport): string {
    switch (report.format) {
      case 'markdown':
        return this.renderMarkdown(report);
      case 'html':
        return this.renderHtml(report);
      case 'json':
        return JSON.stringify(report, null, 2);
      default:
        return this.renderMarkdown(report);
    }
  }

  /**
   * Render as Markdown
   */
  private renderMarkdown(report: DifferentialReport): string {
    const lines: string[] = [];

    lines.push('# Behavioral Differential Analysis Report');
    lines.push('');
    lines.push(`**Generated:** ${report.timestamp.toISOString()}`);
    lines.push(`**Report ID:** ${report.id}`);
    lines.push('');

    for (const section of report.sections) {
      lines.push(`## ${section.title}`);
      lines.push('');

      if (section.type === 'summary') {
        const content = section.content as Record<string, unknown>;
        lines.push(`**Verdict:** ${content['verdict']}`);
        lines.push('');
        lines.push(`- **Equivalence Rate:** ${content['equivalenceRate']}`);
        const testCases = content['testCases'] as Record<string, number>;
        lines.push(`- **Test Cases:** ${testCases['total']} total, ${testCases['equivalent']} equivalent, ${testCases['failed']} failed`);
        const diffs = content['differences'] as Record<string, number>;
        lines.push(`- **Differences:** ${diffs['critical']} critical, ${diffs['warning']} warnings`);
        lines.push(`- **Performance Ratio:** ${content['performanceRatio']}x`);
      } else if (section.type === 'recommendations') {
        const recs = section.content as string[];
        for (const rec of recs) {
          lines.push(`- ${rec}`);
        }
      } else {
        lines.push('```json');
        lines.push(JSON.stringify(section.content, null, 2));
        lines.push('```');
      }

      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Render as HTML
   */
  private renderHtml(report: DifferentialReport): string {
    const lines: string[] = [];

    lines.push('<!DOCTYPE html>');
    lines.push('<html><head>');
    lines.push('<title>Differential Analysis Report</title>');
    lines.push('<style>');
    lines.push('body { font-family: system-ui, sans-serif; max-width: 1200px; margin: 0 auto; padding: 2rem; }');
    lines.push('h1 { color: #1f2937; }');
    lines.push('h2 { color: #374151; border-bottom: 2px solid #e5e7eb; padding-bottom: 0.5rem; }');
    lines.push('.summary { background: #f3f4f6; padding: 1rem; border-radius: 8px; }');
    lines.push('.critical { color: #dc2626; }');
    lines.push('.warning { color: #f59e0b; }');
    lines.push('.success { color: #22c55e; }');
    lines.push('</style>');
    lines.push('</head><body>');

    lines.push('<h1>Behavioral Differential Analysis Report</h1>');
    lines.push(`<p><strong>Generated:</strong> ${report.timestamp.toISOString()}</p>`);

    for (const section of report.sections) {
      lines.push(`<h2>${section.title}</h2>`);
      lines.push(`<div class="${section.type}">`);
      lines.push(`<pre>${JSON.stringify(section.content, null, 2)}</pre>`);
      lines.push('</div>');
    }

    lines.push('</body></html>');
    return lines.join('\n');
  }
}

// ============================================================================
// Factory
// ============================================================================

export function createReportGenerator(): DifferentialReportGenerator {
  return new DifferentialReportGenerator();
}
