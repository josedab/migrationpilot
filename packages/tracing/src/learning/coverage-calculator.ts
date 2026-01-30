/**
 * Coverage Calculator
 * 
 * Analyzes trace coverage against extracted business rules and code paths
 */

import type { BusinessRule } from '@migrationpilot/core';
import type {
  ExecutionTrace,
  CoverageAnalysis,
  CodePathGap,
  SuggestedTestCase,
} from '../types.js';

export interface CoverageConfig {
  projectId: string;
  
  // Coverage thresholds
  minRuleCoverage: number;
  minPathCoverage: number;
  minEdgeCaseCoverage: number;
  
  // Analysis settings
  suggestTestCases: boolean;
  maxSuggestions: number;
}

const DEFAULT_CONFIG: Partial<CoverageConfig> = {
  minRuleCoverage: 0.8,
  minPathCoverage: 0.7,
  minEdgeCaseCoverage: 0.5,
  suggestTestCases: true,
  maxSuggestions: 50,
};

export class CoverageCalculator {
  private config: CoverageConfig;
  private traces: ExecutionTrace[] = [];
  private rules: BusinessRule[] = [];

  constructor(config: Partial<CoverageConfig> & { projectId: string }) {
    this.config = { ...DEFAULT_CONFIG, ...config } as CoverageConfig;
  }

  /**
   * Set traces for coverage analysis
   */
  setTraces(traces: ExecutionTrace[]): void {
    this.traces = traces;
  }

  /**
   * Set business rules for coverage analysis
   */
  setRules(rules: BusinessRule[]): void {
    this.rules = rules;
  }

  /**
   * Calculate comprehensive coverage analysis
   */
  calculateCoverage(): CoverageAnalysis {
    const ruleCoverage = this.calculateRuleCoverage();
    const pathCoverage = this.calculatePathCoverage();
    const edgeCaseCoverage = this.calculateEdgeCaseCoverage();

    const uncoveredRules = this.findUncoveredRules();
    const uncoveredPaths = this.findUncoveredPaths();
    const suggestedTestCases = this.config.suggestTestCases
      ? this.suggestTestCases(uncoveredRules, uncoveredPaths)
      : [];

    return {
      projectId: this.config.projectId,
      analyzedAt: new Date(),
      
      totalRules: this.rules.length,
      rulesWithTraces: ruleCoverage.covered,
      ruleCoverage: ruleCoverage.percentage,
      
      totalCodePaths: pathCoverage.total,
      observedCodePaths: pathCoverage.covered,
      pathCoverage: pathCoverage.percentage,
      
      totalEdgeCases: edgeCaseCoverage.total,
      observedEdgeCases: edgeCaseCoverage.covered,
      edgeCaseCoverage: edgeCaseCoverage.percentage,
      
      uncoveredRules,
      uncoveredPaths,
      suggestedTestCases,
    };
  }

  /**
   * Calculate rule coverage
   */
  private calculateRuleCoverage(): { covered: number; total: number; percentage: number } {
    let covered = 0;

    for (const rule of this.rules) {
      if (this.isRuleCovered(rule)) {
        covered++;
      }
    }

    return {
      covered,
      total: this.rules.length,
      percentage: this.rules.length > 0 ? covered / this.rules.length : 1,
    };
  }

  /**
   * Check if a rule is covered by any trace
   */
  private isRuleCovered(rule: BusinessRule): boolean {
    return this.traces.some(trace => 
      trace.events.some(event =>
        event.location.file === rule.sourceFile &&
        event.location.line >= rule.sourceLines[0] &&
        event.location.line <= rule.sourceLines[1]
      )
    );
  }

  /**
   * Calculate code path coverage
   */
  private calculatePathCoverage(): { covered: number; total: number; percentage: number } {
    // Collect all branches from traces
    const observedBranches = new Set<string>();
    const allBranches = new Set<string>();

    for (const trace of this.traces) {
      for (const event of trace.events) {
        const branchKey = `${event.location.file}:${event.location.line}`;
        
        if (event.type === 'branch_taken') {
          observedBranches.add(`${branchKey}:true`);
          allBranches.add(`${branchKey}:true`);
          allBranches.add(`${branchKey}:false`);
        } else if (event.type === 'branch_not_taken') {
          observedBranches.add(`${branchKey}:false`);
          allBranches.add(`${branchKey}:true`);
          allBranches.add(`${branchKey}:false`);
        }
      }
    }

    return {
      covered: observedBranches.size,
      total: allBranches.size,
      percentage: allBranches.size > 0 ? observedBranches.size / allBranches.size : 1,
    };
  }

  /**
   * Calculate edge case coverage
   */
  private calculateEdgeCaseCoverage(): { covered: number; total: number; percentage: number } {
    let totalEdgeCases = 0;
    let coveredEdgeCases = 0;

    for (const rule of this.rules) {
      totalEdgeCases += rule.edgeCases.length;

      for (const edgeCase of rule.edgeCases) {
        if (this.isEdgeCaseCovered(rule, edgeCase)) {
          coveredEdgeCases++;
        }
      }
    }

    return {
      covered: coveredEdgeCases,
      total: totalEdgeCases,
      percentage: totalEdgeCases > 0 ? coveredEdgeCases / totalEdgeCases : 1,
    };
  }

  /**
   * Check if an edge case is covered
   */
  private isEdgeCaseCovered(rule: BusinessRule, edgeCase: string): boolean {
    // Parse edge case description to extract expected behavior
    const keywords = edgeCase.toLowerCase().split(/\W+/);
    
    // Find traces that might cover this edge case
    const relevantTraces = this.traces.filter(trace =>
      trace.events.some(event =>
        event.location.file === rule.sourceFile &&
        event.location.line >= rule.sourceLines[0] &&
        event.location.line <= rule.sourceLines[1]
      )
    );

    // Check for edge case indicators in traces
    for (const trace of relevantTraces) {
      const traceDescription = this.describeTrace(trace).toLowerCase();
      
      // Check if any keywords from edge case appear in trace
      const matchingKeywords = keywords.filter(kw => traceDescription.includes(kw));
      if (matchingKeywords.length > keywords.length * 0.3) {
        return true;
      }
    }

    return false;
  }

  /**
   * Find rules without trace coverage
   */
  private findUncoveredRules(): string[] {
    return this.rules
      .filter(rule => !this.isRuleCovered(rule))
      .map(rule => rule.id);
  }

  /**
   * Find code paths that haven't been exercised
   */
  private findUncoveredPaths(): CodePathGap[] {
    const observedBranches = new Map<string, { true: boolean; false: boolean }>();

    for (const trace of this.traces) {
      for (const event of trace.events) {
        if (event.type === 'branch_taken' || event.type === 'branch_not_taken') {
          const key = `${event.location.file}:${event.location.line}`;
          const existing = observedBranches.get(key) || { true: false, false: false };
          
          if (event.type === 'branch_taken') {
            existing.true = true;
          } else {
            existing.false = true;
          }
          
          observedBranches.set(key, existing);
        }
      }
    }

    const gaps: CodePathGap[] = [];

    for (const [key, branches] of observedBranches) {
      const [file, lineStr] = key.split(':');
      const line = parseInt(lineStr!, 10);

      if (!branches.true) {
        gaps.push({
          location: { file: file!, line, procedure: undefined, section: undefined },
          condition: this.findConditionAtLocation(file!, line),
          neverTakenBranch: 'true',
          suggestedInput: this.suggestInputForBranch(file!, line, true),
        });
      }

      if (!branches.false) {
        gaps.push({
          location: { file: file!, line, procedure: undefined, section: undefined },
          condition: this.findConditionAtLocation(file!, line),
          neverTakenBranch: 'false',
          suggestedInput: this.suggestInputForBranch(file!, line, false),
        });
      }
    }

    return gaps;
  }

  /**
   * Find the condition at a specific location
   */
  private findConditionAtLocation(file: string, line: number): string {
    for (const trace of this.traces) {
      for (const event of trace.events) {
        if (event.location.file === file && 
            event.location.line === line &&
            event.data.condition) {
          return event.data.condition;
        }
      }
    }
    return 'unknown condition';
  }

  /**
   * Suggest input values to exercise a specific branch
   */
  private suggestInputForBranch(
    _file: string,
    _line: number,
    _branchValue: boolean
  ): Record<string, unknown> | undefined {
    // In a real implementation, would use constraint solving
    // For now, return undefined
    return undefined;
  }

  /**
   * Suggest test cases to improve coverage
   */
  private suggestTestCases(
    uncoveredRules: string[],
    uncoveredPaths: CodePathGap[]
  ): SuggestedTestCase[] {
    const suggestions: SuggestedTestCase[] = [];

    // Suggest tests for uncovered rules
    for (const ruleId of uncoveredRules.slice(0, this.config.maxSuggestions / 2)) {
      const rule = this.rules.find(r => r.id === ruleId);
      if (rule) {
        suggestions.push({
          ruleId,
          reason: `Rule "${rule.name}" has no execution traces`,
          inputs: this.suggestInputsForRule(rule),
          expectedBehavior: rule.logic,
          priority: this.getPriorityForRule(rule),
        });
      }
    }

    // Suggest tests for uncovered paths
    for (const gap of uncoveredPaths.slice(0, this.config.maxSuggestions / 2)) {
      suggestions.push({
        reason: `Branch at ${gap.location.file}:${gap.location.line} never evaluates to ${gap.neverTakenBranch}`,
        inputs: gap.suggestedInput || {},
        expectedBehavior: `Condition ${gap.condition} evaluates to ${gap.neverTakenBranch}`,
        priority: 'medium',
      });
    }

    return suggestions.slice(0, this.config.maxSuggestions);
  }

  /**
   * Suggest input values for testing a rule
   */
  private suggestInputsForRule(rule: BusinessRule): Record<string, unknown> {
    const inputs: Record<string, unknown> = {};

    for (const input of rule.inputs) {
      inputs[input.name] = this.generateValueForType(input.type);
    }

    return inputs;
  }

  /**
   * Generate a sample value for a type
   */
  private generateValueForType(type: string): unknown {
    switch (type) {
      case 'string':
        return 'test_value';
      case 'integer':
      case 'number':
        return 100;
      case 'decimal':
        return 100.50;
      case 'boolean':
        return true;
      case 'date':
        return new Date().toISOString();
      default:
        return null;
    }
  }

  /**
   * Get priority for a rule based on its characteristics
   */
  private getPriorityForRule(rule: BusinessRule): 'high' | 'medium' | 'low' {
    if (rule.confidence < 0.7) return 'high'; // Low confidence needs more testing
    if (rule.category === 'calculation') return 'high'; // Calculations are critical
    if (rule.edgeCases.length > 3) return 'high'; // Many edge cases
    return 'medium';
  }

  /**
   * Describe a trace for analysis
   */
  private describeTrace(trace: ExecutionTrace): string {
    const parts: string[] = [];
    
    parts.push(`inputs: ${JSON.stringify(trace.inputs.parameters)}`);
    parts.push(`outputs: ${JSON.stringify(trace.outputs.parameters)}`);
    
    for (const event of trace.events) {
      if (event.type === 'variable_write') {
        parts.push(`set ${event.data.name} = ${JSON.stringify(event.data.value)}`);
      }
      if (event.type === 'error') {
        parts.push(`error: ${event.data.errorMessage}`);
      }
    }
    
    return parts.join('; ');
  }

  /**
   * Get coverage summary for reporting
   */
  getCoverageSummary(): CoverageSummary {
    const coverage = this.calculateCoverage();
    
    const meetsCriteria = 
      coverage.ruleCoverage >= this.config.minRuleCoverage &&
      coverage.pathCoverage >= this.config.minPathCoverage &&
      coverage.edgeCaseCoverage >= this.config.minEdgeCaseCoverage;

    return {
      meetsCriteria,
      ruleCoverage: {
        current: coverage.ruleCoverage,
        required: this.config.minRuleCoverage,
        gap: Math.max(0, this.config.minRuleCoverage - coverage.ruleCoverage),
      },
      pathCoverage: {
        current: coverage.pathCoverage,
        required: this.config.minPathCoverage,
        gap: Math.max(0, this.config.minPathCoverage - coverage.pathCoverage),
      },
      edgeCaseCoverage: {
        current: coverage.edgeCaseCoverage,
        required: this.config.minEdgeCaseCoverage,
        gap: Math.max(0, this.config.minEdgeCaseCoverage - coverage.edgeCaseCoverage),
      },
      suggestedTestCount: coverage.suggestedTestCases.length,
    };
  }
}

export interface CoverageSummary {
  meetsCriteria: boolean;
  ruleCoverage: {
    current: number;
    required: number;
    gap: number;
  };
  pathCoverage: {
    current: number;
    required: number;
    gap: number;
  };
  edgeCaseCoverage: {
    current: number;
    required: number;
    gap: number;
  };
  suggestedTestCount: number;
}
