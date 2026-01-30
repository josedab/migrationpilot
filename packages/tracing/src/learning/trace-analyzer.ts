/**
 * Trace Analyzer
 * 
 * Analyzes execution traces to discover patterns and validate AI extractions
 */

import { generateId } from '@migrationpilot/core';
import type { BusinessRule } from '@migrationpilot/core';
import type {
  ExecutionTrace,
  TraceEvent,
  LearnedBehavior,
  BehaviorPattern,
  FieldSignature,
  ConditionPattern,
} from '../types.js';

export interface TraceAnalyzerConfig {
  projectId: string;
  
  // Analysis settings
  minOccurrencesForPattern: number;
  confidenceThreshold: number;
  maxPatternsPerRule: number;
  
  // Edge case detection
  detectEdgeCases: boolean;
  edgeCaseThreshold: number; // Percentage below which something is an edge case
}

const DEFAULT_CONFIG: Partial<TraceAnalyzerConfig> = {
  minOccurrencesForPattern: 5,
  confidenceThreshold: 0.7,
  maxPatternsPerRule: 10,
  detectEdgeCases: true,
  edgeCaseThreshold: 0.05,
};

export class TraceAnalyzer {
  private config: TraceAnalyzerConfig;
  private traces: ExecutionTrace[] = [];
  private learnedBehaviors: Map<string, LearnedBehavior> = new Map();

  constructor(config: Partial<TraceAnalyzerConfig> & { projectId: string }) {
    this.config = { ...DEFAULT_CONFIG, ...config } as TraceAnalyzerConfig;
  }

  /**
   * Add traces for analysis
   */
  addTraces(traces: ExecutionTrace[]): void {
    this.traces.push(...traces);
  }

  /**
   * Analyze all traces to discover behavioral patterns
   */
  analyzeTraces(): LearnedBehavior[] {
    const behaviors: LearnedBehavior[] = [];

    // Group traces by program
    const tracesByProgram = this.groupByProgram();

    for (const [programName, programTraces] of tracesByProgram) {
      // Discover calculation patterns
      const calculations = this.discoverCalculations(programName, programTraces);
      behaviors.push(...calculations);

      // Discover validation patterns
      const validations = this.discoverValidations(programName, programTraces);
      behaviors.push(...validations);

      // Discover decision patterns
      const decisions = this.discoverDecisions(programName, programTraces);
      behaviors.push(...decisions);

      // Discover transformation patterns
      const transformations = this.discoverTransformations(programName, programTraces);
      behaviors.push(...transformations);
    }

    // Store and return
    for (const behavior of behaviors) {
      this.learnedBehaviors.set(behavior.id, behavior);
    }

    return behaviors;
  }

  /**
   * Validate AI-extracted rules against observed behavior
   */
  validateRules(rules: BusinessRule[]): RuleValidationReport {
    const validations: RuleValidation[] = [];

    for (const rule of rules) {
      const validation = this.validateRule(rule);
      validations.push(validation);
    }

    const validRules = validations.filter(v => v.isValid);
    const invalidRules = validations.filter(v => !v.isValid);

    return {
      totalRules: rules.length,
      validatedRules: validRules.length,
      invalidRules: invalidRules.length,
      validationRate: rules.length > 0 ? validRules.length / rules.length : 0,
      validations,
      summary: this.generateValidationSummary(validations),
    };
  }

  /**
   * Validate a single rule against traces
   */
  private validateRule(rule: BusinessRule): RuleValidation {
    // Find traces that exercise this rule's code location
    const relevantTraces = this.findTracesForRule(rule);

    if (relevantTraces.length === 0) {
      return {
        ruleId: rule.id,
        ruleName: rule.name,
        isValid: false,
        confidence: 0,
        tracesAnalyzed: 0,
        matchingTraces: 0,
        discrepancies: [],
        reason: 'No execution traces found for this rule',
      };
    }

    // Check if observed behavior matches rule description
    const discrepancies: RuleDiscrepancy[] = [];
    let matchingTraces = 0;

    for (const trace of relevantTraces) {
      const match = this.checkRuleAgainstTrace(rule, trace);
      if (match.matches) {
        matchingTraces++;
      } else {
        discrepancies.push({
          traceId: trace.id,
          expectedBehavior: match.expected,
          observedBehavior: match.observed,
          inputs: match.inputs,
        });
      }
    }

    const matchRate = matchingTraces / relevantTraces.length;
    const isValid = matchRate >= this.config.confidenceThreshold;

    return {
      ruleId: rule.id,
      ruleName: rule.name,
      isValid,
      confidence: matchRate,
      tracesAnalyzed: relevantTraces.length,
      matchingTraces,
      discrepancies: discrepancies.slice(0, 10), // Limit discrepancies
      reason: isValid ? undefined : `Only ${(matchRate * 100).toFixed(1)}% of traces match rule behavior`,
    };
  }

  private findTracesForRule(rule: BusinessRule): ExecutionTrace[] {
    return this.traces.filter(trace => {
      // Check if any event in the trace is from the rule's source location
      return trace.events.some(event => 
        event.location.file === rule.sourceFile &&
        event.location.line >= rule.sourceLines[0] &&
        event.location.line <= rule.sourceLines[1]
      );
    });
  }

  private checkRuleAgainstTrace(
    rule: BusinessRule,
    trace: ExecutionTrace
  ): { matches: boolean; expected: string; observed: string; inputs: Record<string, unknown> } {
    // Extract relevant events from trace
    const relevantEvents = trace.events.filter(event =>
      event.location.file === rule.sourceFile &&
      event.location.line >= rule.sourceLines[0] &&
      event.location.line <= rule.sourceLines[1]
    );

    // Build observed behavior from events
    const observed = this.describeObservedBehavior(relevantEvents);
    const expected = rule.logic;

    // Simple matching - in production would use semantic comparison
    const matches = this.behaviorMatches(expected, observed, rule.category);

    return {
      matches,
      expected,
      observed,
      inputs: trace.inputs.parameters,
    };
  }

  private describeObservedBehavior(events: TraceEvent[]): string {
    const descriptions: string[] = [];

    for (const event of events) {
      switch (event.type) {
        case 'variable_write':
          descriptions.push(`Set ${event.data.name} = ${JSON.stringify(event.data.value)}`);
          break;
        case 'branch_taken':
          descriptions.push(`Condition ${event.data.condition} evaluated to true`);
          break;
        case 'branch_not_taken':
          descriptions.push(`Condition ${event.data.condition} evaluated to false`);
          break;
        case 'procedure_entry':
          descriptions.push(`Called ${event.data.name}`);
          break;
      }
    }

    return descriptions.join('; ');
  }

  private behaviorMatches(expected: string, observed: string, _category: string): boolean {
    // Simplified matching - in production would use NLP/semantic similarity
    const normalizedExpected = expected.toLowerCase();
    const normalizedObserved = observed.toLowerCase();

    // Check for key terms overlap
    const expectedTerms = new Set(normalizedExpected.split(/\W+/));
    const observedTerms = new Set(normalizedObserved.split(/\W+/));

    let overlap = 0;
    for (const term of expectedTerms) {
      if (observedTerms.has(term)) overlap++;
    }

    return overlap / expectedTerms.size > 0.3; // 30% term overlap
  }

  private groupByProgram(): Map<string, ExecutionTrace[]> {
    const map = new Map<string, ExecutionTrace[]>();
    for (const trace of this.traces) {
      const existing = map.get(trace.programName) || [];
      existing.push(trace);
      map.set(trace.programName, existing);
    }
    return map;
  }

  private discoverCalculations(programName: string, traces: ExecutionTrace[]): LearnedBehavior[] {
    const behaviors: LearnedBehavior[] = [];
    
    // Look for patterns in variable writes
    const writePatterns = new Map<string, { inputs: unknown[]; outputs: unknown[] }>();

    for (const trace of traces) {
      const writes = trace.events.filter(e => e.type === 'variable_write');
      
      for (const write of writes) {
        const key = `${programName}:${write.data.name}`;
        const pattern = writePatterns.get(key) || { inputs: [], outputs: [] };
        pattern.inputs.push(trace.inputs.parameters);
        pattern.outputs.push(write.data.value);
        writePatterns.set(key, pattern);
      }
    }

    // Analyze patterns with sufficient occurrences
    for (const [key, pattern] of writePatterns) {
      if (pattern.inputs.length >= this.config.minOccurrencesForPattern) {
        const formula = this.inferFormula(pattern.inputs, pattern.outputs);
        if (formula) {
          behaviors.push(this.createBehavior(key, 'calculation', formula, pattern, traces));
        }
      }
    }

    return behaviors;
  }

  private discoverValidations(programName: string, traces: ExecutionTrace[]): LearnedBehavior[] {
    const behaviors: LearnedBehavior[] = [];
    
    // Look for conditional patterns that check input validity
    const conditionPatterns = new Map<string, ConditionPattern[]>();

    for (const trace of traces) {
      const branches = trace.events.filter(e => 
        e.type === 'branch_taken' || e.type === 'branch_not_taken'
      );

      for (const branch of branches) {
        const key = `${programName}:${branch.location.line}:${branch.data.condition}`;
        const patterns = conditionPatterns.get(key) || [];
        patterns.push({
          condition: branch.data.condition || '',
          frequency: 1,
          outcome: branch.type === 'branch_taken' ? 'valid' : 'invalid',
        });
        conditionPatterns.set(key, patterns);
      }
    }

    // Create behaviors for validation patterns
    for (const [key, patterns] of conditionPatterns) {
      if (patterns.length >= this.config.minOccurrencesForPattern) {
        const aggregated = this.aggregateConditionPatterns(patterns);
        if (aggregated.some(p => p.frequency > this.config.minOccurrencesForPattern)) {
          behaviors.push(this.createValidationBehavior(key, aggregated, traces));
        }
      }
    }

    return behaviors;
  }

  private discoverDecisions(programName: string, traces: ExecutionTrace[]): LearnedBehavior[] {
    const behaviors: LearnedBehavior[] = [];

    // Look for patterns in conditional branches leading to different outcomes
    const decisionTrees = new Map<string, Map<string, number>>();

    for (const trace of traces) {
      const branches = trace.events.filter(e => e.type === 'branch_taken');
      
      // Build decision path
      const path = branches.map(b => `${b.data.condition}=${b.data.conditionResult}`).join('->');
      
      if (path) {
        const outputKey = JSON.stringify(trace.outputs.parameters);
        const pathOutcomes = decisionTrees.get(path) || new Map();
        pathOutcomes.set(outputKey, (pathOutcomes.get(outputKey) || 0) + 1);
        decisionTrees.set(path, pathOutcomes);
      }
    }

    // Create decision behaviors
    for (const [path, outcomes] of decisionTrees) {
      const totalOccurrences = Array.from(outcomes.values()).reduce((a, b) => a + b, 0);
      if (totalOccurrences >= this.config.minOccurrencesForPattern) {
        behaviors.push(this.createDecisionBehavior(programName, path, outcomes, traces));
      }
    }

    return behaviors;
  }

  private discoverTransformations(programName: string, traces: ExecutionTrace[]): LearnedBehavior[] {
    const behaviors: LearnedBehavior[] = [];

    // Look for data transformation patterns (input type -> output type)
    const transformations = new Map<string, { before: unknown[]; after: unknown[] }>();

    for (const trace of traces) {
      for (const [inputKey, inputValue] of Object.entries(trace.inputs.parameters)) {
        for (const [outputKey, outputValue] of Object.entries(trace.outputs.parameters)) {
          if (this.looksLikeTransformation(inputValue, outputValue)) {
            const key = `${programName}:${inputKey}->${outputKey}`;
            const pattern = transformations.get(key) || { before: [], after: [] };
            pattern.before.push(inputValue);
            pattern.after.push(outputValue);
            transformations.set(key, pattern);
          }
        }
      }
    }

    for (const [key, pattern] of transformations) {
      if (pattern.before.length >= this.config.minOccurrencesForPattern) {
        const transformation = this.inferTransformation(pattern.before, pattern.after);
        if (transformation) {
          behaviors.push(this.createTransformationBehavior(key, transformation, pattern, traces));
        }
      }
    }

    return behaviors;
  }

  private looksLikeTransformation(input: unknown, output: unknown): boolean {
    // Simple heuristics for transformation detection
    if (typeof input === 'string' && typeof output === 'string') {
      return input.length > 0 && output.length > 0 && input !== output;
    }
    if (typeof input === 'number' && typeof output === 'number') {
      return input !== output;
    }
    return false;
  }

  private inferFormula(inputs: unknown[], outputs: unknown[]): string | null {
    // Simple formula inference - in production would use regression/symbolic analysis
    if (inputs.length < 2) return null;

    // Try to detect linear relationship for numeric outputs
    const numericOutputs = outputs.filter(o => typeof o === 'number') as number[];
    if (numericOutputs.length === outputs.length) {
      // Check if constant
      const unique = new Set(numericOutputs);
      if (unique.size === 1) {
        return `= ${numericOutputs[0]}`;
      }
    }

    return null; // Could not infer formula
  }

  private inferTransformation(before: unknown[], after: unknown[]): string | null {
    if (before.length === 0) return null;

    // Check for common string transformations
    if (typeof before[0] === 'string' && typeof after[0] === 'string') {
      const b = before as string[];
      const a = after as string[];

      if (b.every((v, i) => v.toUpperCase() === a[i])) {
        return 'UPPERCASE';
      }
      if (b.every((v, i) => v.toLowerCase() === a[i])) {
        return 'LOWERCASE';
      }
      if (b.every((v, i) => v.trim() === a[i])) {
        return 'TRIM';
      }
    }

    return null;
  }

  private aggregateConditionPatterns(patterns: ConditionPattern[]): ConditionPattern[] {
    const aggregated = new Map<string, ConditionPattern>();
    
    for (const p of patterns) {
      const key = `${p.condition}:${p.outcome}`;
      const existing = aggregated.get(key);
      if (existing) {
        existing.frequency++;
      } else {
        aggregated.set(key, { ...p });
      }
    }
    
    return Array.from(aggregated.values());
  }

  private createBehavior(
    _key: string,
    type: BehaviorPattern['type'],
    formula: string,
    pattern: { inputs: unknown[]; outputs: unknown[] },
    traces: ExecutionTrace[]
  ): LearnedBehavior {
    return {
      id: generateId(),
      projectId: this.config.projectId,
      pattern: {
        type,
        description: `${type} pattern: ${formula}`,
        inputSignature: this.inferSignatures(pattern.inputs),
        outputSignature: this.inferSignatures(pattern.outputs),
        formula,
      },
      frequency: pattern.inputs.length,
      confidence: Math.min(pattern.inputs.length / 100, 1.0),
      supportingTraces: traces.slice(0, 10).map(t => t.id),
      exampleInputs: pattern.inputs.slice(0, 5),
      exampleOutputs: pattern.outputs.slice(0, 5),
      status: 'discovered',
      discoveredAt: new Date(),
    };
  }

  private createValidationBehavior(
    _key: string,
    patterns: ConditionPattern[],
    traces: ExecutionTrace[]
  ): LearnedBehavior {
    return {
      id: generateId(),
      projectId: this.config.projectId,
      pattern: {
        type: 'validation',
        description: `Validation: ${patterns[0]?.condition}`,
        inputSignature: [],
        outputSignature: [],
        conditions: patterns,
      },
      frequency: patterns.reduce((sum, p) => sum + p.frequency, 0),
      confidence: 0.8,
      supportingTraces: traces.slice(0, 10).map(t => t.id),
      exampleInputs: [],
      exampleOutputs: [],
      status: 'discovered',
      discoveredAt: new Date(),
    };
  }

  private createDecisionBehavior(
    programName: string,
    path: string,
    outcomes: Map<string, number>,
    traces: ExecutionTrace[]
  ): LearnedBehavior {
    const conditions = path.split('->').map(c => ({
      condition: c,
      frequency: 1,
      outcome: 'branch',
    }));

    return {
      id: generateId(),
      projectId: this.config.projectId,
      pattern: {
        type: 'decision',
        description: `Decision path in ${programName}`,
        inputSignature: [],
        outputSignature: [],
        conditions,
      },
      frequency: Array.from(outcomes.values()).reduce((a, b) => a + b, 0),
      confidence: 0.7,
      supportingTraces: traces.slice(0, 10).map(t => t.id),
      exampleInputs: [],
      exampleOutputs: [],
      status: 'discovered',
      discoveredAt: new Date(),
    };
  }

  private createTransformationBehavior(
    _key: string,
    transformation: string,
    pattern: { before: unknown[]; after: unknown[] },
    traces: ExecutionTrace[]
  ): LearnedBehavior {
    return {
      id: generateId(),
      projectId: this.config.projectId,
      pattern: {
        type: 'transformation',
        description: `Transform: ${transformation}`,
        inputSignature: this.inferSignatures(pattern.before),
        outputSignature: this.inferSignatures(pattern.after),
        formula: transformation,
      },
      frequency: pattern.before.length,
      confidence: 0.85,
      supportingTraces: traces.slice(0, 10).map(t => t.id),
      exampleInputs: pattern.before.slice(0, 5),
      exampleOutputs: pattern.after.slice(0, 5),
      status: 'discovered',
      discoveredAt: new Date(),
    };
  }

  private inferSignatures(values: unknown[]): FieldSignature[] {
    if (values.length === 0) return [];

    const sample = values[0];
    
    if (typeof sample === 'object' && sample !== null && !Array.isArray(sample)) {
      const signatures: FieldSignature[] = [];
      for (const key of Object.keys(sample)) {
        const fieldValues = values.map(v => (v as Record<string, unknown>)[key]);
        signatures.push(this.inferSingleSignature(key, fieldValues));
      }
      return signatures;
    }

    return [this.inferSingleSignature('value', values)];
  }

  private inferSingleSignature(name: string, values: unknown[]): FieldSignature {
    const types = new Set(values.map(v => typeof v));
    const nonNull = values.filter(v => v !== null && v !== undefined);
    
    let type: FieldSignature['type'] = 'string';
    if (types.has('number')) type = 'number';
    else if (types.has('boolean')) type = 'boolean';
    else if (values.some(v => v instanceof Date)) type = 'date';
    else if (values.some(v => Array.isArray(v))) type = 'array';
    else if (types.has('object')) type = 'object';

    let observedRange: { min: unknown; max: unknown } | undefined;
    if (type === 'number') {
      const nums = nonNull as number[];
      observedRange = { min: Math.min(...nums), max: Math.max(...nums) };
    }

    return {
      name,
      type,
      nullable: values.some(v => v === null || v === undefined),
      observedRange,
      observedValues: type === 'string' ? [...new Set(nonNull as string[])].slice(0, 10) : undefined,
    };
  }

  private generateValidationSummary(validations: RuleValidation[]): ValidationSummary {
    const highConfidence = validations.filter(v => v.confidence >= 0.9);
    const lowConfidence = validations.filter(v => v.confidence < 0.5);

    return {
      overallConfidence: validations.length > 0
        ? validations.reduce((sum, v) => sum + v.confidence, 0) / validations.length
        : 0,
      highConfidenceCount: highConfidence.length,
      lowConfidenceCount: lowConfidence.length,
      commonDiscrepancyTypes: this.findCommonDiscrepancies(validations),
      recommendations: this.generateRecommendations(validations),
    };
  }

  private findCommonDiscrepancies(validations: RuleValidation[]): string[] {
    const types = new Map<string, number>();
    for (const v of validations) {
      for (const d of v.discrepancies) {
        const type = d.expectedBehavior.split(' ')[0] || 'unknown';
        types.set(type, (types.get(type) || 0) + 1);
      }
    }
    return Array.from(types.entries())
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5)
      .map(([type]) => type);
  }

  private generateRecommendations(validations: RuleValidation[]): string[] {
    const recommendations: string[] = [];
    
    const noTraces = validations.filter(v => v.tracesAnalyzed === 0);
    if (noTraces.length > 0) {
      recommendations.push(`${noTraces.length} rules have no execution traces - consider adding test cases`);
    }

    const lowConfidence = validations.filter(v => v.confidence < 0.5 && v.tracesAnalyzed > 0);
    if (lowConfidence.length > 0) {
      recommendations.push(`${lowConfidence.length} rules have low validation confidence - review with SME`);
    }

    return recommendations;
  }

  /**
   * Get all learned behaviors
   */
  getLearnedBehaviors(): LearnedBehavior[] {
    return Array.from(this.learnedBehaviors.values());
  }
}

export interface RuleValidation {
  ruleId: string;
  ruleName: string;
  isValid: boolean;
  confidence: number;
  tracesAnalyzed: number;
  matchingTraces: number;
  discrepancies: RuleDiscrepancy[];
  reason?: string;
}

export interface RuleDiscrepancy {
  traceId: string;
  expectedBehavior: string;
  observedBehavior: string;
  inputs: Record<string, unknown>;
}

export interface RuleValidationReport {
  totalRules: number;
  validatedRules: number;
  invalidRules: number;
  validationRate: number;
  validations: RuleValidation[];
  summary: ValidationSummary;
}

export interface ValidationSummary {
  overallConfidence: number;
  highConfidenceCount: number;
  lowConfidenceCount: number;
  commonDiscrepancyTypes: string[];
  recommendations: string[];
}
