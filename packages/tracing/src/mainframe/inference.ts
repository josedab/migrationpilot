/**
 * Behavioral Inference Engine
 * 
 * Analyzes normalized traces to discover and validate business rules
 * through statistical pattern analysis and machine learning techniques.
 */

import { generateId } from '@migrationpilot/core';
import type { BusinessRule, BusinessRuleCategory } from '@migrationpilot/core';
import type { TraceLocation, SuggestedTestCase } from '../types.js';
import type { 
  NormalizedTrace, 
  InferredRule,
  NormalizedVariable,
} from './types.js';

export interface BehavioralInferenceConfig {
  projectId: string;
  
  // Pattern detection thresholds
  minObservationsForRule: number;
  confidenceThreshold: number;
  edgeCaseThreshold: number;
  
  // Analysis settings
  enableFormulaInference: boolean;
  enablePatternMatching: boolean;
  enableStatisticalAnalysis: boolean;
  
  // Limits
  maxRulesPerProgram: number;
  maxExamplesPerRule: number;
}

const DEFAULT_CONFIG: Partial<BehavioralInferenceConfig> = {
  minObservationsForRule: 3,
  confidenceThreshold: 0.7,
  edgeCaseThreshold: 0.05,
  enableFormulaInference: true,
  enablePatternMatching: true,
  enableStatisticalAnalysis: true,
  maxRulesPerProgram: 100,
  maxExamplesPerRule: 10,
};

export interface RuleEvidence {
  traceId: string;
  inputs: Record<string, unknown>;
  outputs: Record<string, unknown>;
  path: string[];
  timestamp: Date;
}

export interface InferredBusinessRule extends BusinessRule {
  evidence: RuleEvidence[];
  inferenceMethod: 'pattern' | 'statistical' | 'formula' | 'hybrid';
  needsValidation: boolean;
  suggestedTestCases: SuggestedTestCase[];
}

export type { SuggestedTestCase };

export class BehavioralInferenceEngine {
  private config: BehavioralInferenceConfig;
  private traces: NormalizedTrace[] = [];
  private ruleObservations = new Map<string, RuleObservation[]>();
  private discoveredRules = new Map<string, InferredBusinessRule>();

  constructor(config: Partial<BehavioralInferenceConfig> & { projectId: string }) {
    this.config = { ...DEFAULT_CONFIG, ...config } as BehavioralInferenceConfig;
  }

  /**
   * Add traces for analysis
   */
  addTraces(traces: NormalizedTrace[]): void {
    this.traces.push(...traces);
    
    // Extract observations from new traces
    for (const trace of traces) {
      this.extractObservations(trace);
    }
  }

  /**
   * Run behavioral inference to discover rules
   */
  inferRules(): InferredBusinessRule[] {
    const rules: InferredBusinessRule[] = [];

    // Phase 1: Pattern-based inference
    if (this.config.enablePatternMatching) {
      rules.push(...this.inferPatternBasedRules());
    }

    // Phase 2: Statistical inference
    if (this.config.enableStatisticalAnalysis) {
      rules.push(...this.inferStatisticalRules());
    }

    // Phase 3: Formula inference
    if (this.config.enableFormulaInference) {
      rules.push(...this.inferFormulaBasedRules());
    }

    // Deduplicate and merge similar rules
    const mergedRules = this.mergeRules(rules);

    // Generate test cases for each rule
    for (const rule of mergedRules) {
      rule.suggestedTestCases = this.generateTestCases(rule);
    }

    // Store discovered rules
    for (const rule of mergedRules) {
      this.discoveredRules.set(rule.id, rule);
    }

    return mergedRules;
  }

  /**
   * Validate existing business rules against trace evidence
   */
  validateRules(rules: BusinessRule[]): RuleValidationResult[] {
    const results: RuleValidationResult[] = [];

    for (const rule of rules) {
      const result = this.validateRule(rule);
      results.push(result);
    }

    return results;
  }

  /**
   * Get all discovered rules
   */
  getDiscoveredRules(): InferredBusinessRule[] {
    return Array.from(this.discoveredRules.values());
  }

  /**
   * Get rules needing SME validation
   */
  getRulesNeedingValidation(): InferredBusinessRule[] {
    return Array.from(this.discoveredRules.values())
      .filter(r => r.needsValidation);
  }

  // ============================================================================
  // OBSERVATION EXTRACTION
  // ============================================================================

  private extractObservations(trace: NormalizedTrace): void {
    // Extract from inferred rules in the trace
    for (const inferredRule of trace.inferredRules) {
      this.recordObservation(inferredRule, trace);
    }

    // Extract calculation patterns
    this.extractCalculationObservations(trace);

    // Extract decision patterns
    this.extractDecisionObservations(trace);

    // Extract transformation patterns
    this.extractTransformationObservations(trace);
  }

  private recordObservation(rule: InferredRule, trace: NormalizedTrace): void {
    const key = this.generateRuleKey(rule);
    const observations = this.ruleObservations.get(key) || [];
    
    observations.push({
      ruleType: rule.type,
      inputs: this.extractInputValues(rule.inputs, trace),
      outputs: this.extractOutputValues(rule.outputs, trace),
      path: trace.executionPath.map(n => `${n.nodeType}:${n.location.line}`),
      traceId: trace.id,
      timestamp: trace.normalizedAt,
      location: rule.sourceLocations[0] || { file: '', line: 0 },
    });

    this.ruleObservations.set(key, observations);
  }

  private generateRuleKey(rule: InferredRule): string {
    const locations = rule.sourceLocations.map(l => `${l.file}:${l.line}`).join(',');
    return `${rule.type}:${locations}:${rule.inputs.join(',')}:${rule.outputs.join(',')}`;
  }

  private extractInputValues(
    inputNames: string[],
    trace: NormalizedTrace
  ): Record<string, unknown> {
    const values: Record<string, unknown> = {};
    for (const name of inputNames) {
      const variable = trace.inputData.variables.find(v => v.name === name);
      if (variable) {
        values[name] = variable.value;
      }
    }
    return values;
  }

  private extractOutputValues(
    outputNames: string[],
    trace: NormalizedTrace
  ): Record<string, unknown> {
    const values: Record<string, unknown> = {};
    for (const name of outputNames) {
      const variable = trace.outputData.variables.find(v => v.name === name);
      if (variable) {
        values[name] = variable.value;
      }
    }
    return values;
  }

  private extractCalculationObservations(trace: NormalizedTrace): void {
    // Look for variable assignments that follow reads
    const states = trace.intermediateStates;
    
    for (let i = 1; i < states.length; i++) {
      const prev = states[i - 1]!;
      const curr = states[i]!;
      
      // Find variables that changed
      for (const currVar of curr.variables) {
        const prevVar = prev.variables.find(v => v.name === currVar.name);
        if (prevVar && prevVar.value !== currVar.value) {
          // This is a potential calculation
          this.recordCalculationObservation(prev, currVar, trace);
        }
      }
    }
  }

  private recordCalculationObservation(
    prevState: { variables: NormalizedVariable[] },
    changedVar: NormalizedVariable,
    trace: NormalizedTrace
  ): void {
    const key = `calculation:${changedVar.name}`;
    const observations = this.ruleObservations.get(key) || [];
    
    observations.push({
      ruleType: 'calculation',
      inputs: Object.fromEntries(
        prevState.variables.map(v => [v.name, v.value])
      ),
      outputs: { [changedVar.name]: changedVar.value },
      path: [],
      traceId: trace.id,
      timestamp: trace.normalizedAt,
      location: { file: trace.program.name, line: 0 },
    });

    this.ruleObservations.set(key, observations);
  }

  private extractDecisionObservations(trace: NormalizedTrace): void {
    // Group branches by location to find decision points
    const branchNodes = trace.executionPath.filter(n => n.nodeType === 'branch');
    
    for (const node of branchNodes) {
      if (!node.condition) continue;
      
      const key = `decision:${node.location.file}:${node.location.line}`;
      const observations = this.ruleObservations.get(key) || [];
      
      observations.push({
        ruleType: 'decision',
        inputs: node.relevantVariables || {},
        outputs: { branch_result: node.conditionResult },
        path: [node.condition],
        traceId: trace.id,
        timestamp: trace.normalizedAt,
        location: node.location,
      });

      this.ruleObservations.set(key, observations);
    }
  }

  private extractTransformationObservations(trace: NormalizedTrace): void {
    // Compare input and output variables for transformations
    for (const inputVar of trace.inputData.variables) {
      for (const outputVar of trace.outputData.variables) {
        if (this.looksLikeTransformation(inputVar.value, outputVar.value)) {
          const key = `transformation:${inputVar.name}:${outputVar.name}`;
          const observations = this.ruleObservations.get(key) || [];
          
          observations.push({
            ruleType: 'transformation',
            inputs: { [inputVar.name]: inputVar.value },
            outputs: { [outputVar.name]: outputVar.value },
            path: [],
            traceId: trace.id,
            timestamp: trace.normalizedAt,
            location: { file: trace.program.name, line: 0 },
          });

          this.ruleObservations.set(key, observations);
        }
      }
    }
  }

  private looksLikeTransformation(input: unknown, output: unknown): boolean {
    if (input === output) return false;
    if (typeof input !== typeof output) return false;
    if (typeof input === 'string' && typeof output === 'string') {
      // Check for common string transformations
      return input.toLowerCase() === output.toLowerCase() ||
             input.trim() === output ||
             input.toUpperCase() === output;
    }
    return false;
  }

  // ============================================================================
  // PATTERN-BASED INFERENCE
  // ============================================================================

  private inferPatternBasedRules(): InferredBusinessRule[] {
    const rules: InferredBusinessRule[] = [];

    for (const [_key, observations] of this.ruleObservations) {
      if (observations.length < this.config.minObservationsForRule) continue;

      const pattern = this.detectPattern(observations);
      if (!pattern) continue;

      const rule = this.createRuleFromPattern(_key, pattern, observations);
      if (rule.confidence >= this.config.confidenceThreshold) {
        rules.push(rule);
      }
    }

    return rules;
  }

  private detectPattern(observations: RuleObservation[]): DetectedPattern | null {
    // Check for consistent input/output mapping
    const ioMap = new Map<string, unknown>();
    let consistent = true;

    for (const obs of observations) {
      const inputKey = JSON.stringify(obs.inputs);
      const existingOutput = ioMap.get(inputKey);
      
      if (existingOutput !== undefined) {
        if (JSON.stringify(existingOutput) !== JSON.stringify(obs.outputs)) {
          consistent = false;
          break;
        }
      } else {
        ioMap.set(inputKey, obs.outputs);
      }
    }

    if (consistent && ioMap.size >= 2) {
      return {
        type: 'mapping',
        inputKeys: Object.keys(observations[0]!.inputs),
        outputKeys: Object.keys(observations[0]!.outputs),
        examples: observations.slice(0, 5).map(o => ({
          inputs: o.inputs,
          outputs: o.outputs,
        })),
        consistency: 1.0,
      };
    }

    return null;
  }

  private createRuleFromPattern(
    key: string,
    pattern: DetectedPattern,
    observations: RuleObservation[]
  ): InferredBusinessRule {
    const [ruleType, ...rest] = key.split(':');
    const location = observations[0]!.location;

    return {
      id: generateId(),
      projectId: this.config.projectId,
      name: `${ruleType}_${rest.join('_')}`.slice(0, 50),
      description: `Inferred ${ruleType} rule based on ${observations.length} observations`,
      category: this.mapRuleTypeToCategory(ruleType!),
      sourceFile: location.file,
      sourceLines: [location.line, location.line],
      sourceCode: '',
      inputs: pattern.inputKeys.map(name => ({
        name,
        type: 'string' as const,
        source: 'inferred',
      })),
      outputs: pattern.outputKeys.map(name => ({
        name,
        type: 'string' as const,
      })),
      logic: this.describePattern(pattern),
      edgeCases: [],
      assumptions: ['Inferred from execution traces'],
      confidence: pattern.consistency * (observations.length / 10),
      reviewStatus: 'pending',
      extractedAt: new Date(),
      extractedBy: 'behavioral-inference-engine',
      version: 1,
      evidence: observations.slice(0, this.config.maxExamplesPerRule).map(o => ({
        traceId: o.traceId,
        inputs: o.inputs,
        outputs: o.outputs,
        path: o.path,
        timestamp: o.timestamp,
      })),
      inferenceMethod: 'pattern',
      needsValidation: pattern.consistency < 0.95,
      suggestedTestCases: [],
    };
  }

  private mapRuleTypeToCategory(ruleType: string): BusinessRuleCategory {
    switch (ruleType) {
      case 'calculation': return 'calculation';
      case 'validation': return 'validation';
      case 'decision': return 'decision';
      case 'transformation': return 'transformation';
      default: return 'decision';
    }
  }

  private describePattern(pattern: DetectedPattern): string {
    const inputs = pattern.inputKeys.join(', ');
    const outputs = pattern.outputKeys.join(', ');
    return `Maps [${inputs}] to [${outputs}] with ${(pattern.consistency * 100).toFixed(0)}% consistency`;
  }

  // ============================================================================
  // STATISTICAL INFERENCE
  // ============================================================================

  private inferStatisticalRules(): InferredBusinessRule[] {
    const rules: InferredBusinessRule[] = [];

    for (const [_key, observations] of this.ruleObservations) {
      if (observations.length < this.config.minObservationsForRule * 2) continue;

      const stats = this.computeStatistics(observations);
      if (!stats) continue;

      // Look for strong correlations
      for (const correlation of stats.correlations) {
        if (correlation.strength >= this.config.confidenceThreshold) {
          rules.push(this.createRuleFromCorrelation(_key, correlation, observations));
        }
      }
    }

    return rules;
  }

  private computeStatistics(observations: RuleObservation[]): Statistics | null {
    if (observations.length < 5) return null;

    const correlations: Correlation[] = [];
    const firstObs = observations[0]!;
    const inputKeys = Object.keys(firstObs.inputs);
    const outputKeys = Object.keys(firstObs.outputs);

    // Compute correlations between inputs and outputs
    for (const inputKey of inputKeys) {
      for (const outputKey of outputKeys) {
        const inputValues = observations.map(o => o.inputs[inputKey]);
        const outputValues = observations.map(o => o.outputs[outputKey]);

        if (inputValues.every(v => typeof v === 'number') &&
            outputValues.every(v => typeof v === 'number')) {
          const correlation = this.pearsonCorrelation(
            inputValues as number[],
            outputValues as number[]
          );

          if (!isNaN(correlation)) {
            correlations.push({
              inputVar: inputKey,
              outputVar: outputKey,
              strength: Math.abs(correlation),
              direction: correlation > 0 ? 'positive' : 'negative',
            });
          }
        }
      }
    }

    return { correlations };
  }

  private pearsonCorrelation(x: number[], y: number[]): number {
    const n = x.length;
    if (n === 0) return NaN;

    const sumX = x.reduce((a, b) => a + b, 0);
    const sumY = y.reduce((a, b) => a + b, 0);
    const sumXY = x.reduce((acc, xi, i) => acc + xi * y[i]!, 0);
    const sumX2 = x.reduce((acc, xi) => acc + xi * xi, 0);
    const sumY2 = y.reduce((acc, yi) => acc + yi * yi, 0);

    const numerator = n * sumXY - sumX * sumY;
    const denominator = Math.sqrt(
      (n * sumX2 - sumX * sumX) * (n * sumY2 - sumY * sumY)
    );

    if (denominator === 0) return NaN;
    return numerator / denominator;
  }

  private createRuleFromCorrelation(
    _key: string,
    correlation: Correlation,
    observations: RuleObservation[]
  ): InferredBusinessRule {
    const location = observations[0]!.location;

    return {
      id: generateId(),
      projectId: this.config.projectId,
      name: `Correlation: ${correlation.inputVar} → ${correlation.outputVar}`,
      description: `Strong ${correlation.direction} correlation detected (r=${correlation.strength.toFixed(2)})`,
      category: 'calculation',
      sourceFile: location.file,
      sourceLines: [location.line, location.line],
      sourceCode: '',
      inputs: [{
        name: correlation.inputVar,
        type: 'decimal' as const,
        source: 'inferred',
      }],
      outputs: [{
        name: correlation.outputVar,
        type: 'decimal' as const,
      }],
      logic: `${correlation.outputVar} is ${correlation.direction}ly correlated with ${correlation.inputVar}`,
      edgeCases: [],
      assumptions: ['Correlation does not imply causation'],
      confidence: correlation.strength,
      reviewStatus: 'pending',
      extractedAt: new Date(),
      extractedBy: 'behavioral-inference-engine',
      version: 1,
      evidence: observations.slice(0, this.config.maxExamplesPerRule).map(o => ({
        traceId: o.traceId,
        inputs: o.inputs,
        outputs: o.outputs,
        path: o.path,
        timestamp: o.timestamp,
      })),
      inferenceMethod: 'statistical',
      needsValidation: true,
      suggestedTestCases: [],
    };
  }

  // ============================================================================
  // FORMULA INFERENCE
  // ============================================================================

  private inferFormulaBasedRules(): InferredBusinessRule[] {
    const rules: InferredBusinessRule[] = [];

    for (const [ruleKey, observations] of this.ruleObservations) {
      if (!ruleKey.startsWith('calculation:')) continue;
      if (observations.length < this.config.minObservationsForRule) continue;

      const formula = this.inferFormula(observations);
      if (!formula) continue;

      rules.push(this.createRuleFromFormula(ruleKey, formula, observations));
    }

    return rules;
  }

  private inferFormula(observations: RuleObservation[]): InferredFormula | null {
    // Get numeric outputs
    const firstObs = observations[0]!;
    const outputKey = Object.keys(firstObs.outputs)[0];
    if (!outputKey) return null;

    const outputs = observations.map(o => o.outputs[outputKey]);
    if (!outputs.every(v => typeof v === 'number')) return null;

    const inputKeys = Object.keys(firstObs.inputs).filter(k =>
      observations.every(o => typeof o.inputs[k] === 'number')
    );

    // Try simple formulas
    for (const inputKey of inputKeys) {
      const inputs = observations.map(o => o.inputs[inputKey] as number);
      const outputNums = outputs as number[];

      // Try direct copy
      if (inputs.every((v, i) => Math.abs(v - outputNums[i]!) < 0.0001)) {
        return {
          expression: inputKey,
          inputVariables: [inputKey],
          outputVariable: outputKey,
          accuracy: 1.0,
        };
      }

      // Try simple multiplication
      const ratios = inputs.map((v, i) => outputNums[i]! / v);
      const avgRatio = ratios.reduce((a, b) => a + b, 0) / ratios.length;
      const ratioVariance = ratios.reduce((acc, r) => acc + Math.pow(r - avgRatio, 2), 0) / ratios.length;
      
      if (ratioVariance < 0.0001 && avgRatio !== 0) {
        return {
          expression: `${inputKey} * ${avgRatio.toFixed(4)}`,
          inputVariables: [inputKey],
          outputVariable: outputKey,
          accuracy: 1 - ratioVariance,
        };
      }

      // Try simple addition with another input
      for (const otherKey of inputKeys) {
        if (otherKey === inputKey) continue;
        
        const otherInputs = observations.map(o => o.inputs[otherKey] as number);
        const sums = inputs.map((v, i) => v + otherInputs[i]!);
        
        if (sums.every((v, i) => Math.abs(v - outputNums[i]!) < 0.0001)) {
          return {
            expression: `${inputKey} + ${otherKey}`,
            inputVariables: [inputKey, otherKey],
            outputVariable: outputKey,
            accuracy: 1.0,
          };
        }
      }
    }

    return null;
  }

  private createRuleFromFormula(
    _key: string,
    formula: InferredFormula,
    observations: RuleObservation[]
  ): InferredBusinessRule {
    const location = observations[0]!.location;

    return {
      id: generateId(),
      projectId: this.config.projectId,
      name: `Formula: ${formula.outputVariable}`,
      description: `Calculated as: ${formula.expression}`,
      category: 'calculation',
      sourceFile: location.file,
      sourceLines: [location.line, location.line],
      sourceCode: '',
      inputs: formula.inputVariables.map(name => ({
        name,
        type: 'decimal' as const,
        source: 'inferred',
      })),
      outputs: [{
        name: formula.outputVariable,
        type: 'decimal' as const,
      }],
      logic: `${formula.outputVariable} = ${formula.expression}`,
      formula: formula.expression,
      edgeCases: [],
      assumptions: [],
      confidence: formula.accuracy,
      reviewStatus: formula.accuracy >= 0.99 ? 'pending' : 'needs_clarification',
      extractedAt: new Date(),
      extractedBy: 'behavioral-inference-engine',
      version: 1,
      evidence: observations.slice(0, this.config.maxExamplesPerRule).map(o => ({
        traceId: o.traceId,
        inputs: o.inputs,
        outputs: o.outputs,
        path: o.path,
        timestamp: o.timestamp,
      })),
      inferenceMethod: 'formula',
      needsValidation: formula.accuracy < 0.99,
      suggestedTestCases: [],
    };
  }

  // ============================================================================
  // RULE VALIDATION
  // ============================================================================

  private validateRule(rule: BusinessRule): RuleValidationResult {
    // Find observations that match this rule's location
    const relevantObservations: RuleObservation[] = [];
    
    for (const [_key, observations] of this.ruleObservations) {
      for (const obs of observations) {
        if (obs.location.file === rule.sourceFile &&
            obs.location.line >= rule.sourceLines[0] &&
            obs.location.line <= rule.sourceLines[1]) {
          relevantObservations.push(obs);
        }
      }
    }

    if (relevantObservations.length === 0) {
      return {
        ruleId: rule.id,
        ruleName: rule.name,
        validated: false,
        confidence: 0,
        observationCount: 0,
        matchingObservations: 0,
        discrepancies: [],
        recommendation: 'Need more execution traces to validate this rule',
      };
    }

    // Check if observed behavior matches rule description
    let matches = 0;
    const discrepancies: RuleDiscrepancy[] = [];

    for (const obs of relevantObservations) {
      const match = this.checkRuleMatch(rule, obs);
      if (match.matches) {
        matches++;
      } else {
        discrepancies.push(match.discrepancy!);
      }
    }

    const confidence = matches / relevantObservations.length;

    return {
      ruleId: rule.id,
      ruleName: rule.name,
      validated: confidence >= this.config.confidenceThreshold,
      confidence,
      observationCount: relevantObservations.length,
      matchingObservations: matches,
      discrepancies: discrepancies.slice(0, 10),
      recommendation: this.generateRecommendation(confidence, discrepancies),
    };
  }

  private checkRuleMatch(
    rule: BusinessRule,
    observation: RuleObservation
  ): { matches: boolean; discrepancy?: RuleDiscrepancy } {
    // Simple matching - in production would use semantic comparison
    if (rule.formula) {
      // Try to evaluate formula
      const evaluated = this.evaluateFormula(rule.formula, observation.inputs);
      const expectedOutput = Object.values(observation.outputs)[0];
      
      if (evaluated !== undefined && Math.abs(evaluated - (expectedOutput as number)) < 0.01) {
        return { matches: true };
      } else {
        return {
          matches: false,
          discrepancy: {
            traceId: observation.traceId,
            inputs: observation.inputs,
            expectedOutput,
            actualOutput: evaluated,
            reason: 'Formula evaluation mismatch',
          },
        };
      }
    }

    // Fall back to category-based matching
    return { matches: observation.ruleType === rule.category };
  }

  private evaluateFormula(formula: string, inputs: Record<string, unknown>): number | undefined {
    try {
      // Simple formula evaluation - in production would use a proper expression parser
      let expr = formula;
      for (const [name, value] of Object.entries(inputs)) {
        if (typeof value === 'number') {
          expr = expr.replace(new RegExp(name, 'g'), String(value));
        }
      }
      
      // Very basic evaluation for simple math
      if (/^[\d\s+\-*/().]+$/.test(expr)) {
        return Function(`"use strict"; return (${expr});`)();
      }
    } catch {
      return undefined;
    }
    return undefined;
  }

  private generateRecommendation(
    confidence: number,
    discrepancies: RuleDiscrepancy[]
  ): string {
    if (confidence >= 0.95) {
      return 'Rule is well-validated by trace evidence';
    } else if (confidence >= 0.7) {
      return 'Rule is mostly validated but has some edge cases to review';
    } else if (confidence >= 0.5) {
      return 'Rule needs SME review - significant variations observed';
    } else {
      const reasons = [...new Set(discrepancies.map(d => d.reason))];
      return `Rule may be incorrect: ${reasons.join(', ')}`;
    }
  }

  // ============================================================================
  // RULE MERGING
  // ============================================================================

  private mergeRules(rules: InferredBusinessRule[]): InferredBusinessRule[] {
    const merged: InferredBusinessRule[] = [];
    const ruleGroups = new Map<string, InferredBusinessRule[]>();

    // Group similar rules
    for (const rule of rules) {
      const groupKey = this.getRuleSimilarityKey(rule);
      const group = ruleGroups.get(groupKey) || [];
      group.push(rule);
      ruleGroups.set(groupKey, group);
    }

    // Merge each group
    for (const group of ruleGroups.values()) {
      if (group.length === 1) {
        merged.push(group[0]!);
      } else {
        merged.push(this.mergeRuleGroup(group));
      }
    }

    return merged.slice(0, this.config.maxRulesPerProgram);
  }

  private getRuleSimilarityKey(rule: InferredBusinessRule): string {
    const inputNames = rule.inputs.map(i => i.name).sort().join(',');
    const outputNames = rule.outputs.map(o => o.name).sort().join(',');
    return `${rule.category}:${inputNames}:${outputNames}`;
  }

  private mergeRuleGroup(rules: InferredBusinessRule[]): InferredBusinessRule {
    // Use the highest confidence rule as base
    rules.sort((a, b) => b.confidence - a.confidence);
    const base = rules[0]!;

    // Merge evidence from all rules
    const allEvidence: RuleEvidence[] = [];
    const seenTraces = new Set<string>();
    
    for (const rule of rules) {
      for (const evidence of rule.evidence) {
        if (!seenTraces.has(evidence.traceId)) {
          allEvidence.push(evidence);
          seenTraces.add(evidence.traceId);
        }
      }
    }

    return {
      ...base,
      evidence: allEvidence.slice(0, this.config.maxExamplesPerRule),
      inferenceMethod: 'hybrid',
    };
  }

  // ============================================================================
  // TEST CASE GENERATION
  // ============================================================================

  private generateTestCases(rule: InferredBusinessRule): SuggestedTestCase[] {
    const testCases: SuggestedTestCase[] = [];

    // Boundary test cases
    testCases.push(...this.generateBoundaryTests(rule));

    // Edge case tests
    testCases.push(...this.generateEdgeCaseTests(rule));

    // Equivalence partition tests
    testCases.push(...this.generatePartitionTests(rule));

    return testCases.slice(0, 10);
  }

  private generateBoundaryTests(rule: InferredBusinessRule): SuggestedTestCase[] {
    const tests: SuggestedTestCase[] = [];

    // Analyze evidence to find boundaries
    for (const input of rule.inputs) {
      const values = rule.evidence
        .map(e => e.inputs[input.name])
        .filter(v => typeof v === 'number') as number[];

      if (values.length < 2) continue;

      const min = Math.min(...values);
      const max = Math.max(...values);

      // Test at boundaries
      tests.push({
        ruleId: rule.id,
        reason: `Boundary test: ${input.name} at minimum value (${min})`,
        inputs: { [input.name]: min },
        expectedBehavior: 'Verify behavior at lower boundary',
        priority: 'high',
      });

      tests.push({
        ruleId: rule.id,
        reason: `Boundary test: ${input.name} at maximum value (${max})`,
        inputs: { [input.name]: max },
        expectedBehavior: 'Verify behavior at upper boundary',
        priority: 'high',
      });
    }

    return tests;
  }

  private generateEdgeCaseTests(rule: InferredBusinessRule): SuggestedTestCase[] {
    const tests: SuggestedTestCase[] = [];

    // Zero values
    for (const input of rule.inputs) {
      tests.push({
        ruleId: rule.id,
        reason: `Edge case: ${input.name} = 0`,
        inputs: { [input.name]: 0 },
        expectedBehavior: 'Verify handling of zero values',
        priority: 'medium',
      });
    }

    // Empty strings for string inputs
    for (const input of rule.inputs) {
      if (input.type === 'string') {
        tests.push({
          ruleId: rule.id,
          reason: `Edge case: ${input.name} empty string`,
          inputs: { [input.name]: '' },
          expectedBehavior: 'Verify handling of empty strings',
          priority: 'medium',
        });
      }
    }

    return tests;
  }

  private generatePartitionTests(rule: InferredBusinessRule): SuggestedTestCase[] {
    const tests: SuggestedTestCase[] = [];

    // Find representative values from each partition
    for (const input of rule.inputs) {
      const values = rule.evidence.map(e => e.inputs[input.name]);
      const uniqueValues = [...new Set(values.map(v => JSON.stringify(v)))];

      if (uniqueValues.length >= 3) {
        tests.push({
          ruleId: rule.id,
          reason: `Partition test: ${input.name} typical value`,
          inputs: { [input.name]: JSON.parse(uniqueValues[Math.floor(uniqueValues.length / 2)]!) },
          expectedBehavior: 'Verify behavior with typical input',
          priority: 'low',
        });
      }
    }

    return tests;
  }
}

// ============================================================================
// SUPPORTING INTERFACES
// ============================================================================

interface RuleObservation {
  ruleType: string;
  inputs: Record<string, unknown>;
  outputs: Record<string, unknown>;
  path: string[];
  traceId: string;
  timestamp: Date;
  location: TraceLocation;
}

interface DetectedPattern {
  type: 'mapping' | 'sequence' | 'transformation';
  inputKeys: string[];
  outputKeys: string[];
  examples: { inputs: Record<string, unknown>; outputs: Record<string, unknown> }[];
  consistency: number;
}

interface Statistics {
  correlations: Correlation[];
}

interface Correlation {
  inputVar: string;
  outputVar: string;
  strength: number;
  direction: 'positive' | 'negative';
}

interface InferredFormula {
  expression: string;
  inputVariables: string[];
  outputVariable: string;
  accuracy: number;
}

export interface RuleValidationResult {
  ruleId: string;
  ruleName: string;
  validated: boolean;
  confidence: number;
  observationCount: number;
  matchingObservations: number;
  discrepancies: RuleDiscrepancy[];
  recommendation: string;
}

interface RuleDiscrepancy {
  traceId: string;
  inputs: Record<string, unknown>;
  expectedOutput: unknown;
  actualOutput: unknown;
  reason: string;
}
