/**
 * Test Oracle - Prediction and Validation Engine
 * 
 * Uses behavioral models to predict expected outputs and validate
 * actual test results against learned behavior.
 */

import { randomUUID } from 'crypto';
import type { TestCase, TestResult, ToleranceConfig } from '../types.js';
import type {
  BehavioralModel,
  OraclePrediction,
  PredictionReasoning,
  OracleValidation,
  OracleDivergence,
  DivergenceAnalysis,
  DivergenceClassification,
  OracleConfig,
  LearnedInvariant,
} from './types.js';

const DEFAULT_TOLERANCE: ToleranceConfig = {
  numeric: 0.001,
  string: 'exact',
  date: 1000, // 1 second
  array: 'ordered',
};

export class TestOracle {
  private config: OracleConfig;
  private models = new Map<string, BehavioralModel>();
  private predictionCache = new Map<string, OraclePrediction>();

  constructor(config: Partial<OracleConfig> & { projectId: string }) {
    this.config = {
      ...config,
      minSamplesForModel: config.minSamplesForModel || 50,
      confidenceThreshold: config.confidenceThreshold || 0.8,
      maxInvariantsPerModel: config.maxInvariantsPerModel || 100,
      defaultTolerance: config.defaultTolerance || DEFAULT_TOLERANCE,
      strictMode: config.strictMode || false,
      enableContinuousLearning: config.enableContinuousLearning || true,
      retrainThreshold: config.retrainThreshold || 0.1,
      cacheEnabled: config.cacheEnabled || true,
      maxCacheSize: config.maxCacheSize || 1000,
    } as OracleConfig;
  }

  /**
   * Register a trained model
   */
  registerModel(model: BehavioralModel): void {
    this.models.set(model.id, model);
  }

  /**
   * Get a registered model
   */
  getModel(modelId: string): BehavioralModel | null {
    return this.models.get(modelId) || null;
  }

  // ============================================================================
  // PREDICTION
  // ============================================================================

  /**
   * Predict expected output for given inputs using the behavioral model
   */
  predict(modelId: string, inputs: Record<string, unknown>): OraclePrediction {
    const model = this.models.get(modelId);
    if (!model) {
      throw new Error(`Model not found: ${modelId}`);
    }

    // Check cache
    const cacheKey = this.getCacheKey(modelId, inputs);
    if (this.config.cacheEnabled && this.predictionCache.has(cacheKey)) {
      return this.predictionCache.get(cacheKey)!;
    }

    const startTime = Date.now();
    const predictedOutput: Record<string, unknown> = {};
    const usedInvariants: string[] = [];
    const usedConstraints: string[] = [];
    const reasoning: PredictionReasoning[] = [];
    let overallConfidence = 0;
    let confidenceCount = 0;

    // Use invariants to predict each output field
    for (const outputSig of model.outputSignature) {
      const fieldPrediction = this.predictField(
        model,
        outputSig.name,
        inputs,
        usedInvariants,
        usedConstraints
      );

      predictedOutput[outputSig.name] = fieldPrediction.value;
      overallConfidence += fieldPrediction.confidence;
      confidenceCount++;

      reasoning.push({
        field: outputSig.name,
        reason: fieldPrediction.reason,
        confidence: fieldPrediction.confidence,
        evidence: fieldPrediction.evidence,
      });
    }

    const prediction: OraclePrediction = {
      testCaseId: `pred_${randomUUID()}`,
      modelId,
      predictedOutput,
      confidence: confidenceCount > 0 ? overallConfidence / confidenceCount : 0,
      usedInvariants,
      usedConstraints,
      reasoning,
      predictionTime: Date.now() - startTime,
      modelVersion: model.version,
    };

    // Cache the prediction
    if (this.config.cacheEnabled) {
      this.cachePrediction(cacheKey, prediction);
    }

    return prediction;
  }

  private predictField(
    model: BehavioralModel,
    fieldName: string,
    inputs: Record<string, unknown>,
    usedInvariants: string[],
    _usedConstraints: string[]
  ): { value: unknown; confidence: number; reason: string; evidence: string[] } {
    const evidence: string[] = [];
    
    // Try formula invariants first (most precise)
    for (const inv of model.invariants) {
      if (!inv.outputFields.includes(fieldName)) continue;
      
      if (inv.type === 'formula') {
        const value = this.evaluateFormula(inv, inputs);
        if (value !== undefined) {
          usedInvariants.push(inv.id);
          evidence.push(`Formula: ${inv.expression}`);
          return {
            value,
            confidence: inv.confidence,
            reason: `Calculated using formula: ${inv.expression}`,
            evidence,
          };
        }
      }
    }

    // Try equality invariants
    for (const inv of model.invariants) {
      if (!inv.outputFields.includes(fieldName)) continue;
      
      if (inv.type === 'equality' && inv.inputFields.length === 1) {
        const inputField = inv.inputFields[0]!;
        if (inputs[inputField] !== undefined) {
          usedInvariants.push(inv.id);
          evidence.push(`Equality: ${inv.expression}`);
          return {
            value: inputs[inputField],
            confidence: inv.confidence,
            reason: `Copied from input field ${inputField}`,
            evidence,
          };
        }
      }
    }

    // Try conditional invariants
    for (const inv of model.invariants) {
      if (!inv.outputFields.includes(fieldName)) continue;
      
      if (inv.type === 'conditional') {
        const match = this.evaluateConditional(inv, inputs);
        if (match !== undefined) {
          usedInvariants.push(inv.id);
          evidence.push(`Conditional: ${inv.expression}`);
          return {
            value: match.value,
            confidence: inv.confidence * 0.9, // Slightly lower for conditionals
            reason: `Matched conditional: ${inv.name}`,
            evidence,
          };
        }
      }
    }

    // Fall back to statistical prediction
    const sig = model.outputSignature.find(s => s.name === fieldName);
    if (sig?.observedDistribution?.parameters?.mean !== undefined) {
      evidence.push(`Statistical mean from ${model.statistics.sampleCount} samples`);
      return {
        value: sig.observedDistribution.parameters.mean,
        confidence: 0.5, // Lower confidence for statistical fallback
        reason: 'Using statistical mean as prediction',
        evidence,
      };
    }

    // Last resort: use most common value
    if (sig?.observedValues && sig.observedValues.length > 0) {
      evidence.push('Most common observed value');
      return {
        value: sig.observedValues[0],
        confidence: 0.3,
        reason: 'Using most common observed value',
        evidence,
      };
    }

    return {
      value: null,
      confidence: 0,
      reason: 'Unable to predict - insufficient model data',
      evidence: ['No applicable invariants or statistics'],
    };
  }

  private evaluateFormula(
    invariant: LearnedInvariant,
    inputs: Record<string, unknown>
  ): unknown {
    // Simple formula evaluation
    // In production, would use a proper expression parser
    const expression = invariant.expression;

    // Handle simple multiplication: "X * input.field"
    const multMatch = expression.match(/^([\d.]+)\s*\*\s*input\.(\w+)$/);
    if (multMatch) {
      const [, multiplier, field] = multMatch;
      const value = inputs[field!] as number;
      if (typeof value === 'number') {
        return parseFloat(multiplier!) * value;
      }
    }

    // Handle simple addition: "input.field1 + input.field2"
    const addMatch = expression.match(/^input\.(\w+)\s*\+\s*input\.(\w+)$/);
    if (addMatch) {
      const [, field1, field2] = addMatch;
      const value1 = inputs[field1!] as number;
      const value2 = inputs[field2!] as number;
      if (typeof value1 === 'number' && typeof value2 === 'number') {
        return value1 + value2;
      }
    }

    // Handle equality: "input.field"
    const eqMatch = expression.match(/^input\.(\w+)$/);
    if (eqMatch) {
      return inputs[eqMatch[1]!];
    }

    return undefined;
  }

  private evaluateConditional(
    invariant: LearnedInvariant,
    inputs: Record<string, unknown>
  ): { value: unknown } | undefined {
    // Parse conditional: if (input.X == "Y") then output.Z == value
    const match = invariant.expression.match(
      /if\s*\(\s*input\.(\w+)\s*==\s*"([^"]+)"\s*\)\s*then\s*output\.\w+\s*==\s*(.+)/
    );
    
    if (match) {
      const [, inputField, expectedValue, outputValue] = match;
      if (String(inputs[inputField!]) === expectedValue) {
        // Parse the output value
        try {
          return { value: JSON.parse(outputValue!) };
        } catch {
          return { value: outputValue };
        }
      }
    }

    return undefined;
  }

  // ============================================================================
  // VALIDATION
  // ============================================================================

  /**
   * Validate test result against oracle prediction
   */
  validate(testResult: TestResult, prediction?: OraclePrediction): OracleValidation {
    // Get or create prediction
    const pred = prediction || this.predictFromTestCase(testResult.testCase);
    
    const divergences: OracleDivergence[] = [];
    const tolerance = testResult.testCase.tolerance || this.config.defaultTolerance;

    // Compare each field
    for (const [field, predictedValue] of Object.entries(pred.predictedOutput)) {
      const actualValue = (testResult.modernOutput as Record<string, unknown>)?.[field];
      
      const divergence = this.compareValues(
        field,
        predictedValue,
        actualValue,
        tolerance
      );
      
      if (divergence) {
        divergences.push(divergence);
      }
    }

    // Check for extra fields in actual output
    if (typeof testResult.modernOutput === 'object' && testResult.modernOutput !== null) {
      for (const field of Object.keys(testResult.modernOutput as Record<string, unknown>)) {
        if (!(field in pred.predictedOutput)) {
          divergences.push({
            field,
            predictedValue: undefined,
            actualValue: (testResult.modernOutput as Record<string, unknown>)[field],
            divergenceType: 'extra',
            severity: 'info',
            withinTolerance: true,
          });
        }
      }
    }

    const valid = divergences.filter(d => !d.withinTolerance).length === 0;

    const validation: OracleValidation = {
      testCaseId: testResult.testCase.id,
      prediction: pred,
      actualOutput: testResult.modernOutput as Record<string, unknown>,
      valid,
      divergences,
    };

    // Add divergence analysis if not valid
    if (!valid) {
      validation.divergenceAnalysis = this.analyzeDivergence(divergences, pred);
    }

    return validation;
  }

  /**
   * Validate a batch of test results
   */
  validateBatch(testResults: TestResult[]): OracleValidation[] {
    return testResults.map(result => this.validate(result));
  }

  private predictFromTestCase(testCase: TestCase): OraclePrediction {
    // Find a suitable model for this test case
    const model = this.findModelForTestCase(testCase);
    if (!model) {
      throw new Error('No suitable model found for test case');
    }
    return this.predict(model.id, testCase.inputs);
  }

  private findModelForTestCase(testCase: TestCase): BehavioralModel | null {
    // If test case has a source rule, find model for that rule
    if (testCase.sourceRule) {
      for (const model of this.models.values()) {
        if (model.ruleId === testCase.sourceRule) {
          return model;
        }
      }
    }
    
    // Otherwise, return the first model (in production would have better matching)
    return this.models.values().next().value || null;
  }

  private compareValues(
    field: string,
    predicted: unknown,
    actual: unknown,
    tolerance: ToleranceConfig
  ): OracleDivergence | null {
    // Handle null/undefined
    if (predicted === null || predicted === undefined) {
      if (actual !== null && actual !== undefined) {
        return {
          field,
          predictedValue: predicted,
          actualValue: actual,
          divergenceType: 'extra',
          severity: 'warning',
          withinTolerance: false,
        };
      }
      return null;
    }

    if (actual === null || actual === undefined) {
      return {
        field,
        predictedValue: predicted,
        actualValue: actual,
        divergenceType: 'missing',
        severity: 'warning',
        withinTolerance: false,
      };
    }

    // Type mismatch
    if (typeof predicted !== typeof actual) {
      return {
        field,
        predictedValue: predicted,
        actualValue: actual,
        divergenceType: 'type',
        severity: 'critical',
        withinTolerance: false,
      };
    }

    // Numeric comparison
    if (typeof predicted === 'number' && typeof actual === 'number') {
      const diff = Math.abs(predicted - actual);
      const numericTolerance = tolerance.numeric || 0.001;
      const withinTolerance = diff <= numericTolerance;
      
      if (!withinTolerance || diff > 0) {
        return {
          field,
          predictedValue: predicted,
          actualValue: actual,
          divergenceType: 'value',
          severity: withinTolerance ? 'info' : 'warning',
          withinTolerance,
          toleranceUsed: numericTolerance,
        };
      }
      return null;
    }

    // String comparison
    if (typeof predicted === 'string' && typeof actual === 'string') {
      let equal = false;
      switch (tolerance.string) {
        case 'trim':
          equal = predicted.trim() === actual.trim();
          break;
        case 'case-insensitive':
          equal = predicted.toLowerCase() === actual.toLowerCase();
          break;
        case 'exact':
        default:
          equal = predicted === actual;
      }

      if (!equal) {
        return {
          field,
          predictedValue: predicted,
          actualValue: actual,
          divergenceType: 'value',
          severity: 'warning',
          withinTolerance: false,
        };
      }
      return null;
    }

    // Array comparison
    if (Array.isArray(predicted) && Array.isArray(actual)) {
      if (tolerance.array === 'unordered') {
        const sortedPred = [...predicted].sort();
        const sortedActual = [...actual].sort();
        if (JSON.stringify(sortedPred) !== JSON.stringify(sortedActual)) {
          return {
            field,
            predictedValue: predicted,
            actualValue: actual,
            divergenceType: 'value',
            severity: 'warning',
            withinTolerance: false,
          };
        }
      } else {
        if (JSON.stringify(predicted) !== JSON.stringify(actual)) {
          return {
            field,
            predictedValue: predicted,
            actualValue: actual,
            divergenceType: 'value',
            severity: 'warning',
            withinTolerance: false,
          };
        }
      }
      return null;
    }

    // Object comparison
    if (typeof predicted === 'object' && typeof actual === 'object') {
      if (JSON.stringify(predicted) !== JSON.stringify(actual)) {
        return {
          field,
          predictedValue: predicted,
          actualValue: actual,
          divergenceType: 'value',
          severity: 'warning',
          withinTolerance: false,
        };
      }
      return null;
    }

    // Default comparison
    if (predicted !== actual) {
      return {
        field,
        predictedValue: predicted,
        actualValue: actual,
        divergenceType: 'value',
        severity: 'warning',
        withinTolerance: false,
      };
    }

    return null;
  }

  // ============================================================================
  // DIVERGENCE ANALYSIS
  // ============================================================================

  private analyzeDivergence(
    divergences: OracleDivergence[],
    prediction: OraclePrediction
  ): DivergenceAnalysis {
    const criticalDivergences = divergences.filter(d => d.severity === 'critical');
    const valueDivergences = divergences.filter(d => d.divergenceType === 'value');
    const missingFields = divergences.filter(d => d.divergenceType === 'missing');

    let classification: DivergenceClassification = 'model_error';
    let rootCause = 'Unknown cause';
    const recommendations: string[] = [];

    // Analyze patterns
    if (criticalDivergences.length > 0) {
      // Type mismatches suggest code bugs
      classification = 'code_bug';
      rootCause = 'Type mismatch detected - likely a code bug in modern implementation';
      recommendations.push('Review the data type handling in the modern code');
      recommendations.push('Check type conversions and null handling');
    } else if (missingFields.length > divergences.length / 2) {
      // Many missing fields suggest incomplete implementation
      classification = 'code_bug';
      rootCause = 'Multiple output fields missing - incomplete implementation';
      recommendations.push('Verify all output fields are properly populated');
    } else if (valueDivergences.length > 0) {
      // Check if divergences are within reasonable bounds
      const toleranceIssues = divergences.filter(d => d.toleranceUsed !== undefined);
      if (toleranceIssues.length === divergences.length) {
        classification = 'tolerance_issue';
        rootCause = 'Values differ slightly - may need tolerance adjustment';
        recommendations.push('Consider adjusting numeric tolerance');
        recommendations.push('Review rounding differences between systems');
      } else {
        // Check prediction confidence
        if (prediction.confidence < 0.7) {
          classification = 'model_error';
          rootCause = 'Low model confidence - may need more training data';
          recommendations.push('Collect more historical execution data');
          recommendations.push('Review the behavioral model for accuracy');
        } else {
          classification = 'code_bug';
          rootCause = 'Value mismatch with high-confidence prediction - likely code bug';
          recommendations.push('Debug the business logic implementation');
          recommendations.push('Compare against the original legacy behavior');
        }
      }
    }

    // Check for edge case patterns
    const affectedFields = divergences.map(d => d.field);
    const reasoning = prediction.reasoning.filter(r => affectedFields.includes(r.field));
    if (reasoning.some(r => r.confidence < 0.5)) {
      classification = 'edge_case';
      rootCause = 'This may be an edge case not well covered by training data';
      recommendations.push('Add this case to the test suite');
      recommendations.push('Verify expected behavior with SME');
    }

    return {
      rootCause,
      classification,
      recommendations,
    };
  }

  // ============================================================================
  // CACHING
  // ============================================================================

  private getCacheKey(modelId: string, inputs: Record<string, unknown>): string {
    return `${modelId}:${JSON.stringify(inputs)}`;
  }

  private cachePrediction(key: string, prediction: OraclePrediction): void {
    // Evict old entries if cache is full
    if (this.predictionCache.size >= this.config.maxCacheSize) {
      const firstKey = this.predictionCache.keys().next().value;
      if (firstKey) {
        this.predictionCache.delete(firstKey);
      }
    }
    this.predictionCache.set(key, prediction);
  }

  /**
   * Clear the prediction cache
   */
  clearCache(): void {
    this.predictionCache.clear();
  }
}
