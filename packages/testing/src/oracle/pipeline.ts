/**
 * Continuous Validation Pipeline
 * 
 * Orchestrates automated validation runs, monitors for model drift,
 * and integrates with CI/CD systems.
 */

import { randomUUID } from 'crypto';
import type { TestResult } from '../types.js';
import type {
  BehavioralModel,
  ContinuousValidationConfig,
  ValidationRun,
  FailureAnalysis,
  FailureDetail,
  FailurePattern,
  DriftAnalysis,
  FieldDrift,
  InvariantDrift,
  OracleValidation,
} from './types.js';
import { TestOracle } from './test-oracle.js';
import { BehavioralLearningEngine } from './learning-engine.js';
import { TestCaseSynthesizer } from './synthesizer.js';

const DEFAULT_CONFIG: ContinuousValidationConfig = {
  enabled: true,
  runOnEveryBuild: false,
  runOnDataChange: true,
  failureThreshold: 0.05,  // 5% failure rate triggers alert
  warningThreshold: 0.02,  // 2% failure rate triggers warning
  alertOnFailure: true,
  alertOnDrift: true,
  autoRetrainOnDrift: false,
  autoGenerateTests: true,
};

export class ContinuousValidationPipeline {
  private config: ContinuousValidationConfig;
  private oracle: TestOracle;
  private learningEngine: BehavioralLearningEngine;
  private synthesizer: TestCaseSynthesizer;
  private validationHistory = new Map<string, ValidationRun[]>();
  private callbacks = {
    onValidationComplete: [] as ((run: ValidationRun) => void)[],
    onFailureDetected: [] as ((run: ValidationRun) => void)[],
    onDriftDetected: [] as ((analysis: DriftAnalysis) => void)[],
  };

  constructor(
    projectId: string,
    config?: Partial<ContinuousValidationConfig>
  ) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.oracle = new TestOracle({ projectId });
    this.learningEngine = new BehavioralLearningEngine({ projectId });
    this.synthesizer = new TestCaseSynthesizer(this.oracle);
  }

  /**
   * Register a trained model for validation
   */
  registerModel(model: BehavioralModel): void {
    this.oracle.registerModel(model);
    this.validationHistory.set(model.id, []);
  }

  /**
   * Run validation for a model
   */
  async runValidation(
    modelId: string,
    testResults: TestResult[],
    trigger: ValidationRun['trigger'] = 'manual'
  ): Promise<ValidationRun> {
    const model = this.oracle.getModel(modelId);
    if (!model) {
      throw new Error(`Model not found: ${modelId}`);
    }

    const runId = `run_${randomUUID()}`;
    const startTime = new Date();

    // Validate each test result
    const validations: OracleValidation[] = [];
    for (const result of testResults) {
      const validation = this.oracle.validate(result);
      validations.push(validation);
    }

    // Compute statistics
    const passed = validations.filter(v => v.valid).length;
    const failed = validations.filter(v => !v.valid && 
      v.divergences.some(d => d.severity === 'critical' || d.severity === 'warning')
    ).length;
    const warnings = validations.filter(v => !v.valid && 
      v.divergences.every(d => d.severity === 'info')
    ).length;

    // Analyze failures
    const failedValidations = validations.filter(v => !v.valid);
    const failureAnalysis = failedValidations.length > 0
      ? this.analyzeFailures(failedValidations)
      : undefined;

    // Check for drift
    const driftAnalysis = await this.detectDrift(modelId, validations);

    const run: ValidationRun = {
      id: runId,
      projectId: model.projectId,
      modelId,
      trigger,
      startedAt: startTime,
      completedAt: new Date(),
      status: 'completed',
      totalTests: testResults.length,
      passed,
      failed,
      warnings,
      failureAnalysis,
      driftAnalysis,
    };

    // Store in history
    const history = this.validationHistory.get(modelId) || [];
    history.push(run);
    this.validationHistory.set(modelId, history);

    // Trigger callbacks
    this.notifyValidationComplete(run);

    // Check thresholds and trigger alerts
    const failureRate = failed / testResults.length;
    if (failureRate >= this.config.failureThreshold && this.config.alertOnFailure) {
      this.notifyFailureDetected(run);
    }

    if (driftAnalysis?.driftDetected && this.config.alertOnDrift) {
      this.notifyDriftDetected(driftAnalysis);
    }

    // Auto-retrain if drift detected
    if (driftAnalysis?.retrainRecommended && this.config.autoRetrainOnDrift) {
      // Would trigger retraining here
      console.log('Auto-retrain recommended - would trigger model retraining');
    }

    return run;
  }

  /**
   * Get validation history for a model
   */
  getValidationHistory(modelId: string, limit?: number): ValidationRun[] {
    const history = this.validationHistory.get(modelId) || [];
    return limit ? history.slice(-limit) : history;
  }

  /**
   * Get the latest validation run
   */
  getLatestRun(modelId: string): ValidationRun | undefined {
    const history = this.validationHistory.get(modelId) || [];
    return history[history.length - 1];
  }

  // ============================================================================
  // FAILURE ANALYSIS
  // ============================================================================

  analyzeFailures(validations: OracleValidation[]): FailureAnalysis {
    const criticalFailures: FailureDetail[] = [];
    const patterns = new Map<string, FailurePatternAccumulator>();

    // Collect all failures
    for (const validation of validations) {
      for (const divergence of validation.divergences) {
        if (divergence.severity === 'critical' || divergence.severity === 'warning') {
          criticalFailures.push({
            testCaseId: validation.testCaseId,
            field: divergence.field,
            expected: divergence.predictedValue,
            actual: divergence.actualValue,
            severity: divergence.severity,
            analysis: this.analyzeIndividualFailure(divergence, validation),
          });

          // Accumulate patterns
          const patternKey = `${divergence.field}:${divergence.divergenceType}`;
          const pattern = patterns.get(patternKey) || {
            field: divergence.field,
            type: divergence.divergenceType,
            count: 0,
            testIds: [],
          };
          pattern.count++;
          pattern.testIds.push(validation.testCaseId);
          patterns.set(patternKey, pattern);
        }
      }
    }

    // Convert patterns to output format
    const commonPatterns: FailurePattern[] = Array.from(patterns.entries())
      .filter(([_, p]) => p.count >= 2) // Only patterns occurring 2+ times
      .sort((a, b) => b[1].count - a[1].count)
      .slice(0, 10)
      .map(([_, p]) => ({
        pattern: `${p.type} in field "${p.field}"`,
        affectedTests: p.count,
        affectedFields: [p.field],
        hypothesis: this.generateHypothesis(p),
      }));

    // Generate root cause hypotheses
    const rootCauseHypotheses = this.generateRootCauseHypotheses(criticalFailures, commonPatterns);

    // Generate recommended actions
    const recommendedActions = this.generateRecommendedActions(
      criticalFailures,
      commonPatterns,
      rootCauseHypotheses
    );

    return {
      criticalFailures: criticalFailures.slice(0, 20), // Limit to top 20
      commonPatterns,
      rootCauseHypotheses,
      recommendedActions,
    };
  }

  private analyzeIndividualFailure(
    divergence: OracleValidation['divergences'][0],
    validation: OracleValidation
  ): string {
    const analysis = validation.divergenceAnalysis;
    if (analysis) {
      return `${analysis.classification}: ${analysis.rootCause}`;
    }

    switch (divergence.divergenceType) {
      case 'value':
        return `Value mismatch: expected ${JSON.stringify(divergence.predictedValue)}, got ${JSON.stringify(divergence.actualValue)}`;
      case 'type':
        return `Type mismatch: expected ${typeof divergence.predictedValue}, got ${typeof divergence.actualValue}`;
      case 'missing':
        return `Missing field: expected ${JSON.stringify(divergence.predictedValue)}, but field was not present`;
      case 'extra':
        return `Extra field: unexpected value ${JSON.stringify(divergence.actualValue)}`;
      default:
        return `Unknown divergence type: ${divergence.divergenceType}`;
    }
  }

  private generateHypothesis(pattern: FailurePatternAccumulator): string {
    switch (pattern.type) {
      case 'value':
        return `The field "${pattern.field}" is consistently producing different values than expected. This may indicate a calculation error or changed business logic.`;
      case 'type':
        return `The field "${pattern.field}" has a type mismatch. Check data type conversions in the modern implementation.`;
      case 'missing':
        return `The field "${pattern.field}" is not being populated. Verify the field mapping and output generation logic.`;
      case 'extra':
        return `The field "${pattern.field}" appears in output but was not expected. This may be intentional new functionality or a bug.`;
      default:
        return `Unknown pattern detected for field "${pattern.field}".`;
    }
  }

  private generateRootCauseHypotheses(
    failures: FailureDetail[],
    patterns: FailurePattern[]
  ): string[] {
    const hypotheses: string[] = [];

    // Check for field-specific patterns
    const fieldCounts = new Map<string, number>();
    for (const failure of failures) {
      fieldCounts.set(failure.field, (fieldCounts.get(failure.field) || 0) + 1);
    }

    const mostAffectedField = Array.from(fieldCounts.entries())
      .sort((a, b) => b[1] - a[1])[0];

    if (mostAffectedField && mostAffectedField[1] >= 3) {
      hypotheses.push(
        `Field "${mostAffectedField[0]}" has ${mostAffectedField[1]} failures. ` +
        `This suggests a systematic issue with this field's calculation or mapping.`
      );
    }

    // Check for severity patterns
    const criticalCount = failures.filter(f => f.severity === 'critical').length;
    if (criticalCount > failures.length * 0.5) {
      hypotheses.push(
        `More than 50% of failures are critical. This may indicate a fundamental ` +
        `issue with the implementation or data flow.`
      );
    }

    // Check for pattern-based hypotheses
    if (patterns.length > 0) {
      hypotheses.push(
        `${patterns.length} common failure patterns detected. ` +
        `Focus on addressing these patterns for maximum impact.`
      );
    }

    // Add default hypothesis if none found
    if (hypotheses.length === 0) {
      hypotheses.push(
        'Failures appear to be isolated without clear patterns. ' +
        'Review individual test cases for specific issues.'
      );
    }

    return hypotheses;
  }

  private generateRecommendedActions(
    failures: FailureDetail[],
    patterns: FailurePattern[],
    hypotheses: string[]
  ): string[] {
    const actions: string[] = [];

    // Priority 1: Address critical failures
    const criticalFailures = failures.filter(f => f.severity === 'critical');
    if (criticalFailures.length > 0) {
      actions.push(
        `[CRITICAL] Review and fix ${criticalFailures.length} critical failures immediately`
      );
    }

    // Priority 2: Address common patterns
    for (const pattern of patterns.slice(0, 3)) {
      actions.push(
        `Investigate pattern: ${pattern.pattern} (affects ${pattern.affectedTests} tests)`
      );
    }

    // Priority 3: Field-specific recommendations
    const affectedFields = new Set(failures.map(f => f.field));
    if (affectedFields.size <= 3) {
      actions.push(
        `Focus debugging on fields: ${Array.from(affectedFields).join(', ')}`
      );
    }

    // Priority 4: Model-related recommendations
    if (hypotheses.some(h => h.includes('systematic'))) {
      actions.push(
        'Consider retraining the behavioral model with recent data'
      );
    }

    // Priority 5: Testing recommendations
    actions.push(
      'Add the failing test cases to the regression test suite'
    );

    return actions;
  }

  // ============================================================================
  // DRIFT DETECTION
  // ============================================================================

  async detectDrift(
    modelId: string,
    recentValidations: OracleValidation[]
  ): Promise<DriftAnalysis> {
    const model = this.oracle.getModel(modelId);
    if (!model) {
      return {
        driftDetected: false,
        driftScore: 0,
        driftedFields: [],
        driftedInvariants: [],
        recommendations: [],
        retrainRecommended: false,
      };
    }

    const driftedFields: FieldDrift[] = [];
    const driftedInvariants: InvariantDrift[] = [];

    // Check field drift
    for (const sig of model.outputSignature) {
      const fieldValues = recentValidations
        .map(v => v.actualOutput[sig.name])
        .filter(v => v !== undefined);

      if (fieldValues.length === 0) continue;

      // Compare distribution with model's learned distribution
      if (sig.dataType === 'number') {
        const recentMean = (fieldValues as number[]).reduce((a, b) => a + b, 0) / fieldValues.length;
        const modelMean = sig.observedDistribution?.parameters?.mean;

        if (modelMean !== undefined) {
          const drift = Math.abs(recentMean - modelMean) / Math.abs(modelMean || 1);
          if (drift > 0.1) { // 10% drift threshold
            driftedFields.push({
              field: sig.name,
              driftType: 'distribution',
              previousValue: modelMean,
              currentValue: recentMean,
              driftMagnitude: drift,
            });
          }
        }
      }

      // Check cardinality drift for string fields
      if (sig.dataType === 'string') {
        const recentCardinality = new Set(fieldValues).size;
        const modelCardinality = sig.cardinality || 0;

        if (modelCardinality > 0) {
          const drift = Math.abs(recentCardinality - modelCardinality) / modelCardinality;
          if (drift > 0.2) { // 20% cardinality change
            driftedFields.push({
              field: sig.name,
              driftType: 'cardinality',
              previousValue: modelCardinality,
              currentValue: recentCardinality,
              driftMagnitude: drift,
            });
          }
        }
      }
    }

    // Check invariant drift
    for (const invariant of model.invariants) {
      const relevantValidations = recentValidations.filter(v =>
        v.prediction.usedInvariants.includes(invariant.id)
      );

      if (relevantValidations.length === 0) continue;

      const holdCount = relevantValidations.filter(v => v.valid).length;
      const recentHoldRate = holdCount / relevantValidations.length;

      if (invariant.holdRate - recentHoldRate > 0.1) { // 10% drop in hold rate
        driftedInvariants.push({
          invariantId: invariant.id,
          invariantName: invariant.name,
          previousHoldRate: invariant.holdRate,
          currentHoldRate: recentHoldRate,
        });
      }
    }

    // Calculate overall drift score
    const fieldDriftScore = driftedFields.length > 0
      ? driftedFields.reduce((sum, d) => sum + d.driftMagnitude, 0) / driftedFields.length
      : 0;
    
    const invariantDriftScore = driftedInvariants.length > 0
      ? driftedInvariants.reduce((sum, d) => 
          sum + (d.previousHoldRate - d.currentHoldRate), 0
        ) / driftedInvariants.length
      : 0;

    const driftScore = Math.max(fieldDriftScore, invariantDriftScore);
    const driftDetected = driftScore > 0.1 || driftedFields.length > 0 || driftedInvariants.length > 0;

    // Generate recommendations
    const recommendations: string[] = [];
    if (driftedFields.length > 0) {
      recommendations.push(
        `${driftedFields.length} fields show distribution drift. Review data quality and business logic changes.`
      );
    }
    if (driftedInvariants.length > 0) {
      recommendations.push(
        `${driftedInvariants.length} invariants are failing more often. Verify if business rules have changed.`
      );
    }

    const retrainRecommended = driftScore > 0.2 || driftedInvariants.length >= 3;
    if (retrainRecommended) {
      recommendations.push(
        'Model retraining recommended due to significant drift in behavior patterns.'
      );
    }

    return {
      driftDetected,
      driftScore,
      driftedFields,
      driftedInvariants,
      recommendations,
      retrainRecommended,
    };
  }

  // ============================================================================
  // EVENT CALLBACKS
  // ============================================================================

  onValidationComplete(callback: (run: ValidationRun) => void): void {
    this.callbacks.onValidationComplete.push(callback);
  }

  onFailureDetected(callback: (run: ValidationRun) => void): void {
    this.callbacks.onFailureDetected.push(callback);
  }

  onDriftDetected(callback: (analysis: DriftAnalysis) => void): void {
    this.callbacks.onDriftDetected.push(callback);
  }

  private notifyValidationComplete(run: ValidationRun): void {
    for (const callback of this.callbacks.onValidationComplete) {
      try {
        callback(run);
      } catch (error) {
        console.error('Error in validation complete callback:', error);
      }
    }
  }

  private notifyFailureDetected(run: ValidationRun): void {
    for (const callback of this.callbacks.onFailureDetected) {
      try {
        callback(run);
      } catch (error) {
        console.error('Error in failure detected callback:', error);
      }
    }
  }

  private notifyDriftDetected(analysis: DriftAnalysis): void {
    for (const callback of this.callbacks.onDriftDetected) {
      try {
        callback(analysis);
      } catch (error) {
        console.error('Error in drift detected callback:', error);
      }
    }
  }

  // ============================================================================
  // REPORTING
  // ============================================================================

  /**
   * Generate a validation summary report
   */
  generateReport(modelId: string): ValidationSummaryReport {
    const history = this.validationHistory.get(modelId) || [];
    const model = this.oracle.getModel(modelId);

    if (history.length === 0) {
      return {
        modelId,
        modelName: model?.name || 'Unknown',
        status: 'no_data',
        summary: 'No validation runs found for this model',
        metrics: {
          totalRuns: 0,
          avgPassRate: 0,
          avgFailureRate: 0,
          driftIncidents: 0,
        },
        trends: [],
        recommendations: ['Run initial validation to establish baseline'],
      };
    }

    const recentRuns = history.slice(-10);
    const totalTests = recentRuns.reduce((sum, r) => sum + r.totalTests, 0);
    const totalPassed = recentRuns.reduce((sum, r) => sum + r.passed, 0);
    const totalFailed = recentRuns.reduce((sum, r) => sum + r.failed, 0);
    const driftIncidents = recentRuns.filter(r => r.driftAnalysis?.driftDetected).length;

    const avgPassRate = totalTests > 0 ? totalPassed / totalTests : 0;
    const avgFailureRate = totalTests > 0 ? totalFailed / totalTests : 0;

    // Determine status
    let status: ValidationSummaryReport['status'] = 'healthy';
    if (avgFailureRate >= this.config.failureThreshold) {
      status = 'critical';
    } else if (avgFailureRate >= this.config.warningThreshold) {
      status = 'warning';
    }

    // Calculate trends
    const trends: ValidationTrend[] = [];
    for (let i = 1; i < recentRuns.length; i++) {
      const prev = recentRuns[i - 1]!;
      const curr = recentRuns[i]!;
      const prevRate = prev.passed / prev.totalTests;
      const currRate = curr.passed / curr.totalTests;
      
      trends.push({
        runId: curr.id,
        timestamp: curr.completedAt || curr.startedAt,
        passRate: currRate,
        change: currRate - prevRate,
        direction: currRate > prevRate ? 'improving' : currRate < prevRate ? 'degrading' : 'stable',
      });
    }

    // Generate recommendations
    const recommendations: string[] = [];
    if (status === 'critical') {
      recommendations.push('Immediate investigation required - failure rate exceeds threshold');
    }
    if (driftIncidents > 0) {
      recommendations.push(`${driftIncidents} drift incidents detected - consider model retraining`);
    }
    if (trends.length >= 3 && trends.slice(-3).every(t => t.direction === 'degrading')) {
      recommendations.push('Consistent degradation trend detected - review recent code changes');
    }

    return {
      modelId,
      modelName: model?.name || 'Unknown',
      status,
      summary: `${recentRuns.length} validation runs analyzed. Pass rate: ${(avgPassRate * 100).toFixed(1)}%`,
      metrics: {
        totalRuns: recentRuns.length,
        avgPassRate,
        avgFailureRate,
        driftIncidents,
      },
      trends,
      recommendations,
    };
  }

  /**
   * Get the learning engine instance for external use
   */
  getLearningEngine(): BehavioralLearningEngine {
    return this.learningEngine;
  }

  /**
   * Get the test synthesizer instance for external use  
   */
  getSynthesizer(): TestCaseSynthesizer {
    return this.synthesizer;
  }
}

// ============================================================================
// SUPPORTING INTERFACES
// ============================================================================

interface FailurePatternAccumulator {
  field: string;
  type: string;
  count: number;
  testIds: string[];
}

export interface ValidationSummaryReport {
  modelId: string;
  modelName: string;
  status: 'healthy' | 'warning' | 'critical' | 'no_data';
  summary: string;
  metrics: {
    totalRuns: number;
    avgPassRate: number;
    avgFailureRate: number;
    driftIncidents: number;
  };
  trends: ValidationTrend[];
  recommendations: string[];
}

interface ValidationTrend {
  runId: string;
  timestamp: Date;
  passRate: number;
  change: number;
  direction: 'improving' | 'stable' | 'degrading';
}
