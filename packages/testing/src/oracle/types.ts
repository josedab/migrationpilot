/**
 * AI-Powered Test Oracle Types
 * 
 * Types for the behavioral learning engine that learns expected behavior
 * from historical execution data to generate and validate test cases.
 */

import type { TestCase, TestResult, ToleranceConfig } from '../types.js';

// ============================================================================
// BEHAVIORAL MODEL TYPES
// ============================================================================

export interface BehavioralModel {
  id: string;
  projectId: string;
  ruleId?: string;
  name: string;
  description: string;
  
  // Model characteristics
  modelType: BehavioralModelType;
  version: number;
  
  // Input/Output signatures
  inputSignature: FieldSignature[];
  outputSignature: FieldSignature[];
  
  // Statistical properties
  statistics: ModelStatistics;
  
  // Learned invariants
  invariants: LearnedInvariant[];
  
  // Learned constraints
  constraints: LearnedConstraint[];
  
  // Edge cases discovered
  edgeCases: DiscoveredEdgeCase[];
  
  // Quality metrics
  quality: ModelQuality;
  
  // Training info
  trainingInfo: TrainingInfo;
  
  createdAt: Date;
  updatedAt: Date;
}

export type BehavioralModelType = 
  | 'statistical'      // Statistical distribution-based
  | 'pattern'          // Pattern matching-based
  | 'regression'       // Regression model for numeric outputs
  | 'decision_tree'    // Decision tree for categorical outputs
  | 'ensemble';        // Combination of models

export interface FieldSignature {
  name: string;
  dataType: 'string' | 'number' | 'boolean' | 'date' | 'object' | 'array';
  nullable: boolean;
  
  // Observed characteristics
  observedDistribution?: Distribution;
  observedRange?: { min: unknown; max: unknown };
  observedValues?: unknown[];
  cardinality?: number;
  nullRate?: number;
}

export interface Distribution {
  type: 'normal' | 'uniform' | 'skewed' | 'categorical' | 'bimodal' | 'unknown';
  parameters?: {
    mean?: number;
    stdDev?: number;
    mode?: unknown;
    skewness?: number;
    kurtosis?: number;
  };
  histogram?: { bucket: string; count: number }[];
}

export interface ModelStatistics {
  sampleCount: number;
  uniqueInputPatterns: number;
  uniqueOutputPatterns: number;
  
  // Input statistics
  inputCorrelations: CorrelationMatrix;
  inputDistributions: Record<string, Distribution>;
  
  // Output statistics
  outputDistributions: Record<string, Distribution>;
  
  // I/O relationships
  inputOutputCorrelations: Record<string, number>;
  
  // Temporal patterns
  temporalPatterns?: TemporalPattern[];
}

export interface CorrelationMatrix {
  fields: string[];
  values: number[][];
}

export interface TemporalPattern {
  name: string;
  pattern: 'daily' | 'weekly' | 'monthly' | 'seasonal' | 'custom';
  description: string;
  affectedFields: string[];
}

// ============================================================================
// INVARIANT TYPES
// ============================================================================

export interface LearnedInvariant {
  id: string;
  name: string;
  type: InvariantType;
  description: string;
  
  // The invariant itself
  expression: string;
  
  // Fields involved
  inputFields: string[];
  outputFields: string[];
  
  // Validation statistics
  holdRate: number;       // How often the invariant holds (0-1)
  violationCount: number;
  totalObservations: number;
  
  // Exceptions
  knownExceptions: InvariantException[];
  
  // Confidence
  confidence: number;
  
  discoveredAt: Date;
}

export type InvariantType = 
  | 'equality'           // output.X == input.Y
  | 'inequality'         // output.X != input.Y
  | 'range'              // output.X in [a, b]
  | 'formula'            // output.X = f(inputs)
  | 'conditional'        // if (condition) then output.X = Y
  | 'ordering'           // output.X <= output.Y
  | 'format'             // output.X matches pattern
  | 'aggregation'        // sum/count/avg relationship
  | 'membership';        // output.X in {set}

export interface InvariantException {
  condition: string;
  description: string;
  frequency: number;
}

// ============================================================================
// CONSTRAINT TYPES
// ============================================================================

export interface LearnedConstraint {
  id: string;
  name: string;
  type: ConstraintType;
  description: string;
  
  // Constraint definition
  expression: string;
  fields: string[];
  
  // Validation
  validationRate: number;
  
  // Severity
  severity: 'error' | 'warning' | 'info';
  
  confidence: number;
}

export type ConstraintType = 
  | 'required'           // Field must be present
  | 'type'               // Field must be of type X
  | 'range'              // Value must be in range
  | 'pattern'            // String must match pattern
  | 'dependency'         // If A then B
  | 'uniqueness'         // Value must be unique
  | 'referential';       // Value must reference existing

// ============================================================================
// EDGE CASE TYPES
// ============================================================================

export interface DiscoveredEdgeCase {
  id: string;
  name: string;
  description: string;
  
  // The triggering condition
  trigger: EdgeCaseTrigger;
  
  // Expected behavior
  expectedBehavior: string;
  expectedOutput?: Record<string, unknown>;
  
  // Observation data
  frequency: number;
  lastObservedAt?: Date;
  exampleInputs: Record<string, unknown>[];
  
  // Risk assessment
  riskLevel: 'critical' | 'high' | 'medium' | 'low';
  
  // Test case generated
  generatedTestCase?: TestCase;
}

export interface EdgeCaseTrigger {
  type: 'boundary' | 'null' | 'empty' | 'overflow' | 'combination' | 'temporal' | 'rare';
  condition: string;
  fields: string[];
}

// ============================================================================
// MODEL QUALITY TYPES
// ============================================================================

export interface ModelQuality {
  accuracy: number;           // How accurately the model predicts outputs
  precision: number;          // True positives / (True positives + False positives)
  recall: number;             // True positives / (True positives + False negatives)
  f1Score: number;            // Harmonic mean of precision and recall
  
  // Coverage
  inputSpaceCoverage: number; // % of input space covered by training data
  outputSpaceCoverage: number;
  edgeCaseCoverage: number;
  
  // Reliability
  consistencyScore: number;   // How consistent are predictions
  confidenceCalibration: number; // How well-calibrated are confidence scores
  
  // Freshness
  dataFreshness: number;      // Days since last training data
  driftDetected: boolean;     // Has model drift been detected
}

export interface TrainingInfo {
  dataSource: string;
  startDate: Date;
  endDate: Date;
  sampleCount: number;
  filterCriteria?: string;
  
  // Training parameters
  parameters: Record<string, unknown>;
  
  // Training metrics
  trainingDuration: number;
  iterations?: number;
  convergenceScore?: number;
}

// ============================================================================
// ORACLE TYPES
// ============================================================================

export interface OracleConfig {
  projectId: string;
  
  // Learning settings
  minSamplesForModel: number;
  confidenceThreshold: number;
  maxInvariantsPerModel: number;
  
  // Prediction settings
  defaultTolerance: ToleranceConfig;
  strictMode: boolean;
  
  // Continuous learning
  enableContinuousLearning: boolean;
  retrainThreshold: number;
  
  // Performance
  cacheEnabled: boolean;
  maxCacheSize: number;
}

export interface OraclePrediction {
  testCaseId: string;
  modelId: string;
  
  // Prediction
  predictedOutput: Record<string, unknown>;
  confidence: number;
  
  // Reasoning
  usedInvariants: string[];
  usedConstraints: string[];
  reasoning: PredictionReasoning[];
  
  // Metadata
  predictionTime: number;
  modelVersion: number;
}

export interface PredictionReasoning {
  field: string;
  reason: string;
  confidence: number;
  evidence: string[];
}

export interface OracleValidation {
  testCaseId: string;
  prediction: OraclePrediction;
  actualOutput: Record<string, unknown>;
  
  // Comparison
  valid: boolean;
  divergences: OracleDivergence[];
  
  // Analysis
  divergenceAnalysis?: DivergenceAnalysis;
}

export interface OracleDivergence {
  field: string;
  predictedValue: unknown;
  actualValue: unknown;
  divergenceType: 'value' | 'type' | 'missing' | 'extra' | 'tolerance';
  severity: 'critical' | 'warning' | 'info';
  withinTolerance: boolean;
  toleranceUsed?: number;
}

export interface DivergenceAnalysis {
  rootCause: string;
  classification: DivergenceClassification;
  recommendations: string[];
  similarDivergences?: string[];
}

export type DivergenceClassification = 
  | 'model_error'        // Model needs retraining
  | 'code_bug'           // Likely bug in modern code
  | 'edge_case'          // Undiscovered edge case
  | 'data_quality'       // Data quality issue
  | 'expected_change'    // Known behavioral change
  | 'tolerance_issue';   // Tolerance needs adjustment

// ============================================================================
// TEST SYNTHESIS TYPES
// ============================================================================

export interface SynthesisConfig {
  targetCoverage: number;
  maxTestCases: number;
  
  // Strategy weights
  boundaryWeight: number;
  partitionWeight: number;
  edgeCaseWeight: number;
  randomWeight: number;
  
  // Constraints
  excludePatterns?: string[];
  focusFields?: string[];
  
  // Quality
  prioritizeCritical: boolean;
  deduplicateSimilar: boolean;
}

export interface SynthesizedTestSuite {
  id: string;
  modelId: string;
  
  // Test cases
  testCases: SynthesizedTestCase[];
  
  // Coverage analysis
  coverageAnalysis: CoverageAnalysis;
  
  // Quality metrics
  qualityMetrics: TestSuiteQuality;
  
  synthesizedAt: Date;
}

export interface SynthesizedTestCase extends TestCase {
  // Oracle-specific fields
  oracleExpectedOutput: Record<string, unknown>;
  predictionConfidence: number;
  synthesisStrategy: SynthesisStrategy;
  synthesisRationale: string;
  
  // Invariants this test verifies
  verifiesInvariants: string[];
  
  // Risk this test mitigates
  riskMitigation?: string;
}

export type SynthesisStrategy = 
  | 'boundary_exploration'
  | 'partition_coverage'
  | 'edge_case_generation'
  | 'invariant_verification'
  | 'constraint_testing'
  | 'random_exploration'
  | 'historical_replay'
  | 'mutation';

export interface CoverageAnalysis {
  inputCoverage: Record<string, FieldCoverage>;
  outputCoverage: Record<string, FieldCoverage>;
  invariantCoverage: number;
  constraintCoverage: number;
  edgeCaseCoverage: number;
  overallCoverage: number;
}

export interface FieldCoverage {
  field: string;
  valueCoverage: number;
  rangeCoverage: number;
  partitionCoverage: number;
  boundaryCoverage: number;
}

export interface TestSuiteQuality {
  diversityScore: number;
  redundancyScore: number;
  riskCoverageScore: number;
  confidenceScore: number;
  estimatedBugFindingProbability: number;
}

// ============================================================================
// CONTINUOUS VALIDATION TYPES
// ============================================================================

export interface ContinuousValidationConfig {
  enabled: boolean;
  
  // Triggers
  runOnEveryBuild: boolean;
  runOnSchedule?: string;  // Cron expression
  runOnDataChange: boolean;
  
  // Thresholds
  failureThreshold: number;
  warningThreshold: number;
  
  // Reporting
  reportingChannel?: string;
  alertOnFailure: boolean;
  alertOnDrift: boolean;
  
  // Auto-actions
  autoRetrainOnDrift: boolean;
  autoGenerateTests: boolean;
}

export interface ValidationRun {
  id: string;
  projectId: string;
  modelId: string;
  
  // Run info
  trigger: 'manual' | 'scheduled' | 'build' | 'data_change';
  startedAt: Date;
  completedAt?: Date;
  status: 'running' | 'completed' | 'failed';
  
  // Results
  totalTests: number;
  passed: number;
  failed: number;
  warnings: number;
  
  // Analysis
  failureAnalysis?: FailureAnalysis;
  driftAnalysis?: DriftAnalysis;
  
  // Artifacts
  reportUrl?: string;
  logsUrl?: string;
}

export interface FailureAnalysis {
  criticalFailures: FailureDetail[];
  commonPatterns: FailurePattern[];
  rootCauseHypotheses: string[];
  recommendedActions: string[];
}

export interface FailureDetail {
  testCaseId: string;
  field: string;
  expected: unknown;
  actual: unknown;
  severity: 'critical' | 'warning';
  analysis: string;
}

export interface FailurePattern {
  pattern: string;
  affectedTests: number;
  affectedFields: string[];
  hypothesis: string;
}

export interface DriftAnalysis {
  driftDetected: boolean;
  driftScore: number;
  
  // What drifted
  driftedFields: FieldDrift[];
  driftedInvariants: InvariantDrift[];
  
  // Recommendations
  recommendations: string[];
  retrainRecommended: boolean;
}

export interface FieldDrift {
  field: string;
  driftType: 'distribution' | 'range' | 'cardinality' | 'null_rate';
  previousValue: unknown;
  currentValue: unknown;
  driftMagnitude: number;
}

export interface InvariantDrift {
  invariantId: string;
  invariantName: string;
  previousHoldRate: number;
  currentHoldRate: number;
  newExceptions?: string[];
}

// ============================================================================
// ORACLE INTERFACE
// ============================================================================

export interface ITestOracle {
  // Model management
  trainModel(data: HistoricalExecution[]): Promise<BehavioralModel>;
  getModel(modelId: string): BehavioralModel | null;
  updateModel(modelId: string, data: HistoricalExecution[]): Promise<BehavioralModel>;
  
  // Prediction
  predict(modelId: string, inputs: Record<string, unknown>): Promise<OraclePrediction>;
  validate(testResult: TestResult): Promise<OracleValidation>;
  
  // Test synthesis
  synthesizeTests(modelId: string, config: SynthesisConfig): Promise<SynthesizedTestSuite>;
  
  // Continuous validation
  runValidation(modelId: string): Promise<ValidationRun>;
  getValidationHistory(modelId: string): ValidationRun[];
  
  // Analysis
  analyzeFailures(validations: OracleValidation[]): FailureAnalysis;
  detectDrift(modelId: string): Promise<DriftAnalysis>;
}

export interface HistoricalExecution {
  id: string;
  timestamp: Date;
  inputs: Record<string, unknown>;
  outputs: Record<string, unknown>;
  executionTimeMs?: number;
  metadata?: Record<string, unknown>;
}
