/**
 * Test Case Synthesizer
 * 
 * Generates comprehensive test suites from behavioral models,
 * targeting high coverage of input space, invariants, and edge cases.
 */

import { randomUUID } from 'crypto';
import type { TestCase } from '../types.js';
import type {
  BehavioralModel,
  SynthesisConfig,
  SynthesizedTestSuite,
  SynthesizedTestCase,
  SynthesisStrategy,
  CoverageAnalysis,
  FieldCoverage,
  TestSuiteQuality,
  FieldSignature,
  LearnedInvariant,
} from './types.js';
import { TestOracle } from './test-oracle.js';

const DEFAULT_SYNTHESIS_CONFIG: Partial<SynthesisConfig> = {
  targetCoverage: 0.8,
  maxTestCases: 100,
  boundaryWeight: 0.3,
  partitionWeight: 0.25,
  edgeCaseWeight: 0.25,
  randomWeight: 0.2,
  prioritizeCritical: true,
  deduplicateSimilar: true,
};

export class TestCaseSynthesizer {
  private oracle: TestOracle;

  constructor(oracle: TestOracle) {
    this.oracle = oracle;
  }

  /**
   * Synthesize a comprehensive test suite from a behavioral model
   */
  synthesize(
    modelId: string,
    config?: Partial<SynthesisConfig>
  ): SynthesizedTestSuite {
    const model = this.oracle.getModel(modelId);
    if (!model) {
      throw new Error(`Model not found: ${modelId}`);
    }

    const synthConfig: SynthesisConfig = {
      ...DEFAULT_SYNTHESIS_CONFIG,
      ...config,
    } as SynthesisConfig;

    const testCases: SynthesizedTestCase[] = [];
    const targetCount = synthConfig.maxTestCases;

    // Calculate test budget for each strategy based on weights
    const totalWeight = synthConfig.boundaryWeight + synthConfig.partitionWeight + 
                       synthConfig.edgeCaseWeight + synthConfig.randomWeight;
    
    const boundaryBudget = Math.floor(targetCount * (synthConfig.boundaryWeight / totalWeight));
    const partitionBudget = Math.floor(targetCount * (synthConfig.partitionWeight / totalWeight));
    const edgeCaseBudget = Math.floor(targetCount * (synthConfig.edgeCaseWeight / totalWeight));
    const randomBudget = targetCount - boundaryBudget - partitionBudget - edgeCaseBudget;

    // Generate boundary tests
    testCases.push(...this.generateBoundaryTests(model, boundaryBudget, synthConfig));

    // Generate partition tests
    testCases.push(...this.generatePartitionTests(model, partitionBudget, synthConfig));

    // Generate edge case tests
    testCases.push(...this.generateEdgeCaseTests(model, edgeCaseBudget, synthConfig));

    // Generate invariant verification tests
    testCases.push(...this.generateInvariantTests(model, Math.floor(targetCount * 0.1)));

    // Generate random exploration tests
    testCases.push(...this.generateRandomTests(model, randomBudget, synthConfig));

    // Deduplicate similar tests
    const dedupedTests = synthConfig.deduplicateSimilar 
      ? this.deduplicateTests(testCases)
      : testCases;

    // Prioritize tests
    const prioritizedTests = this.prioritizeTests(dedupedTests, synthConfig);

    // Calculate coverage
    const coverageAnalysis = this.analyzeCoverage(model, prioritizedTests);

    // Calculate quality metrics
    const qualityMetrics = this.assessQuality(prioritizedTests, coverageAnalysis);

    return {
      id: `suite_${randomUUID()}`,
      modelId,
      testCases: prioritizedTests.slice(0, synthConfig.maxTestCases),
      coverageAnalysis,
      qualityMetrics,
      synthesizedAt: new Date(),
    };
  }

  // ============================================================================
  // BOUNDARY TESTS
  // ============================================================================

  private generateBoundaryTests(
    model: BehavioralModel,
    budget: number,
    config: SynthesisConfig
  ): SynthesizedTestCase[] {
    const tests: SynthesizedTestCase[] = [];
    const testsPerField = Math.ceil(budget / model.inputSignature.length);

    for (const sig of model.inputSignature) {
      if (config.focusFields && !config.focusFields.includes(sig.name)) continue;

      const fieldTests = this.generateFieldBoundaryTests(model, sig, testsPerField);
      tests.push(...fieldTests);
    }

    return tests.slice(0, budget);
  }

  private generateFieldBoundaryTests(
    model: BehavioralModel,
    sig: FieldSignature,
    maxTests: number
  ): SynthesizedTestCase[] {
    const tests: SynthesizedTestCase[] = [];

    if (sig.dataType === 'number') {
      const range = sig.observedRange as { min: number; max: number } | undefined;
      const dist = sig.observedDistribution;

      // At minimum
      if (range?.min !== undefined) {
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: range.min },
          'boundary_exploration',
          `${sig.name} at minimum boundary (${range.min})`,
          [`Range invariant: ${sig.name} >= ${range.min}`]
        ));

        // Just below minimum
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: range.min - 0.001 },
          'boundary_exploration',
          `${sig.name} below minimum boundary`,
          [`Testing out-of-range handling`]
        ));
      }

      // At maximum
      if (range?.max !== undefined) {
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: range.max },
          'boundary_exploration',
          `${sig.name} at maximum boundary (${range.max})`,
          [`Range invariant: ${sig.name} <= ${range.max}`]
        ));

        // Just above maximum
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: range.max + 0.001 },
          'boundary_exploration',
          `${sig.name} above maximum boundary`,
          [`Testing out-of-range handling`]
        ));
      }

      // At zero
      tests.push(this.createSynthesizedTest(
        model,
        { [sig.name]: 0 },
        'boundary_exploration',
        `${sig.name} at zero`,
        [`Testing zero handling`]
      ));

      // At mean ± 2σ (if normal distribution)
      if (dist?.parameters?.mean !== undefined && dist?.parameters?.stdDev !== undefined) {
        const { mean, stdDev } = dist.parameters;
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: mean - 2 * stdDev },
          'boundary_exploration',
          `${sig.name} at -2σ from mean`,
          [`Testing lower statistical boundary`]
        ));
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: mean + 2 * stdDev },
          'boundary_exploration',
          `${sig.name} at +2σ from mean`,
          [`Testing upper statistical boundary`]
        ));
      }

      // Negative value
      if (!range || range.min >= 0) {
        tests.push(this.createSynthesizedTest(
          model,
          { [sig.name]: -1 },
          'boundary_exploration',
          `${sig.name} with negative value`,
          [`Testing negative value handling`]
        ));
      }
    }

    if (sig.dataType === 'string') {
      // Empty string
      tests.push(this.createSynthesizedTest(
        model,
        { [sig.name]: '' },
        'boundary_exploration',
        `${sig.name} as empty string`,
        [`Testing empty string handling`]
      ));

      // Whitespace only
      tests.push(this.createSynthesizedTest(
        model,
        { [sig.name]: '   ' },
        'boundary_exploration',
        `${sig.name} with whitespace only`,
        [`Testing whitespace handling`]
      ));

      // Very long string
      tests.push(this.createSynthesizedTest(
        model,
        { [sig.name]: 'X'.repeat(1000) },
        'boundary_exploration',
        `${sig.name} with very long string`,
        [`Testing long string handling`]
      ));
    }

    return tests.slice(0, maxTests);
  }

  // ============================================================================
  // PARTITION TESTS
  // ============================================================================

  private generatePartitionTests(
    model: BehavioralModel,
    budget: number,
    config: SynthesisConfig
  ): SynthesizedTestCase[] {
    const tests: SynthesizedTestCase[] = [];

    for (const sig of model.inputSignature) {
      if (config.focusFields && !config.focusFields.includes(sig.name)) continue;

      if (sig.dataType === 'number') {
        const partitions = this.computeNumericPartitions(sig);
        for (const partition of partitions) {
          tests.push(this.createSynthesizedTest(
            model,
            { [sig.name]: partition.representative },
            'partition_coverage',
            `${sig.name} in ${partition.name} partition`,
            [`Covering ${partition.description}`]
          ));
        }
      }

      if (sig.dataType === 'string' && sig.observedValues) {
        // Test each observed value (categorical partition)
        for (const value of sig.observedValues.slice(0, 10)) {
          tests.push(this.createSynthesizedTest(
            model,
            { [sig.name]: value },
            'partition_coverage',
            `${sig.name} = ${JSON.stringify(value)}`,
            [`Testing categorical value`]
          ));
        }
      }
    }

    return tests.slice(0, budget);
  }

  private computeNumericPartitions(sig: FieldSignature): NumericPartition[] {
    const partitions: NumericPartition[] = [];
    const range = sig.observedRange as { min: number; max: number } | undefined;
    const dist = sig.observedDistribution;

    if (!range) return partitions;

    const { min, max } = range;
    const span = max - min;

    if (span === 0) {
      partitions.push({
        name: 'single',
        description: `Only observed value`,
        representative: min,
      });
      return partitions;
    }

    // Low partition (0-25%)
    partitions.push({
      name: 'low',
      description: 'Low range (0-25%)',
      representative: min + span * 0.125,
    });

    // Medium-low partition (25-50%)
    partitions.push({
      name: 'medium-low',
      description: 'Medium-low range (25-50%)',
      representative: min + span * 0.375,
    });

    // Medium-high partition (50-75%)
    partitions.push({
      name: 'medium-high',
      description: 'Medium-high range (50-75%)',
      representative: min + span * 0.625,
    });

    // High partition (75-100%)
    partitions.push({
      name: 'high',
      description: 'High range (75-100%)',
      representative: min + span * 0.875,
    });

    // At mean (if available)
    if (dist?.parameters?.mean !== undefined) {
      partitions.push({
        name: 'mean',
        description: 'At statistical mean',
        representative: dist.parameters.mean,
      });
    }

    return partitions;
  }

  // ============================================================================
  // EDGE CASE TESTS
  // ============================================================================

  private generateEdgeCaseTests(
    model: BehavioralModel,
    budget: number,
    _config: SynthesisConfig
  ): SynthesizedTestCase[] {
    const tests: SynthesizedTestCase[] = [];

    // Generate tests from discovered edge cases
    for (const edgeCase of model.edgeCases) {
      if (edgeCase.exampleInputs.length > 0) {
        const inputs = this.fillMissingInputs(model, edgeCase.exampleInputs[0]!);
        tests.push(this.createSynthesizedTest(
          model,
          inputs,
          'edge_case_generation',
          edgeCase.name,
          [`Edge case: ${edgeCase.description}`],
          edgeCase.riskLevel === 'critical' ? 'critical' : 
            edgeCase.riskLevel === 'high' ? 'high' : 'medium',
          edgeCase.riskLevel === 'critical' || edgeCase.riskLevel === 'high'
            ? `Mitigates ${edgeCase.riskLevel} risk: ${edgeCase.trigger.condition}`
            : undefined
        ));
      }
    }

    // Generate combination edge cases (multiple fields at boundaries)
    const numericFields = model.inputSignature.filter(s => s.dataType === 'number');
    if (numericFields.length >= 2) {
      const field1 = numericFields[0]!;
      const field2 = numericFields[1]!;
      const range1 = field1.observedRange as { min: number; max: number } | undefined;
      const range2 = field2.observedRange as { min: number; max: number } | undefined;

      if (range1 && range2) {
        // Both at minimum
        tests.push(this.createSynthesizedTest(
          model,
          { [field1.name]: range1.min, [field2.name]: range2.min },
          'edge_case_generation',
          `Both ${field1.name} and ${field2.name} at minimum`,
          [`Testing combination boundary`],
          'high'
        ));

        // Both at maximum
        tests.push(this.createSynthesizedTest(
          model,
          { [field1.name]: range1.max, [field2.name]: range2.max },
          'edge_case_generation',
          `Both ${field1.name} and ${field2.name} at maximum`,
          [`Testing combination boundary`],
          'high'
        ));
      }
    }

    return tests.slice(0, budget);
  }

  // ============================================================================
  // INVARIANT TESTS
  // ============================================================================

  private generateInvariantTests(
    model: BehavioralModel,
    budget: number
  ): SynthesizedTestCase[] {
    const tests: SynthesizedTestCase[] = [];

    for (const invariant of model.invariants) {
      const testInputs = this.generateInputsForInvariant(model, invariant);
      if (testInputs) {
        tests.push(this.createSynthesizedTest(
          model,
          testInputs,
          'invariant_verification',
          `Verify: ${invariant.name}`,
          [invariant.id],
          'medium',
          undefined,
          [invariant.id]
        ));
      }
    }

    return tests.slice(0, budget);
  }

  private generateInputsForInvariant(
    model: BehavioralModel,
    invariant: LearnedInvariant
  ): Record<string, unknown> | null {
    const inputs: Record<string, unknown> = {};

    // For equality invariants, use typical values
    if (invariant.type === 'equality' || invariant.type === 'formula') {
      for (const fieldName of invariant.inputFields) {
        const sig = model.inputSignature.find(s => s.name === fieldName);
        if (sig) {
          inputs[fieldName] = this.getTypicalValue(sig);
        }
      }
    }

    // For conditional invariants, ensure condition is met
    if (invariant.type === 'conditional') {
      const match = invariant.expression.match(/input\.(\w+)\s*==\s*"([^"]+)"/);
      if (match) {
        const [, field, value] = match;
        inputs[field!] = value;
      }
    }

    // Fill in remaining fields
    return this.fillMissingInputs(model, inputs);
  }

  // ============================================================================
  // RANDOM TESTS
  // ============================================================================

  private generateRandomTests(
    model: BehavioralModel,
    budget: number,
    config: SynthesisConfig
  ): SynthesizedTestCase[] {
    const tests: SynthesizedTestCase[] = [];

    for (let i = 0; i < budget; i++) {
      const inputs: Record<string, unknown> = {};

      for (const sig of model.inputSignature) {
        if (config.excludePatterns?.some(p => sig.name.match(p))) continue;
        inputs[sig.name] = this.generateRandomValue(sig);
      }

      tests.push(this.createSynthesizedTest(
        model,
        inputs,
        'random_exploration',
        `Random exploration test ${i + 1}`,
        [`Exploring input space randomly`],
        'low'
      ));
    }

    return tests;
  }

  private generateRandomValue(sig: FieldSignature): unknown {
    const range = sig.observedRange as { min: number; max: number } | undefined;
    const dist = sig.observedDistribution;

    switch (sig.dataType) {
      case 'number': {
        if (range) {
          return range.min + Math.random() * (range.max - range.min);
        }
        if (dist?.parameters?.mean !== undefined && dist?.parameters?.stdDev !== undefined) {
          // Generate from normal distribution
          return this.randomNormal(dist.parameters.mean, dist.parameters.stdDev);
        }
        return Math.random() * 1000;
      }
      case 'string': {
        if (sig.observedValues && sig.observedValues.length > 0) {
          const index = Math.floor(Math.random() * sig.observedValues.length);
          return sig.observedValues[index];
        }
        return `random_${randomUUID().slice(0, 8)}`;
      }
      case 'boolean':
        return Math.random() > 0.5;
      case 'date':
        return new Date(Date.now() - Math.random() * 365 * 24 * 60 * 60 * 1000);
      default:
        return null;
    }
  }

  private randomNormal(mean: number, stdDev: number): number {
    // Box-Muller transform
    const u1 = Math.random();
    const u2 = Math.random();
    const z = Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
    return mean + z * stdDev;
  }

  // ============================================================================
  // HELPERS
  // ============================================================================

  private createSynthesizedTest(
    model: BehavioralModel,
    inputs: Record<string, unknown>,
    strategy: SynthesisStrategy,
    rationale: string,
    _invariants: string[] = [],
    priority: TestCase['priority'] = 'medium',
    riskMitigation?: string,
    verifiesInvariants: string[] = []
  ): SynthesizedTestCase {
    // Fill in any missing inputs
    const completeInputs = this.fillMissingInputs(model, inputs);
    
    // Get oracle prediction
    const prediction = this.oracle.predict(model.id, completeInputs);

    return {
      id: `tc_${randomUUID()}`,
      name: rationale.slice(0, 100),
      description: rationale,
      inputs: completeInputs,
      expectedOutput: prediction.predictedOutput,
      tolerance: { numeric: 0.001, string: 'exact' },
      tags: [strategy],
      generationStrategy: strategy === 'random_exploration' ? 'random' : 
                         strategy === 'boundary_exploration' ? 'boundary' :
                         strategy === 'partition_coverage' ? 'equivalence-partition' :
                         strategy === 'historical_replay' ? 'historical-replay' :
                         'property-based',
      priority,
      oracleExpectedOutput: prediction.predictedOutput,
      predictionConfidence: prediction.confidence,
      synthesisStrategy: strategy,
      synthesisRationale: rationale,
      verifiesInvariants,
      riskMitigation,
    };
  }

  private fillMissingInputs(
    model: BehavioralModel,
    partialInputs: Record<string, unknown>
  ): Record<string, unknown> {
    const complete: Record<string, unknown> = { ...partialInputs };

    for (const sig of model.inputSignature) {
      if (complete[sig.name] === undefined) {
        complete[sig.name] = this.getTypicalValue(sig);
      }
    }

    return complete;
  }

  private getTypicalValue(sig: FieldSignature): unknown {
    const dist = sig.observedDistribution;
    
    if (sig.dataType === 'number') {
      if (dist?.parameters?.mean !== undefined) {
        return dist.parameters.mean;
      }
      const range = sig.observedRange as { min: number; max: number } | undefined;
      if (range) {
        return (range.min + range.max) / 2;
      }
      return 0;
    }

    if (sig.dataType === 'string') {
      if (sig.observedValues && sig.observedValues.length > 0) {
        return sig.observedValues[0];
      }
      return '';
    }

    if (sig.dataType === 'boolean') {
      return false;
    }

    if (sig.dataType === 'date') {
      return new Date();
    }

    return null;
  }

  private deduplicateTests(tests: SynthesizedTestCase[]): SynthesizedTestCase[] {
    const seen = new Set<string>();
    const unique: SynthesizedTestCase[] = [];

    for (const test of tests) {
      const key = JSON.stringify(test.inputs);
      if (!seen.has(key)) {
        seen.add(key);
        unique.push(test);
      }
    }

    return unique;
  }

  private prioritizeTests(
    tests: SynthesizedTestCase[],
    config: SynthesisConfig
  ): SynthesizedTestCase[] {
    if (!config.prioritizeCritical) return tests;

    return tests.sort((a, b) => {
      const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
      const aPriority = priorityOrder[a.priority];
      const bPriority = priorityOrder[b.priority];
      
      if (aPriority !== bPriority) return aPriority - bPriority;
      
      // Secondary sort by prediction confidence (higher first)
      return b.predictionConfidence - a.predictionConfidence;
    });
  }

  // ============================================================================
  // COVERAGE ANALYSIS
  // ============================================================================

  private analyzeCoverage(
    model: BehavioralModel,
    tests: SynthesizedTestCase[]
  ): CoverageAnalysis {
    const inputCoverage: Record<string, FieldCoverage> = {};
    const outputCoverage: Record<string, FieldCoverage> = {};

    // Analyze input coverage
    for (const sig of model.inputSignature) {
      inputCoverage[sig.name] = this.analyzeFieldCoverage(sig, tests, 'inputs');
    }

    // Analyze output coverage
    for (const sig of model.outputSignature) {
      outputCoverage[sig.name] = this.analyzeFieldCoverage(sig, tests, 'outputs');
    }

    // Invariant coverage
    const testedInvariants = new Set<string>();
    for (const test of tests) {
      for (const invId of test.verifiesInvariants) {
        testedInvariants.add(invId);
      }
    }
    const invariantCoverage = model.invariants.length > 0
      ? testedInvariants.size / model.invariants.length
      : 1.0;

    // Constraint coverage (estimate from input coverage)
    const constraintCoverage = Object.values(inputCoverage)
      .reduce((sum, c) => sum + c.valueCoverage, 0) / Object.keys(inputCoverage).length;

    // Edge case coverage
    const edgeCaseTests = tests.filter(t => t.synthesisStrategy === 'edge_case_generation');
    const edgeCaseCoverage = model.edgeCases.length > 0
      ? Math.min(edgeCaseTests.length / model.edgeCases.length, 1.0)
      : 1.0;

    // Overall coverage
    const overallCoverage = (
      Object.values(inputCoverage).reduce((sum, c) => sum + c.valueCoverage, 0) / Object.keys(inputCoverage).length * 0.4 +
      invariantCoverage * 0.3 +
      edgeCaseCoverage * 0.3
    );

    return {
      inputCoverage,
      outputCoverage,
      invariantCoverage,
      constraintCoverage,
      edgeCaseCoverage,
      overallCoverage,
    };
  }

  private analyzeFieldCoverage(
    sig: FieldSignature,
    tests: SynthesizedTestCase[],
    fieldType: 'inputs' | 'outputs'
  ): FieldCoverage {
    const values = tests.map(t => 
      fieldType === 'inputs' ? t.inputs[sig.name] : t.oracleExpectedOutput[sig.name]
    );

    const uniqueValues = new Set(values.map(v => JSON.stringify(v)));
    const cardinality = sig.cardinality || uniqueValues.size;
    const valueCoverage = cardinality > 0 ? uniqueValues.size / cardinality : 1.0;

    let rangeCoverage = 1.0;
    let partitionCoverage = 0;
    let boundaryCoverage = 0;

    if (sig.dataType === 'number') {
      const range = sig.observedRange as { min: number; max: number } | undefined;
      if (range) {
        const numValues = values.filter(v => typeof v === 'number') as number[];
        if (numValues.length > 0) {
          const minVal = Math.min(...numValues);
          const maxVal = Math.max(...numValues);
          const span = range.max - range.min;
          rangeCoverage = span > 0 ? (maxVal - minVal) / span : 1.0;

          // Check partition coverage (how many quartiles are covered)
          const quartiles = [0.25, 0.5, 0.75].map(q => range.min + q * span);
          const coveredQuartiles = quartiles.filter(q => 
            numValues.some(v => Math.abs(v - q) < span * 0.1)
          );
          partitionCoverage = coveredQuartiles.length / quartiles.length;

          // Check boundary coverage
          const atMin = numValues.some(v => Math.abs(v - range.min) < span * 0.05);
          const atMax = numValues.some(v => Math.abs(v - range.max) < span * 0.05);
          boundaryCoverage = (atMin ? 0.5 : 0) + (atMax ? 0.5 : 0);
        }
      }
    }

    return {
      field: sig.name,
      valueCoverage: Math.min(valueCoverage, 1.0),
      rangeCoverage,
      partitionCoverage,
      boundaryCoverage,
    };
  }

  // ============================================================================
  // QUALITY ASSESSMENT
  // ============================================================================

  private assessQuality(
    tests: SynthesizedTestCase[],
    coverage: CoverageAnalysis
  ): TestSuiteQuality {
    // Diversity score - how varied are the test inputs
    const strategies = new Set(tests.map(t => t.synthesisStrategy));
    const diversityScore = strategies.size / 6; // 6 possible strategies

    // Redundancy score - inverse of duplicate similarity
    const uniqueInputs = new Set(tests.map(t => JSON.stringify(t.inputs)));
    const redundancyScore = uniqueInputs.size / tests.length;

    // Risk coverage score - how many high/critical tests
    const criticalTests = tests.filter(t => t.priority === 'critical' || t.priority === 'high');
    const riskCoverageScore = Math.min(criticalTests.length / (tests.length * 0.3), 1.0);

    // Confidence score - average prediction confidence
    const confidenceScore = tests.length > 0
      ? tests.reduce((sum, t) => sum + t.predictionConfidence, 0) / tests.length
      : 0;

    // Estimated bug-finding probability (heuristic)
    const estimatedBugFindingProbability = (
      coverage.overallCoverage * 0.3 +
      coverage.edgeCaseCoverage * 0.3 +
      diversityScore * 0.2 +
      riskCoverageScore * 0.2
    );

    return {
      diversityScore,
      redundancyScore,
      riskCoverageScore,
      confidenceScore,
      estimatedBugFindingProbability,
    };
  }
}

interface NumericPartition {
  name: string;
  description: string;
  representative: number;
}
