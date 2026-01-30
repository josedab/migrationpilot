/**
 * Regression Test Generator
 * 
 * Generates comprehensive test suites from production logs and historical data.
 * Creates golden datasets for equivalence testing during migration.
 */

import type { 
  TestCase, 
  TestGenerationStrategy, 
  TestBusinessRule,
  ExecutionTrace,
  ToleranceConfig,
} from './types.js';
import { randomUUID } from 'crypto';

export interface ProductionLogEntry {
  timestamp: string;
  transactionId: string;
  programName: string;
  inputs: Record<string, unknown>;
  outputs: Record<string, unknown>;
  executionTimeMs: number;
  metadata?: {
    userId?: string;
    batchId?: string;
    environment?: string;
    [key: string]: unknown;
  };
}

export interface DataAnonymizationConfig {
  fields: {
    [fieldPattern: string]: AnonymizationStrategy;
  };
  preserveFormat: boolean;
  seed?: number;
}

export type AnonymizationStrategy = 
  | 'hash'
  | 'mask'
  | 'replace'
  | 'tokenize'
  | 'generalize'
  | 'synthetic';

export interface GoldenDataset {
  id: string;
  name: string;
  description: string;
  projectId: string;
  createdAt: string;
  updatedAt: string;
  version: number;
  testCases: TestCase[];
  sourceMetadata: {
    logCount: number;
    dateRange: { start: string; end: string };
    uniqueInputPatterns: number;
    anonymized: boolean;
  };
  statistics: {
    totalCases: number;
    byPriority: Record<string, number>;
    byStrategy: Record<string, number>;
    inputCoverage: number;
    outputCoverage: number;
  };
}

export interface RegressionTestConfig {
  maxTestCases?: number;
  minCoverage?: number;
  includeStrategies?: TestGenerationStrategy[];
  excludeStrategies?: TestGenerationStrategy[];
  priorityThreshold?: 'critical' | 'high' | 'medium' | 'low';
  anonymization?: DataAnonymizationConfig;
  tolerance?: ToleranceConfig;
  deduplication?: boolean;
  diversityBias?: number; // 0-1, higher = more diverse test cases
}

export class RegressionTestGenerator {
  private defaultConfig: Required<RegressionTestConfig> = {
    maxTestCases: 1000,
    minCoverage: 0.8,
    includeStrategies: ['historical-replay', 'boundary', 'equivalence-partition', 'property-based'],
    excludeStrategies: [],
    priorityThreshold: 'low',
    anonymization: {
      fields: {},
      preserveFormat: true,
    },
    tolerance: {
      numeric: 0.001,
      string: 'exact',
      date: 1000,
      array: 'ordered',
    },
    deduplication: true,
    diversityBias: 0.5,
  };

  /**
   * Generate test cases from production logs
   */
  generateFromLogs(
    logs: ProductionLogEntry[],
    rules: TestBusinessRule[],
    config: RegressionTestConfig = {}
  ): GoldenDataset {
    const mergedConfig = { ...this.defaultConfig, ...config };
    
    // Step 1: Anonymize sensitive data
    const anonymizedLogs = mergedConfig.anonymization.fields && Object.keys(mergedConfig.anonymization.fields).length > 0
      ? this.anonymizeLogs(logs, mergedConfig.anonymization)
      : logs;
    
    // Step 2: Extract unique input/output patterns
    const patterns = this.extractPatterns(anonymizedLogs);
    
    // Step 3: Generate historical replay tests
    const historicalTests = this.generateHistoricalReplayTests(
      anonymizedLogs,
      rules,
      mergedConfig
    );
    
    // Step 4: Generate boundary tests based on observed data ranges
    const boundaryTests = this.generateDataDrivenBoundaryTests(
      patterns,
      rules,
      mergedConfig
    );
    
    // Step 5: Generate partition tests based on observed clusters
    const partitionTests = this.generateClusterBasedPartitionTests(
      patterns,
      rules,
      mergedConfig
    );
    
    // Step 6: Deduplicate and prioritize
    let allTests = [...historicalTests, ...boundaryTests, ...partitionTests];
    
    if (mergedConfig.deduplication) {
      allTests = this.deduplicateTests(allTests);
    }
    
    // Step 7: Apply diversity selection
    allTests = this.selectDiverseTests(
      allTests,
      mergedConfig.maxTestCases,
      mergedConfig.diversityBias
    );
    
    // Step 8: Build golden dataset
    return this.buildGoldenDataset(
      allTests,
      anonymizedLogs,
      rules,
      mergedConfig
    );
  }

  /**
   * Generate tests from execution traces
   */
  generateFromTraces(
    traces: ExecutionTrace[],
    rules: TestBusinessRule[],
    config: RegressionTestConfig = {}
  ): TestCase[] {
    const mergedConfig = { ...this.defaultConfig, ...config };
    const tests: TestCase[] = [];

    for (const trace of traces) {
      // Create replay test from trace
      tests.push({
        id: `tc_trace_${randomUUID()}`,
        name: `Trace replay - ${trace.sessionId}`,
        description: `Replay execution trace from ${trace.systemType} system`,
        inputs: trace.inputs,
        expectedOutput: trace.output,
        tolerance: mergedConfig.tolerance,
        tags: ['trace-replay', trace.systemType],
        generationStrategy: 'historical-replay',
        priority: 'high',
      });

      // Generate variation tests based on trace events
      const variationTests = this.generateTraceVariations(trace, rules);
      tests.push(...variationTests);
    }

    return tests;
  }

  /**
   * Generate tests from a CSV or batch file of historical data
   */
  generateFromBatchData(
    data: Array<{ inputs: Record<string, unknown>; outputs: Record<string, unknown> }>,
    _rules: TestBusinessRule[],
    config: RegressionTestConfig = {}
  ): TestCase[] {
    const mergedConfig = { ...this.defaultConfig, ...config };
    const tests: TestCase[] = [];

    for (let i = 0; i < data.length && i < mergedConfig.maxTestCases; i++) {
      const record = data[i];
      if (!record) continue;
      
      tests.push({
        id: `tc_batch_${randomUUID()}`,
        name: `Batch test case ${i + 1}`,
        description: `Historical test case from batch data`,
        inputs: record.inputs,
        expectedOutput: record.outputs,
        tolerance: mergedConfig.tolerance,
        tags: ['batch-data', 'historical'],
        generationStrategy: 'historical-replay',
        priority: 'medium',
      });
    }

    return tests;
  }

  /**
   * Ingest JCL job output logs and extract test cases
   */
  generateFromJCLLogs(
    jclOutput: string,
    programName: string,
    _rules: TestBusinessRule[],
    config: RegressionTestConfig = {}
  ): TestCase[] {
    const mergedConfig = { ...this.defaultConfig, ...config };
    const tests: TestCase[] = [];

    // Parse JCL output format (simplified - real implementation would be more complex)
    const runs = this.parseJCLOutput(jclOutput);

    for (const run of runs) {
      if (run.programName === programName || !programName) {
        tests.push({
          id: `tc_jcl_${randomUUID()}`,
          name: `JCL job ${run.jobId} - step ${run.stepName}`,
          description: `Test case from JCL job execution`,
          inputs: run.inputs,
          expectedOutput: run.outputs,
          tolerance: mergedConfig.tolerance,
          tags: ['jcl', run.jobId, run.stepName],
          generationStrategy: 'historical-replay',
          priority: 'high',
        });
      }
    }

    return tests;
  }

  /**
   * Update golden dataset with drift detection
   */
  detectDrift(
    currentDataset: GoldenDataset,
    newLogs: ProductionLogEntry[],
    threshold: number = 0.1
  ): {
    hasDrift: boolean;
    newPatterns: string[];
    removedPatterns: string[];
    changedBehaviors: Array<{
      testCase: TestCase;
      oldOutput: unknown;
      newOutput: unknown;
    }>;
    recommendation: 'update' | 'review' | 'keep';
  } {
    const currentPatterns = new Set(
      currentDataset.testCases.map(tc => this.hashInputs(tc.inputs))
    );
    
    const newPatterns: string[] = [];
    const newOutputMap = new Map<string, unknown>();

    for (const log of newLogs) {
      const hash = this.hashInputs(log.inputs);
      if (!currentPatterns.has(hash)) {
        newPatterns.push(hash);
      }
      newOutputMap.set(hash, log.outputs);
    }

    const changedBehaviors: Array<{
      testCase: TestCase;
      oldOutput: unknown;
      newOutput: unknown;
    }> = [];

    for (const tc of currentDataset.testCases) {
      const hash = this.hashInputs(tc.inputs);
      const newOutput = newOutputMap.get(hash);
      if (newOutput && JSON.stringify(newOutput) !== JSON.stringify(tc.expectedOutput)) {
        changedBehaviors.push({
          testCase: tc,
          oldOutput: tc.expectedOutput,
          newOutput,
        });
      }
    }

    const driftRatio = (newPatterns.length + changedBehaviors.length) / 
      (currentDataset.testCases.length || 1);
    
    const hasDrift = driftRatio > threshold;

    return {
      hasDrift,
      newPatterns,
      removedPatterns: [], // Would need to track what's no longer seen
      changedBehaviors,
      recommendation: driftRatio > 0.3 ? 'update' : driftRatio > threshold ? 'review' : 'keep',
    };
  }

  // Private helper methods

  private anonymizeLogs(
    logs: ProductionLogEntry[],
    config: DataAnonymizationConfig
  ): ProductionLogEntry[] {
    return logs.map(log => ({
      ...log,
      inputs: this.anonymizeRecord(log.inputs, config),
      outputs: this.anonymizeRecord(log.outputs, config),
      metadata: log.metadata ? this.anonymizeRecord(log.metadata, config) : undefined,
    }));
  }

  private anonymizeRecord(
    record: Record<string, unknown>,
    config: DataAnonymizationConfig
  ): Record<string, unknown> {
    const result: Record<string, unknown> = {};

    for (const [key, value] of Object.entries(record)) {
      let strategy: AnonymizationStrategy | undefined;
      
      // Find matching anonymization rule
      for (const [pattern, s] of Object.entries(config.fields)) {
        if (key.match(new RegExp(pattern, 'i'))) {
          strategy = s;
          break;
        }
      }

      if (strategy) {
        result[key] = this.anonymizeValue(value, strategy, config.preserveFormat);
      } else {
        result[key] = value;
      }
    }

    return result;
  }

  private anonymizeValue(
    value: unknown,
    strategy: AnonymizationStrategy,
    preserveFormat: boolean
  ): unknown {
    if (value === null || value === undefined) return value;
    
    const strValue = String(value);

    switch (strategy) {
      case 'hash':
        return this.simpleHash(strValue);
      
      case 'mask':
        if (preserveFormat) {
          return strValue.replace(/[a-zA-Z]/g, 'X').replace(/[0-9]/g, '9');
        }
        return '***MASKED***';
      
      case 'replace':
        return 'REDACTED';
      
      case 'tokenize':
        return `TOKEN_${this.simpleHash(strValue).substring(0, 8)}`;
      
      case 'generalize':
        // For numbers, generalize to ranges
        if (typeof value === 'number') {
          return Math.floor(value / 100) * 100;
        }
        // For strings, keep first char only
        return strValue.charAt(0) + '***';
      
      case 'synthetic':
        // Generate synthetic data of same type
        if (typeof value === 'number') {
          return Math.random() * 10000;
        }
        return `synth_${Math.random().toString(36).substring(7)}`;
      
      default:
        return value;
    }
  }

  private simpleHash(value: string): string {
    let hash = 0;
    for (let i = 0; i < value.length; i++) {
      const char = value.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash;
    }
    return Math.abs(hash).toString(16).padStart(8, '0');
  }

  private extractPatterns(logs: ProductionLogEntry[]): {
    inputRanges: Map<string, { min: number; max: number; values: Set<string> }>;
    outputRanges: Map<string, { min: number; max: number; values: Set<string> }>;
    correlations: Array<{ input: string; output: string; correlation: number }>;
  } {
    const inputRanges = new Map<string, { min: number; max: number; values: Set<string> }>();
    const outputRanges = new Map<string, { min: number; max: number; values: Set<string> }>();

    for (const log of logs) {
      this.updateRanges(log.inputs, inputRanges);
      this.updateRanges(log.outputs, outputRanges);
    }

    return {
      inputRanges,
      outputRanges,
      correlations: [], // Would need statistical analysis
    };
  }

  private updateRanges(
    record: Record<string, unknown>,
    ranges: Map<string, { min: number; max: number; values: Set<string> }>
  ): void {
    for (const [key, value] of Object.entries(record)) {
      const existing = ranges.get(key) || { min: Infinity, max: -Infinity, values: new Set() };
      
      if (typeof value === 'number') {
        existing.min = Math.min(existing.min, value);
        existing.max = Math.max(existing.max, value);
      }
      existing.values.add(String(value));
      
      ranges.set(key, existing);
    }
  }

  private generateHistoricalReplayTests(
    logs: ProductionLogEntry[],
    rules: TestBusinessRule[],
    config: Required<RegressionTestConfig>
  ): TestCase[] {
    const tests: TestCase[] = [];
    const seenInputs = new Set<string>();

    // Sort by timestamp to get recent cases
    const sortedLogs = [...logs].sort((a, b) => 
      new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
    );

    for (const log of sortedLogs) {
      const inputHash = this.hashInputs(log.inputs);
      
      // Skip duplicates if dedup enabled
      if (config.deduplication && seenInputs.has(inputHash)) {
        continue;
      }
      seenInputs.add(inputHash);

      // Find matching rule
      const matchingRule = this.findMatchingRule(log, rules);

      tests.push({
        id: `tc_hist_${randomUUID()}`,
        name: `Historical: ${log.programName} - ${log.transactionId}`,
        description: `Replay of production transaction from ${log.timestamp}`,
        inputs: log.inputs,
        expectedOutput: log.outputs,
        tolerance: config.tolerance,
        tags: ['historical-replay', log.programName],
        sourceRule: matchingRule?.id,
        generationStrategy: 'historical-replay',
        priority: this.determinePriority(log, matchingRule),
      });

      if (tests.length >= config.maxTestCases / 2) {
        break; // Reserve space for other strategies
      }
    }

    return tests;
  }

  private generateDataDrivenBoundaryTests(
    patterns: ReturnType<typeof this.extractPatterns>,
    _rules: TestBusinessRule[],
    config: Required<RegressionTestConfig>
  ): TestCase[] {
    const tests: TestCase[] = [];

    for (const [inputName, range] of patterns.inputRanges) {
      if (range.min !== Infinity && range.max !== -Infinity) {
        // Test at observed boundaries
        tests.push({
          id: `tc_bound_min_${randomUUID()}`,
          name: `Boundary: ${inputName} at observed minimum`,
          description: `Test with ${inputName} at minimum observed value (${range.min})`,
          inputs: { [inputName]: range.min },
          tolerance: config.tolerance,
          tags: ['boundary', 'data-driven'],
          generationStrategy: 'boundary',
          priority: 'high',
        });

        tests.push({
          id: `tc_bound_max_${randomUUID()}`,
          name: `Boundary: ${inputName} at observed maximum`,
          description: `Test with ${inputName} at maximum observed value (${range.max})`,
          inputs: { [inputName]: range.max },
          tolerance: config.tolerance,
          tags: ['boundary', 'data-driven'],
          generationStrategy: 'boundary',
          priority: 'high',
        });

        // Just beyond observed boundaries
        tests.push({
          id: `tc_bound_beyond_max_${randomUUID()}`,
          name: `Boundary: ${inputName} beyond observed maximum`,
          description: `Test with ${inputName} slightly above maximum (${range.max * 1.1})`,
          inputs: { [inputName]: range.max * 1.1 },
          tolerance: config.tolerance,
          tags: ['boundary', 'extrapolation'],
          generationStrategy: 'boundary',
          priority: 'medium',
        });
      }
    }

    return tests;
  }

  private generateClusterBasedPartitionTests(
    patterns: ReturnType<typeof this.extractPatterns>,
    _rules: TestBusinessRule[],
    config: Required<RegressionTestConfig>
  ): TestCase[] {
    const tests: TestCase[] = [];

    // For each input, generate representative values from observed data
    for (const [inputName, range] of patterns.inputRanges) {
      const values = Array.from(range.values).slice(0, 10); // Take first 10 unique values
      
      for (const value of values) {
        tests.push({
          id: `tc_part_${randomUUID()}`,
          name: `Partition: ${inputName} = ${value}`,
          description: `Test with observed ${inputName} value`,
          inputs: { [inputName]: this.parseValue(value) },
          tolerance: config.tolerance,
          tags: ['partition', 'data-driven'],
          generationStrategy: 'equivalence-partition',
          priority: 'medium',
        });
      }
    }

    return tests;
  }

  private generateTraceVariations(
    trace: ExecutionTrace,
    _rules: TestBusinessRule[]
  ): TestCase[] {
    const tests: TestCase[] = [];
    
    // Generate variations based on branch events in trace
    const branchEvents = trace.events.filter(e => e.type === 'branch');
    
    for (const branch of branchEvents) {
      // Create test case that exercises alternate branch
      tests.push({
        id: `tc_variation_${randomUUID()}`,
        name: `Trace variation - alternate branch at ${branch.location}`,
        description: `Variation to exercise different branch at ${branch.location}`,
        inputs: this.mutateInputsForBranch(trace.inputs, branch),
        tolerance: { numeric: 0.001, string: 'exact' },
        tags: ['trace-variation', 'branch-coverage'],
        generationStrategy: 'property-based',
        priority: 'medium',
      });
    }

    return tests;
  }

  private parseJCLOutput(jclOutput: string): Array<{
    jobId: string;
    stepName: string;
    programName: string;
    inputs: Record<string, unknown>;
    outputs: Record<string, unknown>;
  }> {
    const runs: Array<{
      jobId: string;
      stepName: string;
      programName: string;
      inputs: Record<string, unknown>;
      outputs: Record<string, unknown>;
    }> = [];

    // Simplified JCL parsing - real implementation would handle actual JCL output format
    const jobPattern = /\/\/(\w+)\s+JOB/g;
    const stepPattern = /\/\/(\w+)\s+EXEC\s+PGM=(\w+)/g;

    let jobMatch: RegExpExecArray | null;
    let currentJobId = 'UNKNOWN';
    
    while ((jobMatch = jobPattern.exec(jclOutput)) !== null) {
      currentJobId = jobMatch[1] ?? 'UNKNOWN';
    }

    let stepMatch: RegExpExecArray | null;
    while ((stepMatch = stepPattern.exec(jclOutput)) !== null) {
      runs.push({
        jobId: currentJobId,
        stepName: stepMatch[1] ?? 'UNKNOWN',
        programName: stepMatch[2] ?? 'UNKNOWN',
        inputs: {}, // Would parse from SYSIN
        outputs: {}, // Would parse from SYSOUT
      });
    }

    return runs;
  }

  private deduplicateTests(tests: TestCase[]): TestCase[] {
    const seen = new Map<string, TestCase>();
    
    for (const test of tests) {
      const hash = this.hashInputs(test.inputs);
      const existing = seen.get(hash);
      
      // Keep higher priority test
      if (!existing || this.comparePriority(test.priority, existing.priority) > 0) {
        seen.set(hash, test);
      }
    }

    return Array.from(seen.values());
  }

  private selectDiverseTests(
    tests: TestCase[],
    maxCount: number,
    diversityBias: number
  ): TestCase[] {
    if (tests.length <= maxCount) return tests;

    // Sort by priority first
    const sorted = [...tests].sort((a, b) => 
      this.comparePriority(b.priority, a.priority)
    );

    // Select diverse subset
    const selected: TestCase[] = [];
    const selectedHashes = new Set<string>();

    // First pass: select by priority
    const priorityCount = Math.floor(maxCount * (1 - diversityBias));
    for (const test of sorted) {
      if (selected.length >= priorityCount) break;
      selected.push(test);
      selectedHashes.add(this.hashInputs(test.inputs));
    }

    // Second pass: maximize diversity
    const remaining = sorted.filter(t => !selectedHashes.has(this.hashInputs(t.inputs)));
    
    while (selected.length < maxCount && remaining.length > 0) {
      // Simple diversity: pick test with most different inputs from selected
      let maxDiversity = -1;
      let mostDiverse: TestCase | null = null;
      let mostDiverseIndex = -1;

      for (let i = 0; i < remaining.length; i++) {
        const test = remaining[i];
        if (!test) continue;
        const diversity = this.calculateDiversity(test, selected);
        if (diversity > maxDiversity) {
          maxDiversity = diversity;
          mostDiverse = test;
          mostDiverseIndex = i;
        }
      }

      if (mostDiverse) {
        selected.push(mostDiverse);
        remaining.splice(mostDiverseIndex, 1);
      } else {
        break;
      }
    }

    return selected;
  }

  private buildGoldenDataset(
    tests: TestCase[],
    logs: ProductionLogEntry[],
    rules: TestBusinessRule[],
    config: Required<RegressionTestConfig>
  ): GoldenDataset {
    const byPriority: Record<string, number> = {};
    const byStrategy: Record<string, number> = {};

    for (const test of tests) {
      byPriority[test.priority] = (byPriority[test.priority] || 0) + 1;
      byStrategy[test.generationStrategy] = (byStrategy[test.generationStrategy] || 0) + 1;
    }

    const timestamps = logs.map(l => new Date(l.timestamp).getTime());
    const minTime = timestamps.length > 0 ? Math.min(...timestamps) : Date.now();
    const maxTime = timestamps.length > 0 ? Math.max(...timestamps) : Date.now();

    return {
      id: `gd_${randomUUID()}`,
      name: `Golden Dataset - ${new Date().toISOString().split('T')[0]}`,
      description: `Auto-generated from ${logs.length} production logs`,
      projectId: '',
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
      version: 1,
      testCases: tests,
      sourceMetadata: {
        logCount: logs.length,
        dateRange: {
          start: new Date(minTime).toISOString(),
          end: new Date(maxTime).toISOString(),
        },
        uniqueInputPatterns: new Set(logs.map(l => this.hashInputs(l.inputs))).size,
        anonymized: Object.keys(config.anonymization.fields).length > 0,
      },
      statistics: {
        totalCases: tests.length,
        byPriority,
        byStrategy,
        inputCoverage: this.calculateInputCoverage(tests, rules),
        outputCoverage: this.calculateOutputCoverage(tests, rules),
      },
    };
  }

  private hashInputs(inputs: Record<string, unknown>): string {
    return this.simpleHash(JSON.stringify(inputs));
  }

  private findMatchingRule(
    log: ProductionLogEntry,
    rules: TestBusinessRule[]
  ): TestBusinessRule | undefined {
    // Simple matching by input names
    const inputNames = Object.keys(log.inputs);
    
    return rules.find(rule => {
      const ruleInputs = rule.inputs?.map(i => i.name) || [];
      return inputNames.some(name => ruleInputs.includes(name));
    });
  }

  private determinePriority(
    log: ProductionLogEntry,
    rule: TestBusinessRule | undefined
  ): TestCase['priority'] {
    if (rule && rule.confidence < 0.7) return 'critical';
    if (log.metadata?.environment === 'production') return 'high';
    return 'medium';
  }

  private comparePriority(a: TestCase['priority'], b: TestCase['priority']): number {
    const order = { critical: 4, high: 3, medium: 2, low: 1 };
    return order[a] - order[b];
  }

  private calculateDiversity(test: TestCase, selected: TestCase[]): number {
    if (selected.length === 0) return 1;
    
    let totalDifference = 0;
    for (const s of selected) {
      totalDifference += this.inputDifference(test.inputs, s.inputs);
    }
    
    return totalDifference / selected.length;
  }

  private inputDifference(a: Record<string, unknown>, b: Record<string, unknown>): number {
    const keysA = new Set(Object.keys(a));
    const keysB = new Set(Object.keys(b));
    const allKeys = new Set([...keysA, ...keysB]);
    
    let differences = 0;
    for (const key of allKeys) {
      if (a[key] !== b[key]) differences++;
    }
    
    return differences / allKeys.size;
  }

  private mutateInputsForBranch(
    inputs: Record<string, unknown>,
    _branch: { data: unknown }
  ): Record<string, unknown> {
    // Simple mutation - flip boolean values, adjust numbers slightly
    const mutated = { ...inputs };
    
    for (const [key, value] of Object.entries(mutated)) {
      if (typeof value === 'boolean') {
        mutated[key] = !value;
        break; // Mutate one thing at a time
      } else if (typeof value === 'number') {
        mutated[key] = value === 0 ? 1 : value * -1;
        break;
      }
    }
    
    return mutated;
  }

  private parseValue(value: string): unknown {
    // Try to parse as number
    const num = parseFloat(value);
    if (!isNaN(num)) return num;
    
    // Try boolean
    if (value.toLowerCase() === 'true') return true;
    if (value.toLowerCase() === 'false') return false;
    
    return value;
  }

  private calculateInputCoverage(tests: TestCase[], rules: TestBusinessRule[]): number {
    const allInputs = new Set<string>();
    const testedInputs = new Set<string>();
    
    for (const rule of rules) {
      for (const input of rule.inputs || []) {
        allInputs.add(input.name);
      }
    }
    
    for (const test of tests) {
      for (const inputName of Object.keys(test.inputs)) {
        if (allInputs.has(inputName)) {
          testedInputs.add(inputName);
        }
      }
    }
    
    return allInputs.size > 0 ? testedInputs.size / allInputs.size : 1;
  }

  private calculateOutputCoverage(tests: TestCase[], rules: TestBusinessRule[]): number {
    const allOutputs = new Set<string>();
    const testedOutputs = new Set<string>();
    
    for (const rule of rules) {
      for (const output of rule.outputs || []) {
        allOutputs.add(output.name);
      }
    }
    
    for (const test of tests) {
      if (test.expectedOutput && typeof test.expectedOutput === 'object') {
        for (const outputName of Object.keys(test.expectedOutput as Record<string, unknown>)) {
          if (allOutputs.has(outputName)) {
            testedOutputs.add(outputName);
          }
        }
      }
    }
    
    return allOutputs.size > 0 ? testedOutputs.size / allOutputs.size : 1;
  }
}
