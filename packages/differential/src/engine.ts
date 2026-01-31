/**
 * Behavioral Differential Analysis Engine
 * 
 * Main orchestrator for comparing legacy and migrated code behavior.
 */

import type {
  DifferentialConfig,
  ExecutableCode,
  ExecutionContext,
  ExecutionResult,
  BehaviorComparison,
  TestCaseResult,
  ComparisonSummary,
  BehaviorDifference,
  DifferentialEvent,
  GeneratedInput,
  DataValue,
} from './types.js';

import { InputFuzzer, createInputFuzzer } from './analyzers/input-fuzzer.js';
import { ExecutionTracer, TraceAnalyzer, createTraceAnalyzer } from './analyzers/execution-tracer.js';
import { OutputComparator, createOutputComparator } from './comparators/output-comparator.js';
import { SideEffectComparator, createSideEffectComparator } from './comparators/side-effect-comparator.js';
import { DifferentialReportGenerator, createReportGenerator } from './reporters/report-generator.js';

// ============================================================================
// Engine Configuration
// ============================================================================

const DEFAULT_CONFIG: DifferentialConfig = {
  maxIterations: 1000,
  timeout: 30000,
  fuzzingDepth: 3,
  numericTolerance: 0.0001,
  stringComparison: 'normalized',
  dateFormatTolerance: true,
  nullEquivalents: ['', 'null', 'NULL', 'None', 'undefined', 'N/A'],
  ignoreWhitespace: true,
  parallelExecution: true,
  maxConcurrency: 4,
};

// ============================================================================
// Behavioral Differential Engine
// ============================================================================

export class BehavioralDifferentialEngine {
  private config: DifferentialConfig;
  private inputFuzzer: InputFuzzer;
  private traceAnalyzer: TraceAnalyzer;
  private outputComparator: OutputComparator;
  private sideEffectComparator: SideEffectComparator;
  private reportGenerator: DifferentialReportGenerator;
  private eventHandlers: ((event: DifferentialEvent) => void)[] = [];

  constructor(config: Partial<DifferentialConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.inputFuzzer = createInputFuzzer('boundary');
    this.traceAnalyzer = createTraceAnalyzer();
    this.outputComparator = createOutputComparator(this.config);
    this.sideEffectComparator = createSideEffectComparator();
    this.reportGenerator = createReportGenerator();
  }

  /**
   * Subscribe to differential analysis events
   */
  onEvent(handler: (event: DifferentialEvent) => void): () => void {
    this.eventHandlers.push(handler);
    return () => {
      const index = this.eventHandlers.indexOf(handler);
      if (index > -1) this.eventHandlers.splice(index, 1);
    };
  }

  /**
   * Run differential analysis between legacy and migrated code
   */
  async analyze(
    legacyCode: ExecutableCode,
    migratedCode: ExecutableCode,
    testInputs?: GeneratedInput[]
  ): Promise<BehaviorComparison> {
    const comparisonId = `comparison_${Date.now()}`;

    // Generate test inputs if not provided
    const inputs = testInputs ?? this.inputFuzzer.generate(this.config.maxIterations ?? 100);

    const comparison: BehaviorComparison = {
      id: comparisonId,
      timestamp: new Date(),
      legacyCode,
      migratedCode,
      testCases: [],
      summary: this.createEmptySummary(),
      differences: [],
    };

    this.emit({ type: 'analysis-started', comparison });

    // Run test cases
    const testCases = await this.runTestCases(
      legacyCode,
      migratedCode,
      inputs
    );

    comparison.testCases = testCases;

    // Collect all differences
    for (const tc of testCases) {
      for (const diff of tc.comparison.outputDiffs) {
        comparison.differences.push(this.toBehaviorDifference(tc.id, 'output', diff as unknown as Record<string, unknown>));
      }
      for (const diff of tc.comparison.sideEffectDiffs) {
        comparison.differences.push(this.toBehaviorDifference(tc.id, 'side-effect', diff as unknown as Record<string, unknown>));
      }
    }

    // Calculate summary
    comparison.summary = this.calculateSummary(testCases);

    this.emit({ type: 'analysis-completed', comparison });

    return comparison;
  }

  /**
   * Generate a differential report
   */
  generateReport(
    comparison: BehaviorComparison,
    format: 'json' | 'html' | 'markdown' = 'markdown'
  ): string {
    const report = this.reportGenerator.generate(comparison, format);
    return this.reportGenerator.render(report);
  }

  /**
   * Run all test cases
   */
  private async runTestCases(
    legacyCode: ExecutableCode,
    migratedCode: ExecutableCode,
    inputs: GeneratedInput[]
  ): Promise<TestCaseResult[]> {
    const results: TestCaseResult[] = [];

    if (this.config.parallelExecution) {
      const batchSize = this.config.maxConcurrency ?? 4;
      for (let i = 0; i < inputs.length; i += batchSize) {
        const batch = inputs.slice(i, i + batchSize);
        const batchResults = await Promise.all(
          batch.map((input, idx) =>
            this.runSingleTestCase(
              legacyCode,
              migratedCode,
              input,
              `test_${i + idx}`
            )
          )
        );
        results.push(...batchResults);
      }
    } else {
      for (let i = 0; i < inputs.length; i++) {
        const input = inputs[i]!;
        const result = await this.runSingleTestCase(
          legacyCode,
          migratedCode,
          input,
          `test_${i}`
        );
        results.push(result);
      }
    }

    return results;
  }

  /**
   * Run a single test case
   */
  private async runSingleTestCase(
    legacyCode: ExecutableCode,
    migratedCode: ExecutableCode,
    input: GeneratedInput,
    testId: string
  ): Promise<TestCaseResult> {
    const context: ExecutionContext = {
      inputs: input.values,
      timeout: this.config.timeout,
    };

    // Execute both versions
    const legacyResult = await this.executeCode(legacyCode, context);
    const migratedResult = await this.executeCode(migratedCode, context);

    // Compare results
    const outputDiffs = this.outputComparator.compare(
      legacyResult.outputs,
      migratedResult.outputs
    );

    const traceDiffs = this.traceAnalyzer.compare(
      legacyResult.trace,
      migratedResult.trace
    );

    const sideEffectDiffs = this.sideEffectComparator.compare(
      legacyResult.sideEffects,
      migratedResult.sideEffects
    );

    const equivalent = 
      outputDiffs.filter(d => d.severity === 'critical').length === 0 &&
      sideEffectDiffs.filter(d => d.diff === 'missing' || d.diff === 'modified').length === 0;

    const testCase: TestCaseResult = {
      id: testId,
      name: input.rationale || `Test ${testId}`,
      context,
      legacyResult,
      migratedResult,
      comparison: {
        equivalent,
        outputDiffs,
        traceDiffs: traceDiffs.branchDiffs.map(bd => ({
          type: 'branch' as const,
          location: bd.location,
          legacy: bd.legacy,
          migrated: bd.migrated,
          impact: 'medium' as const,
        })),
        sideEffectDiffs,
        performanceDiff: {
          legacyDuration: legacyResult.duration,
          migratedDuration: migratedResult.duration,
          ratio: migratedResult.duration / Math.max(1, legacyResult.duration),
          significant: Math.abs(migratedResult.duration - legacyResult.duration) > 100,
        },
      },
    };

    this.emit({ type: 'test-case-completed', testCase });

    return testCase;
  }

  /**
   * Execute code (simulated - real implementation would use interpreters)
   */
  private async executeCode(
    code: ExecutableCode,
    context: ExecutionContext
  ): Promise<ExecutionResult> {
    const startTime = Date.now();
    const tracer = new ExecutionTracer();
    tracer.start();

    // Simulated execution - in a real implementation, this would:
    // 1. For COBOL: Use GnuCOBOL or a COBOL interpreter
    // 2. For Java: Use JVM with instrumentation
    // 3. For Python: Use exec() with trace hooks
    // 4. For TypeScript: Use VM with instrumentation

    // Simulate some execution
    const outputs: Record<string, DataValue> = {};

    // Copy inputs to outputs (simplified simulation)
    for (const [key, value] of Object.entries(context.inputs)) {
      outputs[`output_${key}`] = value;
    }

    // Simulate some business logic output
    outputs['result'] = {
      type: 'string',
      value: 'PROCESSED',
    };

    // Record trace
    tracer.recordStep(
      { file: code.entryPoint, line: 1, function: 'main' },
      'ENTER',
      context.inputs
    );

    tracer.recordBranch(
      { file: code.entryPoint, line: 10 },
      'condition > threshold',
      true
    );

    tracer.recordStep(
      { file: code.entryPoint, line: 20, function: 'main' },
      'EXIT',
      outputs
    );

    const duration = Date.now() - startTime;

    return {
      success: true,
      outputs,
      sideEffects: [],
      trace: tracer.getTrace(),
      duration,
    };
  }

  /**
   * Calculate comparison summary
   */
  private calculateSummary(testCases: TestCaseResult[]): ComparisonSummary {
    const total = testCases.length;
    const equivalent = testCases.filter(tc => tc.comparison.equivalent).length;
    const failed = testCases.filter(tc => 
      tc.comparison.outputDiffs.some(d => d.severity === 'critical')
    ).length;
    const warnings = testCases.filter(tc =>
      tc.comparison.outputDiffs.some(d => d.severity === 'warning') &&
      !tc.comparison.outputDiffs.some(d => d.severity === 'critical')
    ).length;

    let criticalDiffs = 0;
    let warningDiffs = 0;
    let totalPerfRatio = 0;

    for (const tc of testCases) {
      criticalDiffs += tc.comparison.outputDiffs.filter(d => d.severity === 'critical').length;
      warningDiffs += tc.comparison.outputDiffs.filter(d => d.severity === 'warning').length;
      totalPerfRatio += tc.comparison.performanceDiff?.ratio ?? 1;
    }

    return {
      totalTestCases: total,
      equivalentCases: equivalent,
      failedCases: failed,
      warningCases: warnings,
      equivalenceRate: total > 0 ? equivalent / total : 1,
      criticalDifferences: criticalDiffs,
      warningDifferences: warningDiffs,
      averagePerformanceRatio: total > 0 ? totalPerfRatio / total : 1,
    };
  }

  /**
   * Create empty summary
   */
  private createEmptySummary(): ComparisonSummary {
    return {
      totalTestCases: 0,
      equivalentCases: 0,
      failedCases: 0,
      warningCases: 0,
      equivalenceRate: 0,
      criticalDifferences: 0,
      warningDifferences: 0,
      averagePerformanceRatio: 1,
    };
  }

  /**
   * Convert a diff to BehaviorDifference
   */
  private toBehaviorDifference(
    testCaseId: string,
    type: 'output' | 'trace' | 'side-effect',
    diff: Record<string, unknown>
  ): BehaviorDifference {
    const severity = (diff['severity'] as 'critical' | 'warning' | 'info') || 'warning';
    const path = diff['path'] as string | undefined;
    const explanation = diff['explanation'] as string | undefined;
    
    return {
      id: `diff_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      type,
      severity,
      testCaseId,
      description: explanation || `${type} difference at ${path || 'unknown'}`,
      legacy: diff['legacy'] || diff['legacyValue'],
      migrated: diff['migrated'] || diff['migratedValue'],
      possibleCauses: this.inferCauses(type),
      suggestedFixes: this.suggestFixes(type),
    };
  }

  /**
   * Infer possible causes for a difference
   */
  private inferCauses(type: string): string[] {
    const causes: string[] = [];

    if (type === 'output') {
      causes.push('Calculation logic differs between implementations');
      causes.push('Data type handling differs (e.g., decimal precision)');
      causes.push('Null/empty value handling differs');
    } else if (type === 'side-effect') {
      causes.push('Database operation sequence differs');
      causes.push('Transaction boundaries differ');
    }

    return causes;
  }

  /**
   * Suggest fixes for a difference
   */
  private suggestFixes(type: string): string[] {
    const fixes: string[] = [];

    if (type === 'output') {
      fixes.push('Review business logic implementation');
      fixes.push('Check data type conversions');
      fixes.push('Verify rounding and precision handling');
    } else if (type === 'side-effect') {
      fixes.push('Align database operation sequences');
      fixes.push('Verify transaction boundaries match');
    }

    return fixes;
  }

  /**
   * Emit an event
   */
  private emit(event: DifferentialEvent): void {
    for (const handler of this.eventHandlers) {
      try {
        handler(event);
      } catch (e) {
        console.error('Event handler error:', e);
      }
    }
  }
}

// ============================================================================
// Factory
// ============================================================================

export function createDifferentialEngine(
  config?: Partial<DifferentialConfig>
): BehavioralDifferentialEngine {
  return new BehavioralDifferentialEngine(config);
}
