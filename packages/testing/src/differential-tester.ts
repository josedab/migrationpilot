/**
 * Differential Tester
 * 
 * Runs differential testing between legacy and modern systems
 * using production traffic shadowing
 */

import type { 
  SystemAdapter, 
  TestResult, 
  TestCase,
  EquivalenceReport 
} from './types.js';
import { EquivalenceValidator } from './equivalence-validator.js';
import { randomUUID } from 'crypto';

export interface DifferentialTestConfig {
  shadowPercentage: number;
  maxConcurrent: number;
  timeoutMs: number;
  reportDiscrepancies: boolean;
  onDiscrepancy?: (result: TestResult) => void;
}

export interface ShadowResult {
  requestId: string;
  timestamp: string;
  inputs: Record<string, unknown>;
  legacyOutput: unknown;
  modernOutput: unknown;
  equivalent: boolean;
  latencyMs: {
    legacy: number;
    modern: number;
  };
  differences?: TestResult['differences'];
}

export class DifferentialTester {
  private legacyAdapter: SystemAdapter;
  private modernAdapter: SystemAdapter;
  private validator: EquivalenceValidator;
  private config: DifferentialTestConfig;
  private results: ShadowResult[] = [];
  private isRunning = false;

  constructor(
    legacyAdapter: SystemAdapter,
    modernAdapter: SystemAdapter,
    config?: Partial<DifferentialTestConfig>
  ) {
    this.legacyAdapter = legacyAdapter;
    this.modernAdapter = modernAdapter;
    this.validator = new EquivalenceValidator(legacyAdapter, modernAdapter);
    this.config = {
      shadowPercentage: 10,
      maxConcurrent: 5,
      timeoutMs: 5000,
      reportDiscrepancies: true,
      ...config,
    };
  }

  /**
   * Shadow a single request
   */
  async shadowRequest(inputs: Record<string, unknown>): Promise<ShadowResult> {
    const requestId = randomUUID();
    const timestamp = new Date().toISOString();

    // Execute on both systems in parallel
    const [legacyResult, modernResult] = await Promise.all([
      this.legacyAdapter.execute(inputs),
      this.modernAdapter.execute(inputs),
    ]);

    // Create test case for validation
    const testCase: TestCase = {
      id: requestId,
      name: 'Shadow test',
      description: 'Production shadow test',
      inputs,
      tags: ['shadow'],
      generationStrategy: 'historical-replay',
      priority: 'medium',
    };

    // Validate equivalence
    const testResult = await this.validator.validateTestCase(testCase);

    const result: ShadowResult = {
      requestId,
      timestamp,
      inputs,
      legacyOutput: legacyResult.output,
      modernOutput: modernResult.output,
      equivalent: testResult.equivalent,
      latencyMs: {
        legacy: legacyResult.executionTimeMs,
        modern: modernResult.executionTimeMs,
      },
      differences: testResult.differences,
    };

    // Store result
    this.results.push(result);

    // Report discrepancies
    if (!result.equivalent && this.config.reportDiscrepancies) {
      this.config.onDiscrepancy?.(testResult);
    }

    return result;
  }

  /**
   * Start continuous shadow testing
   */
  async startShadowing(
    requestGenerator: () => Promise<Record<string, unknown> | null>
  ): Promise<void> {
    this.isRunning = true;

    while (this.isRunning) {
      // Check if we should shadow this request
      if (Math.random() * 100 > this.config.shadowPercentage) {
        continue;
      }

      const inputs = await requestGenerator();
      if (!inputs) break;

      try {
        await this.shadowRequest(inputs);
      } catch (error) {
        console.error('Shadow request failed:', error);
      }
    }
  }

  /**
   * Stop shadow testing
   */
  stopShadowing(): void {
    this.isRunning = false;
  }

  /**
   * Get shadow testing statistics
   */
  getStatistics(): ShadowStatistics {
    const total = this.results.length;
    const equivalent = this.results.filter(r => r.equivalent).length;
    const divergent = total - equivalent;

    const legacyLatencies = this.results.map(r => r.latencyMs.legacy);
    const modernLatencies = this.results.map(r => r.latencyMs.modern);

    return {
      totalRequests: total,
      equivalentResponses: equivalent,
      divergentResponses: divergent,
      equivalenceRate: total > 0 ? equivalent / total : 1,
      latency: {
        legacy: {
          avg: this.average(legacyLatencies),
          p50: this.percentile(legacyLatencies, 50),
          p95: this.percentile(legacyLatencies, 95),
          p99: this.percentile(legacyLatencies, 99),
        },
        modern: {
          avg: this.average(modernLatencies),
          p50: this.percentile(modernLatencies, 50),
          p95: this.percentile(modernLatencies, 95),
          p99: this.percentile(modernLatencies, 99),
        },
      },
      recentDivergences: this.results
        .filter(r => !r.equivalent)
        .slice(-10),
    };
  }

  /**
   * Generate a report from shadow testing
   */
  generateReport(): EquivalenceReport {
    const results: TestResult[] = this.results.map(r => ({
      testCase: {
        id: r.requestId,
        name: `Shadow test ${r.requestId.substring(0, 8)}`,
        description: `Shadow test at ${r.timestamp}`,
        inputs: r.inputs,
        tags: ['shadow'],
        generationStrategy: 'historical-replay' as const,
        priority: 'medium' as const,
      },
      legacyOutput: r.legacyOutput,
      modernOutput: r.modernOutput,
      equivalent: r.equivalent,
      executionTimeMs: r.latencyMs,
      differences: r.differences,
    }));

    const passed = results.filter(r => r.equivalent).length;
    const failed = results.filter(r => !r.equivalent).length;

    return {
      projectId: '',
      moduleId: '',
      timestamp: new Date().toISOString(),
      totalTests: results.length,
      passed,
      failed,
      skipped: 0,
      coverage: {
        rulesCovered: 0,
        totalRules: 0,
        codePathsCovered: passed,
        totalCodePaths: results.length,
        boundaryTestsCovered: 0,
        edgeCasesCovered: 0,
      },
      confidence: results.length > 0 ? passed / results.length : 0,
      results,
      summary: {
        status: failed === 0 ? 'passed' : failed < results.length * 0.05 ? 'warning' : 'failed',
        passRate: results.length > 0 ? passed / results.length : 1,
        criticalFailures: failed,
        recommendations: failed > 0 
          ? [`${failed} shadow tests failed - investigate divergences`]
          : [],
        riskAssessment: failed === 0 ? 'low' : failed < 5 ? 'medium' : 'high',
      },
    };
  }

  /**
   * Clear stored results
   */
  clearResults(): void {
    this.results = [];
  }

  private average(numbers: number[]): number {
    if (numbers.length === 0) return 0;
    return numbers.reduce((a, b) => a + b, 0) / numbers.length;
  }

  private percentile(numbers: number[], p: number): number {
    if (numbers.length === 0) return 0;
    const sorted = [...numbers].sort((a, b) => a - b);
    const index = Math.ceil((p / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)] ?? 0;
  }
}

export interface ShadowStatistics {
  totalRequests: number;
  equivalentResponses: number;
  divergentResponses: number;
  equivalenceRate: number;
  latency: {
    legacy: LatencyStats;
    modern: LatencyStats;
  };
  recentDivergences: ShadowResult[];
}

export interface LatencyStats {
  avg: number;
  p50: number;
  p95: number;
  p99: number;
}
