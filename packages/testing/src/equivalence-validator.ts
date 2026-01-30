/**
 * Equivalence Validator
 * 
 * Validates that legacy and modern systems produce equivalent outputs
 */

import type {
  TestCase,
  TestResult,
  SystemAdapter,
  EquivalenceReport,
  Difference,
  CoverageMetrics,
  ReportSummary,
} from './types.js';

export class EquivalenceValidator {
  private legacyAdapter: SystemAdapter;
  private modernAdapter: SystemAdapter;

  constructor(legacyAdapter: SystemAdapter, modernAdapter: SystemAdapter) {
    this.legacyAdapter = legacyAdapter;
    this.modernAdapter = modernAdapter;
  }

  /**
   * Validate a single test case
   */
  async validateTestCase(testCase: TestCase): Promise<TestResult> {
    try {
      // Execute on both systems
      const [legacyResult, modernResult] = await Promise.all([
        this.legacyAdapter.execute(testCase.inputs),
        this.modernAdapter.execute(testCase.inputs),
      ]);

      // Check for execution errors
      if (legacyResult.error || modernResult.error) {
        return {
          testCase,
          legacyOutput: legacyResult.output,
          modernOutput: modernResult.output,
          equivalent: false,
          executionTimeMs: {
            legacy: legacyResult.executionTimeMs,
            modern: modernResult.executionTimeMs,
          },
          error: legacyResult.error || modernResult.error,
        };
      }

      // Compare outputs
      const differences = this.compareOutputs(
        legacyResult.output,
        modernResult.output,
        testCase.tolerance,
        ''
      );

      const equivalent = differences.filter(d => d.severity === 'critical').length === 0;

      return {
        testCase,
        legacyOutput: legacyResult.output,
        modernOutput: modernResult.output,
        equivalent,
        executionTimeMs: {
          legacy: legacyResult.executionTimeMs,
          modern: modernResult.executionTimeMs,
        },
        differences: differences.length > 0 ? differences : undefined,
      };
    } catch (error) {
      return {
        testCase,
        legacyOutput: null,
        modernOutput: null,
        equivalent: false,
        executionTimeMs: { legacy: 0, modern: 0 },
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }

  /**
   * Validate multiple test cases
   */
  async validate(testCases: TestCase[]): Promise<EquivalenceReport> {
    const results: TestResult[] = [];
    
    // Run tests in batches for performance
    const batchSize = 10;
    for (let i = 0; i < testCases.length; i += batchSize) {
      const batch = testCases.slice(i, i + batchSize);
      const batchResults = await Promise.all(
        batch.map(tc => this.validateTestCase(tc))
      );
      results.push(...batchResults);
    }

    // Calculate metrics
    const passed = results.filter(r => r.equivalent).length;
    const failed = results.filter(r => !r.equivalent && !r.error).length;
    const skipped = results.filter(r => !!r.error).length;

    // Calculate coverage
    const coverage = this.calculateCoverage(testCases, results);

    // Calculate confidence score
    const confidence = this.calculateConfidence(results, coverage);

    // Generate summary
    const summary = this.generateSummary(results, coverage, confidence);

    return {
      projectId: '',
      moduleId: '',
      timestamp: new Date().toISOString(),
      totalTests: testCases.length,
      passed,
      failed,
      skipped,
      coverage,
      confidence,
      results,
      summary,
    };
  }

  /**
   * Compare two outputs and find differences
   */
  private compareOutputs(
    legacy: unknown,
    modern: unknown,
    tolerance?: TestCase['tolerance'],
    path: string = ''
  ): Difference[] {
    const differences: Difference[] = [];

    // Handle null/undefined
    if (legacy === null || legacy === undefined) {
      if (modern !== null && modern !== undefined) {
        differences.push({
          path: path || 'root',
          legacyValue: legacy,
          modernValue: modern,
          type: 'extra',
          severity: 'critical',
        });
      }
      return differences;
    }

    if (modern === null || modern === undefined) {
      differences.push({
        path: path || 'root',
        legacyValue: legacy,
        modernValue: modern,
        type: 'missing',
        severity: 'critical',
      });
      return differences;
    }

    // Type mismatch
    if (typeof legacy !== typeof modern) {
      differences.push({
        path: path || 'root',
        legacyValue: legacy,
        modernValue: modern,
        type: 'type-mismatch',
        severity: 'critical',
      });
      return differences;
    }

    // Compare by type
    if (typeof legacy === 'number') {
      const numericTolerance = tolerance?.numeric ?? 0.001;
      if (Math.abs(legacy - (modern as number)) > numericTolerance) {
        differences.push({
          path: path || 'root',
          legacyValue: legacy,
          modernValue: modern,
          type: 'value-mismatch',
          severity: 'critical',
        });
      }
    } else if (typeof legacy === 'string') {
      let legacyStr = legacy;
      let modernStr = modern as string;
      
      if (tolerance?.string === 'trim') {
        legacyStr = legacyStr.trim();
        modernStr = modernStr.trim();
      } else if (tolerance?.string === 'case-insensitive') {
        legacyStr = legacyStr.toLowerCase();
        modernStr = modernStr.toLowerCase();
      }
      
      if (legacyStr !== modernStr) {
        differences.push({
          path: path || 'root',
          legacyValue: legacy,
          modernValue: modern,
          type: 'value-mismatch',
          severity: 'critical',
        });
      }
    } else if (typeof legacy === 'boolean') {
      if (legacy !== modern) {
        differences.push({
          path: path || 'root',
          legacyValue: legacy,
          modernValue: modern,
          type: 'value-mismatch',
          severity: 'critical',
        });
      }
    } else if (Array.isArray(legacy)) {
      if (!Array.isArray(modern)) {
        differences.push({
          path: path || 'root',
          legacyValue: legacy,
          modernValue: modern,
          type: 'type-mismatch',
          severity: 'critical',
        });
      } else {
        const modernArr = modern as unknown[];
        
        if (tolerance?.array === 'unordered') {
          // Compare as sets
          if (legacy.length !== modernArr.length) {
            differences.push({
              path: `${path}.length`,
              legacyValue: legacy.length,
              modernValue: modernArr.length,
              type: 'value-mismatch',
              severity: 'critical',
            });
          }
        } else {
          // Compare element by element
          const maxLen = Math.max(legacy.length, modernArr.length);
          for (let i = 0; i < maxLen; i++) {
            const itemDiffs = this.compareOutputs(
              legacy[i],
              modernArr[i],
              tolerance,
              `${path}[${i}]`
            );
            differences.push(...itemDiffs);
          }
        }
      }
    } else if (typeof legacy === 'object') {
      const legacyObj = legacy as Record<string, unknown>;
      const modernObj = modern as Record<string, unknown>;
      
      const allKeys = new Set([...Object.keys(legacyObj), ...Object.keys(modernObj)]);
      
      for (const key of allKeys) {
        const keyPath = path ? `${path}.${key}` : key;
        
        if (!(key in legacyObj)) {
          differences.push({
            path: keyPath,
            legacyValue: undefined,
            modernValue: modernObj[key],
            type: 'extra',
            severity: 'warning',
          });
        } else if (!(key in modernObj)) {
          differences.push({
            path: keyPath,
            legacyValue: legacyObj[key],
            modernValue: undefined,
            type: 'missing',
            severity: 'critical',
          });
        } else {
          const keyDiffs = this.compareOutputs(
            legacyObj[key],
            modernObj[key],
            tolerance,
            keyPath
          );
          differences.push(...keyDiffs);
        }
      }
    }

    return differences;
  }

  /**
   * Calculate test coverage metrics
   */
  private calculateCoverage(
    testCases: TestCase[],
    results: TestResult[]
  ): CoverageMetrics {
    const coveredRules = new Set(testCases.map(tc => tc.sourceRule).filter(Boolean));
    const boundaryTests = testCases.filter(tc => tc.generationStrategy === 'boundary');
    const edgeCaseTests = testCases.filter(tc => tc.tags.includes('edge-case'));
    
    return {
      rulesCovered: coveredRules.size,
      totalRules: coveredRules.size, // Would come from project metadata
      codePathsCovered: results.filter(r => r.equivalent).length,
      totalCodePaths: testCases.length,
      boundaryTestsCovered: boundaryTests.filter(tc => 
        results.find(r => r.testCase.id === tc.id)?.equivalent
      ).length,
      edgeCasesCovered: edgeCaseTests.filter(tc => 
        results.find(r => r.testCase.id === tc.id)?.equivalent
      ).length,
    };
  }

  /**
   * Calculate confidence score (0-1)
   */
  private calculateConfidence(
    results: TestResult[],
    coverage: CoverageMetrics
  ): number {
    if (results.length === 0) return 0;

    const passRate = results.filter(r => r.equivalent).length / results.length;
    const coverageScore = coverage.codePathsCovered / Math.max(coverage.totalCodePaths, 1);
    
    // Weight: 70% pass rate, 30% coverage
    return passRate * 0.7 + coverageScore * 0.3;
  }

  /**
   * Generate report summary
   */
  private generateSummary(
    results: TestResult[],
    coverage: CoverageMetrics,
    confidence: number
  ): ReportSummary {
    const passRate = results.length > 0 
      ? results.filter(r => r.equivalent).length / results.length 
      : 0;
    
    const criticalFailures = results.filter(r => 
      !r.equivalent && 
      r.testCase.priority === 'critical'
    ).length;

    // Determine status
    let status: ReportSummary['status'];
    if (passRate >= 0.99 && criticalFailures === 0) {
      status = 'passed';
    } else if (passRate >= 0.95 && criticalFailures === 0) {
      status = 'warning';
    } else {
      status = 'failed';
    }

    // Risk assessment
    let riskAssessment: ReportSummary['riskAssessment'];
    if (confidence >= 0.95 && criticalFailures === 0) {
      riskAssessment = 'low';
    } else if (confidence >= 0.80 && criticalFailures <= 2) {
      riskAssessment = 'medium';
    } else {
      riskAssessment = 'high';
    }

    // Generate recommendations
    const recommendations: string[] = [];
    
    if (passRate < 0.99) {
      recommendations.push(`${Math.round((1 - passRate) * results.length)} test cases failed - review differences`);
    }
    if (criticalFailures > 0) {
      recommendations.push(`${criticalFailures} critical test(s) failed - must fix before migration`);
    }
    if (coverage.boundaryTestsCovered < coverage.rulesCovered * 2) {
      recommendations.push('Consider adding more boundary value tests');
    }
    if (coverage.edgeCasesCovered === 0) {
      recommendations.push('No edge case tests - consult SME for edge case scenarios');
    }

    return {
      status,
      passRate,
      criticalFailures,
      recommendations,
      riskAssessment,
    };
  }
}
