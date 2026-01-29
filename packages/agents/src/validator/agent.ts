/**
 * Validator Agent
 * 
 * Validates behavioral equivalence between legacy and modern code.
 * Generates test cases, executes comparisons, and reports differences.
 */

import type {
  AgentType,
  TestCase,
  TestResult,
  EquivalenceReport,
  BusinessRule,
} from '@migrationpilot/core';
import { BaseAgent, type AgentConfig, type AgentContext, type AgentResponse } from '../common/base-agent.js';
import { getCopilotClient, type ICopilotClient } from '../common/copilot-client.js';
import { modelConfigProvider } from '../common/model-config.js';
import { validatorTools } from './tools.js';

export interface ValidationResult {
  report: EquivalenceReport;
  testCases: TestCase[];
  results: TestResult[];
  recommendations: string[];
}

export interface TestGenerationConfig {
  strategies: ('boundary' | 'partition' | 'property' | 'historical')[];
  maxTestsPerRule: number;
  includeEdgeCases: boolean;
  tolerances?: Record<string, number>;
}

const SYSTEM_PROMPT = `You are a quality assurance expert specializing in migration validation. Your job is to ensure that migrated code behaves identically to the original legacy code.

Your responsibilities:
1. Generate comprehensive test cases from business rules
2. Execute tests against both legacy and modern systems
3. Compare outputs and identify discrepancies
4. Analyze root causes of any differences
5. Calculate confidence scores for migration readiness

Testing strategies:
- Boundary value analysis: Test at edges of valid ranges
- Equivalence partitioning: Test representative values from each partition
- Property-based testing: Random inputs to find edge cases
- Historical replay: Use real historical data

For discrepancies:
- Identify exact inputs that cause differences
- Determine if difference is significant or acceptable (rounding, etc.)
- Provide root cause analysis
- Recommend fixes

Output validation results in structured JSON format with detailed metrics.`;

export class ValidatorAgent extends BaseAgent {
  private copilot: ICopilotClient;

  constructor(copilotClient?: ICopilotClient) {
    const modelConfig = modelConfigProvider.getConfig('validator');
    const config: AgentConfig = {
      model: modelConfig.model,
      systemPrompt: SYSTEM_PROMPT,
      tools: validatorTools,
      temperature: modelConfig.temperature,
      maxTokens: modelConfig.maxTokens,
    };
    super(config);
    this.copilot = copilotClient ?? getCopilotClient();
  }

  get type(): AgentType {
    return 'validator';
  }

  /**
   * Generate test cases from business rules
   */
  async generateTestCases(
    context: AgentContext,
    businessRules: BusinessRule[],
    config: TestGenerationConfig
  ): Promise<AgentResponse<TestCase[]>> {
    await this.initialize(context);

    const prompt = `Generate comprehensive test cases for these business rules:

Business Rules:
${JSON.stringify(businessRules.map(r => ({
  id: r.id,
  name: r.name,
  category: r.category,
  inputs: r.inputs,
  outputs: r.outputs,
  logic: r.logic,
  edgeCases: r.edgeCases,
})), null, 2)}

Test Generation Config:
- Strategies: ${config.strategies.join(', ')}
- Max tests per rule: ${config.maxTestsPerRule}
- Include edge cases: ${config.includeEdgeCases}

For each rule, generate:
1. Boundary value tests (min, max, just inside, just outside)
2. Equivalence partition tests (valid/invalid partitions)
3. Edge case tests (zero, null, empty, maximum precision)
4. Happy path tests

Each test case should have:
- Unique ID
- Description
- Input values
- Expected outputs (if known)
- Category (boundary, partition, edge, happy)
- Priority (high for critical rules)

Output as JSON array of TestCase objects.`;

    return this.send(prompt) as Promise<AgentResponse<TestCase[]>>;
  }

  /**
   * Validate equivalence between legacy and modern outputs
   */
  async validateEquivalence(
    context: AgentContext,
    testCases: TestCase[],
    legacyResults: Record<string, unknown>[],
    modernResults: Record<string, unknown>[],
    tolerances?: Record<string, number>
  ): Promise<AgentResponse<ValidationResult>> {
    await this.initialize(context);

    const prompt = `Validate equivalence between legacy and modern system outputs:

Test Cases: ${testCases.length}

Legacy Results:
${JSON.stringify(legacyResults.slice(0, 10), null, 2)}
${legacyResults.length > 10 ? `... and ${legacyResults.length - 10} more` : ''}

Modern Results:
${JSON.stringify(modernResults.slice(0, 10), null, 2)}
${modernResults.length > 10 ? `... and ${modernResults.length - 10} more` : ''}

Tolerances:
${JSON.stringify(tolerances || { default: 0.01 }, null, 2)}

For each comparison:
1. Check if outputs are equivalent within tolerance
2. If different, identify which fields differ
3. Determine if difference is significant
4. Provide root cause analysis for failures

Generate a validation report with:
- Total tests, passed, failed
- Overall equivalence score
- Failed test details with analysis
- Recommendations for fixing discrepancies

Output as JSON matching ValidationResult schema.`;

    return this.send(prompt) as Promise<AgentResponse<ValidationResult>>;
  }

  /**
   * Analyze a specific failure
   */
  async analyzeFailure(
    context: AgentContext,
    testCase: TestCase,
    legacyOutput: Record<string, unknown>,
    modernOutput: Record<string, unknown>,
    legacyCode: string,
    modernCode: string
  ): Promise<AgentResponse<{
    rootCause: string;
    recommendation: string;
    severity: 'critical' | 'major' | 'minor';
    codeLocation?: string;
  }>> {
    await this.initialize(context);

    const prompt = `Analyze this test failure:

Test Case:
${JSON.stringify(testCase, null, 2)}

Legacy Output:
${JSON.stringify(legacyOutput, null, 2)}

Modern Output:
${JSON.stringify(modernOutput, null, 2)}

Legacy Code:
\`\`\`
${legacyCode.slice(0, 2000)}
\`\`\`

Modern Code:
\`\`\`
${modernCode.slice(0, 2000)}
\`\`\`

Determine:
1. Root cause of the difference
2. Whether it's a bug in modern code or expected difference
3. Severity (critical, major, minor)
4. Specific code location to fix
5. Recommendation for resolution

Output as JSON.`;

    return this.send(prompt) as Promise<AgentResponse<{
      rootCause: string;
      recommendation: string;
      severity: 'critical' | 'major' | 'minor';
      codeLocation?: string;
    }>>;
  }

  /**
   * Calculate overall migration confidence
   */
  async calculateConfidence(
    context: AgentContext,
    validationResults: ValidationResult,
    rulesCoverage: number,
    pathsCoverage: number
  ): Promise<AgentResponse<{
    confidence: number;
    breakdown: {
      equivalenceScore: number;
      coverageScore: number;
      criticality: number;
    };
    readyForProduction: boolean;
    blockers: string[];
  }>> {
    await this.initialize(context);

    const prompt = `Calculate migration confidence based on validation results:

Validation Summary:
- Total Tests: ${validationResults.report.totalTests}
- Passed: ${validationResults.report.passed}
- Failed: ${validationResults.report.failed}
- Equivalence Score: ${validationResults.report.equivalenceScore}

Coverage:
- Business Rules Coverage: ${(rulesCoverage * 100).toFixed(1)}%
- Code Paths Coverage: ${(pathsCoverage * 100).toFixed(1)}%

Failed Tests Analysis:
${JSON.stringify(validationResults.results.filter(r => !r.equivalent).slice(0, 5), null, 2)}

Calculate:
1. Overall confidence score (0-100%)
2. Breakdown by category
3. Whether migration is ready for production
4. Any blocking issues that must be resolved

A migration is ready for production if:
- Equivalence score >= 99%
- No critical failures
- Business rules coverage >= 95%
- Code paths coverage >= 85%

Output as JSON.`;

    return this.send(prompt) as Promise<AgentResponse<{
      confidence: number;
      breakdown: {
        equivalenceScore: number;
        coverageScore: number;
        criticality: number;
      };
      readyForProduction: boolean;
      blockers: string[];
    }>>;
  }

  /**
   * Execute the agent logic
   */
  protected async execute(_prompt: string): Promise<AgentResponse> {
    return this.executeWithCopilot(this.copilot);
  }
}
