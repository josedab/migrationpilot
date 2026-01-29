/**
 * Validator Agent Tools
 */

import type { AgentTool } from '@migrationpilot/core';

/**
 * Generate test cases from business rules
 */
export const generateTestCasesTool: AgentTool = {
  name: 'generate_test_cases',
  description: 'Generate comprehensive test cases from business rule specifications.',
  parameters: {
    rule: {
      type: 'object',
      description: 'Business rule to generate tests for',
      required: true,
    },
    strategies: {
      type: 'array',
      description: 'Test generation strategies to use',
      required: true,
    },
    max_cases: {
      type: 'number',
      description: 'Maximum number of test cases to generate',
      required: false,
    },
  },
  execute: async (args) => {
    const rule = args.rule as { inputs: Array<{ name: string; type: string }> };
    const strategies = args.strategies as string[];
    const maxCases = (args.max_cases as number) || 10;
    
    const testCases = [];
    
    for (const input of rule.inputs) {
      if (strategies.includes('boundary') && input.type === 'decimal') {
        testCases.push(
          { inputs: { [input.name]: 0 }, description: `${input.name} at zero` },
          { inputs: { [input.name]: 0.01 }, description: `${input.name} minimal positive` },
          { inputs: { [input.name]: 999999.99 }, description: `${input.name} near max` }
        );
      }
      
      if (strategies.includes('partition')) {
        testCases.push(
          { inputs: { [input.name]: 100 }, description: `${input.name} typical value` },
          { inputs: { [input.name]: 1000 }, description: `${input.name} larger value` }
        );
      }
    }
    
    return { testCases: testCases.slice(0, maxCases) };
  },
};

/**
 * Compare outputs for equivalence
 */
export const compareOutputsTool: AgentTool = {
  name: 'compare_outputs',
  description: 'Compare legacy and modern system outputs for equivalence.',
  parameters: {
    legacy_output: {
      type: 'object',
      description: 'Output from legacy system',
      required: true,
    },
    modern_output: {
      type: 'object',
      description: 'Output from modern system',
      required: true,
    },
    tolerance: {
      type: 'number',
      description: 'Numeric tolerance for comparison',
      required: false,
    },
  },
  execute: async (args) => {
    const legacy = args.legacy_output as Record<string, unknown>;
    const modern = args.modern_output as Record<string, unknown>;
    const tolerance = (args.tolerance as number) || 0.01;
    
    const differences: Array<{
      field: string;
      legacy: unknown;
      modern: unknown;
      significant: boolean;
    }> = [];
    
    for (const key of Object.keys(legacy)) {
      const legacyVal = legacy[key];
      const modernVal = modern[key];
      
      if (typeof legacyVal === 'number' && typeof modernVal === 'number') {
        if (Math.abs(legacyVal - modernVal) > tolerance) {
          differences.push({
            field: key,
            legacy: legacyVal,
            modern: modernVal,
            significant: Math.abs(legacyVal - modernVal) > tolerance * 10,
          });
        }
      } else if (legacyVal !== modernVal) {
        differences.push({
          field: key,
          legacy: legacyVal,
          modern: modernVal,
          significant: true,
        });
      }
    }
    
    return {
      equivalent: differences.length === 0,
      differences,
      significantDifferences: differences.filter(d => d.significant).length,
    };
  },
};

/**
 * Execute property-based tests
 */
export const propertyTestTool: AgentTool = {
  name: 'property_test',
  description: 'Run property-based tests with random inputs.',
  parameters: {
    input_spec: {
      type: 'object',
      description: 'Specification for input generation',
      required: true,
    },
    iterations: {
      type: 'number',
      description: 'Number of random tests to run',
      required: false,
    },
  },
  execute: async (args) => {
    const spec = args.input_spec as Record<string, { type: string; min?: number; max?: number }>;
    const iterations = (args.iterations as number) || 100;
    
    const generatedInputs = [];
    
    for (let i = 0; i < iterations; i++) {
      const input: Record<string, unknown> = {};
      
      for (const [key, config] of Object.entries(spec)) {
        if (config.type === 'number' || config.type === 'decimal') {
          const min = config.min || 0;
          const max = config.max || 1000000;
          input[key] = min + Math.random() * (max - min);
        } else if (config.type === 'integer') {
          const min = config.min || 0;
          const max = config.max || 1000;
          input[key] = Math.floor(min + Math.random() * (max - min));
        } else if (config.type === 'string') {
          input[key] = `test_${i}`;
        }
      }
      
      generatedInputs.push(input);
    }
    
    return { inputs: generatedInputs };
  },
};

/**
 * Analyze test coverage
 */
export const analyzeCoverageTool: AgentTool = {
  name: 'analyze_coverage',
  description: 'Analyze test coverage for business rules and code paths.',
  parameters: {
    test_cases: {
      type: 'array',
      description: 'Test cases executed',
      required: true,
    },
    business_rules: {
      type: 'array',
      description: 'Business rules to check coverage for',
      required: true,
    },
  },
  execute: async (args) => {
    const testCases = args.test_cases as Array<{ ruleId?: string }>;
    const businessRules = args.business_rules as Array<{ id: string }>;
    
    const coveredRules = new Set(testCases.filter(t => t.ruleId).map(t => t.ruleId));
    const totalRules = businessRules.length;
    const coveredCount = businessRules.filter(r => coveredRules.has(r.id)).length;
    
    return {
      totalRules,
      coveredRules: coveredCount,
      coveragePercent: totalRules > 0 ? (coveredCount / totalRules) * 100 : 0,
      uncoveredRules: businessRules.filter(r => !coveredRules.has(r.id)).map(r => r.id),
    };
  },
};

/**
 * Calculate confidence score
 */
export const calculateConfidenceTool: AgentTool = {
  name: 'calculate_confidence',
  description: 'Calculate overall migration confidence score.',
  parameters: {
    passed_tests: {
      type: 'number',
      description: 'Number of passed tests',
      required: true,
    },
    total_tests: {
      type: 'number',
      description: 'Total number of tests',
      required: true,
    },
    critical_failures: {
      type: 'number',
      description: 'Number of critical test failures',
      required: true,
    },
    rule_coverage: {
      type: 'number',
      description: 'Business rule coverage percentage',
      required: true,
    },
  },
  execute: async (args) => {
    const passed = args.passed_tests as number;
    const total = args.total_tests as number;
    const criticalFailures = args.critical_failures as number;
    const ruleCoverage = args.rule_coverage as number;
    
    // Calculate confidence components
    const equivalenceScore = total > 0 ? (passed / total) * 100 : 0;
    const coverageScore = ruleCoverage;
    const criticalityPenalty = criticalFailures * 10;
    
    // Overall confidence
    let confidence = (equivalenceScore * 0.6 + coverageScore * 0.4) - criticalityPenalty;
    confidence = Math.max(0, Math.min(100, confidence));
    
    // Determine readiness
    const readyForProduction = 
      equivalenceScore >= 99 &&
      criticalFailures === 0 &&
      ruleCoverage >= 95;
    
    return {
      confidence,
      equivalenceScore,
      coverageScore,
      readyForProduction,
      blockers: [
        ...(equivalenceScore < 99 ? [`Equivalence score ${equivalenceScore.toFixed(1)}% below 99% threshold`] : []),
        ...(criticalFailures > 0 ? [`${criticalFailures} critical test failures`] : []),
        ...(ruleCoverage < 95 ? [`Rule coverage ${ruleCoverage.toFixed(1)}% below 95% threshold`] : []),
      ],
    };
  },
};

export const validatorTools: AgentTool[] = [
  generateTestCasesTool,
  compareOutputsTool,
  propertyTestTool,
  analyzeCoverageTool,
  calculateConfidenceTool,
];
