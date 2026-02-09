/**
 * Regression Manager API Routes
 * Automated regression test suite management
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { RegressionManager, TestGenerator, CodeGenerator } from '@migrationpilot/regression-manager';
import type { GenerationConfig, TestFramework } from '@migrationpilot/regression-manager';
import type { BusinessRule } from '@migrationpilot/core';

export const regressionManagerRoutes = new Hono();

// Create manager instance
const manager = new RegressionManager();

// In-memory cache for demo rules (would be database in production)
const rulesCache = new Map<string, BusinessRule[]>();

// ============================================================================
// SCHEMA DEFINITIONS
// ============================================================================

const CreateSuiteSchema = z.object({
  projectId: z.string(),
  name: z.string(),
});

const GenerateTestsSchema = z.object({
  targetLanguage: z.string().optional(),
  targetFramework: z.enum(['jest', 'vitest', 'mocha', 'pytest', 'junit', 'nunit', 'xunit']).optional(),
  generateBoundaryTests: z.boolean().optional(),
  generateEquivalenceTests: z.boolean().optional(),
  generateNegativeTests: z.boolean().optional(),
  maxTestsPerRule: z.number().optional(),
  includeEdgeCases: z.boolean().optional(),
  includeAssertions: z.boolean().optional(),
  includeDocumentation: z.boolean().optional(),
});

const CompareRunsSchema = z.object({
  legacyRunId: z.string(),
  modernRunId: z.string(),
});

// ============================================================================
// ROUTES
// ============================================================================

/**
 * POST /api/regression/suites
 * Create a new test suite
 */
regressionManagerRoutes.post('/suites', async (c) => {
  const body = await c.req.json();
  const parsed = CreateSuiteSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { projectId, name } = parsed.data;

    // Get or create demo rules
    let rules = rulesCache.get(projectId);
    if (!rules) {
      rules = createDemoRules(projectId);
      rulesCache.set(projectId, rules);
    }

    const suite = await manager.createSuite(projectId, name, rules);

    return c.json({
      data: {
        id: suite.id,
        name: suite.name,
        projectId: suite.projectId,
        testCaseCount: suite.testCases.length,
        coverage: suite.coverage,
        status: suite.status,
        createdAt: suite.createdAt,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to create suite',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/regression/suites/:projectId
 * List all suites for a project
 */
regressionManagerRoutes.get('/suites/:projectId', (c) => {
  const { projectId } = c.req.param();

  const suites = manager.listSuites(projectId);

  return c.json({
    data: {
      projectId,
      suites: suites.map(s => ({
        id: s.id,
        name: s.name,
        testCaseCount: s.testCases.length,
        coverage: s.coverage.rulesCoveragePercent,
        status: s.status,
        lastRun: s.runHistory[s.runHistory.length - 1]?.completedAt || null,
      })),
      count: suites.length,
    },
  });
});

/**
 * GET /api/regression/suite/:suiteId
 * Get a specific suite
 */
regressionManagerRoutes.get('/suite/:suiteId', (c) => {
  const { suiteId } = c.req.param();

  const suite = manager.getSuite(suiteId);
  if (!suite) {
    return c.json({ error: 'Suite not found' }, 404);
  }

  return c.json({
    data: {
      id: suite.id,
      projectId: suite.projectId,
      name: suite.name,
      description: suite.description,
      testCases: suite.testCases.map(tc => ({
        id: tc.id,
        name: tc.name,
        type: tc.type,
        priority: tc.priority,
        status: tc.status,
        relatedRuleIds: tc.relatedRuleIds,
      })),
      coverage: suite.coverage,
      status: suite.status,
      createdAt: suite.createdAt,
      updatedAt: suite.updatedAt,
      runCount: suite.runHistory.length,
    },
  });
});

/**
 * POST /api/regression/suite/:suiteId/generate
 * Generate test cases for a suite
 */
regressionManagerRoutes.post('/suite/:suiteId/generate', async (c) => {
  const { suiteId } = c.req.param();
  const body = await c.req.json();
  const parsed = GenerateTestsSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const suite = manager.getSuite(suiteId);
    if (!suite) {
      return c.json({ error: 'Suite not found' }, 404);
    }

    // Get rules for the project
    let rules = rulesCache.get(suite.projectId);
    if (!rules) {
      rules = createDemoRules(suite.projectId);
      rulesCache.set(suite.projectId, rules);
    }

    const config: GenerationConfig = {
      targetLanguage: parsed.data.targetLanguage || 'typescript',
      targetFramework: (parsed.data.targetFramework || 'vitest') as TestFramework,
      generateBoundaryTests: parsed.data.generateBoundaryTests ?? true,
      generateEquivalenceTests: parsed.data.generateEquivalenceTests ?? true,
      generateNegativeTests: parsed.data.generateNegativeTests ?? true,
      maxTestsPerRule: parsed.data.maxTestsPerRule ?? 10,
      includeEdgeCases: parsed.data.includeEdgeCases ?? true,
      includeAssertions: parsed.data.includeAssertions ?? true,
      includeDocumentation: parsed.data.includeDocumentation ?? true,
    };

    const result = await manager.generateTestCases(suiteId, rules, config);

    return c.json({
      data: {
        suiteId: result.suiteId,
        testCasesGenerated: result.testCasesGenerated,
        testCasesByRule: result.testCasesByRule,
        coverage: result.coverage,
        warnings: result.warnings,
        generatedFileCount: result.generatedFiles.length,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to generate tests',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/regression/suite/:suiteId/run
 * Run a test suite
 */
regressionManagerRoutes.post('/suite/:suiteId/run', async (c) => {
  const { suiteId } = c.req.param();

  try {
    const run = await manager.runSuite(suiteId);

    return c.json({
      data: {
        runId: run.id,
        suiteId: run.suiteId,
        status: run.status,
        summary: run.summary,
        startedAt: run.startedAt,
        completedAt: run.completedAt,
        environment: run.environment,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to run suite',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/regression/run/:runId
 * Get a specific run
 */
regressionManagerRoutes.get('/run/:runId', (c) => {
  const { runId } = c.req.param();

  const run = manager.getRun(runId);
  if (!run) {
    return c.json({ error: 'Run not found' }, 404);
  }

  return c.json({
    data: {
      id: run.id,
      suiteId: run.suiteId,
      status: run.status,
      summary: run.summary,
      results: run.results.map(r => ({
        testCaseId: r.testCaseId,
        status: r.status,
        duration: r.duration,
        error: r.error?.message || null,
      })),
      startedAt: run.startedAt,
      completedAt: run.completedAt,
      environment: run.environment,
    },
  });
});

/**
 * POST /api/regression/compare
 * Compare two test runs
 */
regressionManagerRoutes.post('/compare', async (c) => {
  const body = await c.req.json();
  const parsed = CompareRunsSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const { legacyRunId, modernRunId } = parsed.data;
    const comparison = await manager.compareRuns(legacyRunId, modernRunId);

    return c.json({
      data: {
        id: comparison.id,
        legacyRunId: comparison.legacyRunId,
        modernRunId: comparison.modernRunId,
        summary: comparison.summary,
        matches: comparison.matches.length,
        mismatches: comparison.mismatches.map(m => ({
          testCaseId: m.testCaseId,
          severity: m.severity,
          differenceCount: m.differences.length,
        })),
        timestamp: comparison.timestamp,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to compare runs',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/regression/suite/:suiteId/coverage
 * Get coverage metrics for a suite
 */
regressionManagerRoutes.get('/suite/:suiteId/coverage', (c) => {
  const { suiteId } = c.req.param();

  try {
    const coverage = manager.getCoverage(suiteId);

    return c.json({
      data: {
        suiteId,
        coverage,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to get coverage',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/regression/suite/:suiteId/export
 * Export a test suite
 */
regressionManagerRoutes.get('/suite/:suiteId/export', (c) => {
  const { suiteId } = c.req.param();
  const format = c.req.query('format') as 'json' | 'yaml' | 'code' || 'json';

  try {
    const exported = manager.exportSuite(suiteId, format);

    const contentType = format === 'yaml' ? 'text/yaml' :
                        format === 'code' ? 'text/plain' :
                        'application/json';

    return c.text(exported, 200, {
      'Content-Type': contentType,
      'Content-Disposition': `attachment; filename="suite_${suiteId}.${format === 'code' ? 'txt' : format}"`,
    });
  } catch (error) {
    return c.json({
      error: 'Failed to export suite',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/regression/generate-code
 * Generate test code without saving to a suite
 */
regressionManagerRoutes.post('/generate-code', async (c) => {
  const body = await c.req.json();
  const { projectId, language = 'typescript', framework = 'vitest' } = body;

  if (!projectId) {
    return c.json({ error: 'projectId required' }, 400);
  }

  try {
    // Get rules
    let rules = rulesCache.get(projectId);
    if (!rules) {
      rules = createDemoRules(projectId);
      rulesCache.set(projectId, rules);
    }

    // Generate test cases
    const testGenerator = new TestGenerator();
    const testCases = testGenerator.generateTestCases(rules);

    // Generate code
    const codeGenerator = new CodeGenerator(language, framework as TestFramework);
    const files = codeGenerator.generateCode(testCases);

    return c.json({
      data: {
        projectId,
        language,
        framework,
        testCasesGenerated: testCases.length,
        files: files.map(f => ({
          path: f.path,
          testCaseCount: f.testCaseIds.length,
          sizeBytes: f.content.length,
        })),
        code: files,
      },
    });
  } catch (error) {
    return c.json({
      error: 'Failed to generate code',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/regression/frameworks
 * Get list of supported test frameworks
 */
regressionManagerRoutes.get('/frameworks', (c) => {
  return c.json({
    data: {
      frameworks: [
        { id: 'vitest', name: 'Vitest', language: 'typescript' },
        { id: 'jest', name: 'Jest', language: 'typescript' },
        { id: 'mocha', name: 'Mocha', language: 'javascript' },
        { id: 'pytest', name: 'Pytest', language: 'python' },
        { id: 'junit', name: 'JUnit', language: 'java' },
        { id: 'nunit', name: 'NUnit', language: 'csharp' },
        { id: 'xunit', name: 'xUnit', language: 'csharp' },
      ],
    },
  });
});

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function createDemoRules(projectId: string): BusinessRule[] {
  const now = new Date();
  return [
    {
      id: 'br_001',
      projectId,
      name: 'Interest Calculation',
      description: 'Calculate simple interest on loan principal',
      category: 'calculation',
      sourceFile: 'CALCINT.cbl',
      sourceLines: [100, 150],
      sourceCode: 'COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE * WS-YEARS',
      inputs: [
        { name: 'principal', type: 'decimal', source: 'Loan Application' },
        { name: 'rate', type: 'decimal', source: 'Rate Table' },
        { name: 'years', type: 'integer', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'interest', type: 'decimal', description: 'Calculated interest amount' },
      ],
      logic: 'Interest = Principal × Rate × Years',
      formula: 'I = P × R × T',
      edgeCases: ['Zero principal', 'Zero rate', 'Zero years'],
      assumptions: ['Rate is annual percentage as decimal'],
      confidence: 0.95,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_002',
      projectId,
      name: 'Loan Eligibility',
      description: 'Determine if customer is eligible for loan',
      category: 'validation',
      sourceFile: 'LOANELIG.cbl',
      sourceLines: [200, 280],
      sourceCode: 'IF WS-CREDIT-SCORE >= 650 AND WS-INCOME >= WS-LOAN-AMOUNT / 3',
      inputs: [
        { name: 'creditScore', type: 'integer', source: 'Credit Bureau' },
        { name: 'income', type: 'decimal', source: 'Customer Application' },
        { name: 'loanAmount', type: 'decimal', source: 'Loan Application' },
      ],
      outputs: [
        { name: 'eligible', type: 'boolean', description: 'Eligibility result' },
      ],
      logic: 'Eligible if credit score >= 650 AND income >= loan amount / 3',
      edgeCases: ['Credit score exactly 650', 'Income exactly 1/3 of loan', 'No credit history'],
      assumptions: ['Credit score from primary bureau'],
      confidence: 0.92,
      reviewStatus: 'approved',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
    {
      id: 'br_003',
      projectId,
      name: 'Risk Assessment',
      description: 'Calculate risk score based on multiple factors',
      category: 'calculation',
      sourceFile: 'RISKCALC.cbl',
      sourceLines: [300, 400],
      sourceCode: 'COMPUTE WS-RISK-SCORE = (1000 - WS-CREDIT-SCORE) / 10 + WS-DTI-RATIO * 5',
      inputs: [
        { name: 'creditScore', type: 'integer', source: 'Credit Bureau' },
        { name: 'dtiRatio', type: 'decimal', source: 'Calculated' },
      ],
      outputs: [
        { name: 'riskScore', type: 'decimal', description: 'Risk score 0-100' },
      ],
      logic: 'Risk Score = (1000 - Credit Score) / 10 + DTI Ratio × 5',
      formula: 'R = (1000 - C) / 10 + D × 5',
      edgeCases: ['Perfect credit score (850)', 'Minimum credit score (300)', 'DTI over 100%'],
      assumptions: ['Credit score range 300-850'],
      confidence: 0.88,
      reviewStatus: 'pending',
      extractedAt: now,
      extractedBy: 'ArcheologistAgent',
      version: 1,
    },
  ];
}
