/**
 * Regression Test Generator API Routes
 * 
 * Endpoints for generating test suites from production data
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { RegressionTestGenerator, type ProductionLogEntry, type GoldenDataset, type TestCase } from '@migrationpilot/testing';
import { generateId } from '@migrationpilot/core';

export const testGeneratorRoutes = new Hono();

// Store golden datasets in memory (in production, use database)
const goldenDatasets = new Map<string, GoldenDataset>();

// Type for changed behaviors from drift detection
interface ChangedBehavior {
  testCase: TestCase;
}

const ProductionLogSchema = z.object({
  timestamp: z.string(),
  transactionId: z.string(),
  programName: z.string(),
  inputs: z.record(z.unknown()),
  outputs: z.record(z.unknown()),
  executionTimeMs: z.number().optional().default(0),
  metadata: z.record(z.unknown()).optional(),
});

const GenerateFromLogsSchema = z.object({
  logs: z.array(ProductionLogSchema).min(1),
  rules: z.array(z.object({
    id: z.string(),
    name: z.string(),
    description: z.string(),
    inputs: z.array(z.object({
      name: z.string(),
      type: z.string(),
      description: z.string().optional(),
      constraints: z.object({
        min: z.number().optional(),
        max: z.number().optional(),
        enum: z.array(z.string()).optional(),
      }).optional(),
    })).optional(),
    outputs: z.array(z.object({
      name: z.string(),
      type: z.string(),
      description: z.string().optional(),
    })).optional(),
    edgeCases: z.array(z.string()).optional(),
    confidence: z.number().min(0).max(1),
  })).optional().default([]),
  config: z.object({
    maxTestCases: z.number().int().min(1).max(10000).optional(),
    minCoverage: z.number().min(0).max(1).optional(),
    deduplication: z.boolean().optional(),
    diversityBias: z.number().min(0).max(1).optional(),
    anonymization: z.object({
      fields: z.record(z.enum(['hash', 'mask', 'replace', 'tokenize', 'generalize', 'synthetic'])),
      preserveFormat: z.boolean().default(true),
    }).optional(),
    tolerance: z.object({
      numeric: z.number().optional(),
      string: z.enum(['exact', 'trim', 'case-insensitive']).optional(),
      date: z.number().optional(),
      array: z.enum(['ordered', 'unordered']).optional(),
    }).optional(),
  }).optional(),
  projectId: z.string().optional(),
});

const GenerateFromBatchSchema = z.object({
  data: z.array(z.object({
    inputs: z.record(z.unknown()),
    outputs: z.record(z.unknown()),
  })).min(1),
  rules: z.array(z.any()).optional().default([]),
  config: z.object({
    maxTestCases: z.number().int().min(1).max(10000).optional(),
  }).optional(),
  projectId: z.string().optional(),
});

const GenerateFromJCLSchema = z.object({
  jclOutput: z.string().min(1),
  programName: z.string().optional(),
  rules: z.array(z.any()).optional().default([]),
  config: z.object({
    maxTestCases: z.number().int().min(1).max(10000).optional(),
  }).optional(),
  projectId: z.string().optional(),
});

const DetectDriftSchema = z.object({
  datasetId: z.string(),
  newLogs: z.array(ProductionLogSchema).min(1),
  threshold: z.number().min(0).max(1).optional().default(0.1),
});

/**
 * POST /generate/from-logs
 * Generate test cases from production logs
 */
testGeneratorRoutes.post('/generate/from-logs', async (c) => {
  const body = await c.req.json();
  const parsed = GenerateFromLogsSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const generator = new RegressionTestGenerator();
    const dataset = generator.generateFromLogs(
      parsed.data.logs as ProductionLogEntry[],
      parsed.data.rules,
      parsed.data.config
    );

    // Set project ID
    dataset.projectId = parsed.data.projectId || generateId();

    // Store the dataset
    goldenDatasets.set(dataset.id, dataset);

    return c.json({
      data: {
        datasetId: dataset.id,
        testCasesGenerated: dataset.testCases.length,
        statistics: dataset.statistics,
        sourceMetadata: dataset.sourceMetadata,
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Generation failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /generate/from-batch
 * Generate test cases from batch/CSV data
 */
testGeneratorRoutes.post('/generate/from-batch', async (c) => {
  const body = await c.req.json();
  const parsed = GenerateFromBatchSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const generator = new RegressionTestGenerator();
    const testCases = generator.generateFromBatchData(
      parsed.data.data,
      parsed.data.rules,
      parsed.data.config
    );

    // Create a minimal dataset for batch data
    const dataset: GoldenDataset = {
      id: `gd_${generateId()}`,
      name: `Batch Dataset - ${new Date().toISOString().split('T')[0]}`,
      description: `Generated from ${parsed.data.data.length} batch records`,
      projectId: parsed.data.projectId || generateId(),
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
      version: 1,
      testCases,
      sourceMetadata: {
        logCount: parsed.data.data.length,
        dateRange: { start: new Date().toISOString(), end: new Date().toISOString() },
        uniqueInputPatterns: testCases.length,
        anonymized: false,
      },
      statistics: {
        totalCases: testCases.length,
        byPriority: { medium: testCases.length },
        byStrategy: { 'historical-replay': testCases.length },
        inputCoverage: 1,
        outputCoverage: 1,
      },
    };

    goldenDatasets.set(dataset.id, dataset);

    return c.json({
      data: {
        datasetId: dataset.id,
        testCasesGenerated: testCases.length,
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Generation failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /generate/from-jcl
 * Generate test cases from JCL job output
 */
testGeneratorRoutes.post('/generate/from-jcl', async (c) => {
  const body = await c.req.json();
  const parsed = GenerateFromJCLSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const generator = new RegressionTestGenerator();
    const testCases = generator.generateFromJCLLogs(
      parsed.data.jclOutput,
      parsed.data.programName || '',
      parsed.data.rules,
      parsed.data.config
    );

    return c.json({
      data: {
        testCasesGenerated: testCases.length,
        testCases: testCases.slice(0, 100), // Return first 100
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Generation failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * GET /datasets
 * List all golden datasets
 */
testGeneratorRoutes.get('/datasets', async (c) => {
  const datasets = Array.from(goldenDatasets.values()).map(d => ({
    id: d.id,
    name: d.name,
    projectId: d.projectId,
    createdAt: d.createdAt,
    testCaseCount: d.testCases.length,
    statistics: d.statistics,
  }));

  return c.json({ data: datasets });
});

/**
 * GET /datasets/:id
 * Get a specific golden dataset
 */
testGeneratorRoutes.get('/datasets/:id', async (c) => {
  const datasetId = c.req.param('id');
  const dataset = goldenDatasets.get(datasetId);

  if (!dataset) {
    return c.json({ error: 'Dataset not found' }, 404);
  }

  return c.json({ data: dataset });
});

/**
 * GET /datasets/:id/test-cases
 * Get test cases from a dataset with pagination
 */
testGeneratorRoutes.get('/datasets/:id/test-cases', async (c) => {
  const datasetId = c.req.param('id');
  const dataset = goldenDatasets.get(datasetId);

  if (!dataset) {
    return c.json({ error: 'Dataset not found' }, 404);
  }

  const page = parseInt(c.req.query('page') || '1', 10);
  const pageSize = parseInt(c.req.query('pageSize') || '50', 10);
  const priority = c.req.query('priority');
  const strategy = c.req.query('strategy');

  let filtered: TestCase[] = dataset.testCases;

  if (priority) {
    filtered = filtered.filter((tc: TestCase) => tc.priority === priority);
  }

  if (strategy) {
    filtered = filtered.filter((tc: TestCase) => tc.generationStrategy === strategy);
  }

  const start = (page - 1) * pageSize;
  const end = start + pageSize;
  const paginated = filtered.slice(start, end);

  return c.json({
    data: {
      testCases: paginated,
      pagination: {
        page,
        pageSize,
        total: filtered.length,
        totalPages: Math.ceil(filtered.length / pageSize),
      },
    },
  });
});

/**
 * POST /datasets/:id/drift-check
 * Check for drift in a golden dataset
 */
testGeneratorRoutes.post('/datasets/:id/drift-check', async (c) => {
  const datasetId = c.req.param('id');
  const dataset = goldenDatasets.get(datasetId);

  if (!dataset) {
    return c.json({ error: 'Dataset not found' }, 404);
  }

  const body = await c.req.json();
  const parsed = DetectDriftSchema.safeParse({ ...body, datasetId });

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const generator = new RegressionTestGenerator();
    const driftResult = generator.detectDrift(
      dataset,
      parsed.data.newLogs as ProductionLogEntry[],
      parsed.data.threshold
    );

    return c.json({
      data: {
        hasDrift: driftResult.hasDrift,
        newPatternsCount: driftResult.newPatterns.length,
        changedBehaviorsCount: driftResult.changedBehaviors.length,
        recommendation: driftResult.recommendation,
        details: {
          newPatterns: driftResult.newPatterns.slice(0, 10),
          changedBehaviors: driftResult.changedBehaviors.slice(0, 10).map((cb: ChangedBehavior) => ({
            testCaseId: cb.testCase.id,
            testCaseName: cb.testCase.name,
          })),
        },
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Drift check failed',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * DELETE /datasets/:id
 * Delete a golden dataset
 */
testGeneratorRoutes.delete('/datasets/:id', async (c) => {
  const datasetId = c.req.param('id');

  if (goldenDatasets.has(datasetId)) {
    goldenDatasets.delete(datasetId);
    return c.json({ data: { message: 'Dataset deleted successfully' } });
  }

  return c.json({ error: 'Dataset not found' }, 404);
});

/**
 * POST /datasets/:id/export
 * Export a golden dataset in various formats
 */
testGeneratorRoutes.post('/datasets/:id/export', async (c) => {
  const datasetId = c.req.param('id');
  const dataset = goldenDatasets.get(datasetId);

  if (!dataset) {
    return c.json({ error: 'Dataset not found' }, 404);
  }

  const body = await c.req.json();
  const format = body.format || 'json';

  switch (format) {
    case 'json':
      return c.json({ data: dataset });
    
    case 'csv': {
      const headers = ['id', 'name', 'inputs', 'expectedOutput', 'priority', 'strategy'];
      const rows = dataset.testCases.map((tc: TestCase) => [
        tc.id,
        tc.name,
        JSON.stringify(tc.inputs),
        JSON.stringify(tc.expectedOutput),
        tc.priority,
        tc.generationStrategy,
      ]);
      const csv = [headers.join(','), ...rows.map((r: string[]) => r.join(','))].join('\n');
      
      c.header('Content-Type', 'text/csv');
      c.header('Content-Disposition', `attachment; filename="${dataset.id}.csv"`);
      return c.body(csv);
    }
    
    case 'junit': {
      // Generate JUnit XML format
      const xml = `<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="${dataset.name}" tests="${dataset.testCases.length}">
  <testsuite name="RegressionTests" tests="${dataset.testCases.length}">
    ${dataset.testCases.map((tc: TestCase) => `
    <testcase name="${tc.name}" classname="RegressionTests">
      <properties>
        <property name="inputs" value="${encodeURIComponent(JSON.stringify(tc.inputs))}"/>
        <property name="expected" value="${encodeURIComponent(JSON.stringify(tc.expectedOutput))}"/>
      </properties>
    </testcase>`).join('')}
  </testsuite>
</testsuites>`;
      
      c.header('Content-Type', 'application/xml');
      c.header('Content-Disposition', `attachment; filename="${dataset.id}.xml"`);
      return c.body(xml);
    }
    
    default:
      return c.json({ error: `Unsupported format: ${format}` }, 400);
  }
});
