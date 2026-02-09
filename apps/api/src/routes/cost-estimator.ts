/**
 * Cost Estimator API Routes
 * Self-service migration cost estimation endpoints
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { CostEstimatorService } from '@migrationpilot/cost-estimator';
import type { SourceLanguage, TargetLanguage } from '@migrationpilot/core';

export const costEstimatorRoutes = new Hono();

const estimatorService = new CostEstimatorService();

// Schema for quick estimate
const QuickEstimateSchema = z.object({
  code: z.string().min(1).max(100000),
  sourceLanguage: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']).optional(),
});

// Schema for full estimate
const FullEstimateSchema = z.object({
  code: z.union([
    z.string(),
    z.array(z.object({
      content: z.string(),
      filename: z.string().optional(),
      language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']).optional(),
    })),
  ]),
  sourceLanguage: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']).optional(),
  targetLanguage: z.enum(['java', 'python', 'typescript', 'go', 'csharp']).optional(),
  targetFramework: z.string().optional(),
  options: z.object({
    includeTestGeneration: z.boolean().optional(),
    includeDocumentation: z.boolean().optional(),
    humanReviewLevel: z.enum(['minimal', 'standard', 'comprehensive']).optional(),
    deploymentComplexity: z.enum(['simple', 'standard', 'enterprise']).optional(),
    teamExperience: z.enum(['novice', 'intermediate', 'expert']).optional(),
    currency: z.string().optional(),
  }).optional(),
});

// Schema for batch estimate
const BatchEstimateSchema = z.object({
  files: z.array(z.object({
    content: z.string(),
    filename: z.string().optional(),
    language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']).optional(),
  })).min(1).max(100),
  options: FullEstimateSchema.shape.options,
});

/**
 * POST /api/cost-estimator/quick
 * Quick estimate for freemium tier - minimal analysis, instant response
 */
costEstimatorRoutes.post('/quick', async (c) => {
  const body = await c.req.json();
  const parsed = QuickEstimateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const result = await estimatorService.quickEstimate({
      code: parsed.data.code,
      sourceLanguage: parsed.data.sourceLanguage as SourceLanguage,
    });

    return c.json({ data: result });
  } catch (error) {
    return c.json({
      error: 'Estimation failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/cost-estimator/estimate
 * Full detailed estimate with breakdown
 */
costEstimatorRoutes.post('/estimate', async (c) => {
  const body = await c.req.json();
  const parsed = FullEstimateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    // Convert string code to CodeSample format
    const code = typeof parsed.data.code === 'string'
      ? { content: parsed.data.code }
      : parsed.data.code;

    const result = await estimatorService.estimate({
      code,
      sourceLanguage: parsed.data.sourceLanguage as SourceLanguage,
      targetLanguage: parsed.data.targetLanguage as TargetLanguage,
      targetFramework: parsed.data.targetFramework,
      options: parsed.data.options,
    });

    return c.json({ data: result });
  } catch (error) {
    return c.json({
      error: 'Estimation failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * POST /api/cost-estimator/batch
 * Batch estimate for multiple files
 */
costEstimatorRoutes.post('/batch', async (c) => {
  const body = await c.req.json();
  const parsed = BatchEstimateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({
      error: 'Validation error',
      details: parsed.error.issues,
    }, 400);
  }

  try {
    const result = await estimatorService.batchEstimate({
      files: parsed.data.files.map(f => ({
        content: f.content,
        filename: f.filename,
        language: f.language as SourceLanguage,
      })),
      options: parsed.data.options,
    });

    return c.json({ data: result });
  } catch (error) {
    return c.json({
      error: 'Batch estimation failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, 500);
  }
});

/**
 * GET /api/cost-estimator/languages
 * Get supported source and target languages
 */
costEstimatorRoutes.get('/languages', (c) => {
  return c.json({
    data: {
      source: [
        { id: 'cobol', name: 'COBOL', dialects: ['COBOL-85', 'IBM Enterprise', 'Micro Focus', 'GnuCOBOL'] },
        { id: 'fortran', name: 'Fortran', dialects: ['F77', 'F90', 'F95'] },
        { id: 'vb6', name: 'Visual Basic 6', dialects: ['VB6'] },
        { id: 'vba', name: 'VBA', dialects: ['Excel VBA', 'Access VBA'] },
        { id: 'java-legacy', name: 'Legacy Java', dialects: ['J2EE', 'EJB 2.x', 'Struts 1.x'] },
      ],
      target: [
        { id: 'java', name: 'Java', frameworks: ['Spring Boot', 'Quarkus'] },
        { id: 'python', name: 'Python', frameworks: ['FastAPI', 'Django'] },
        { id: 'typescript', name: 'TypeScript', frameworks: ['NestJS', 'Express'] },
        { id: 'go', name: 'Go', frameworks: ['Standard Library', 'Gin'] },
        { id: 'csharp', name: 'C#', frameworks: ['.NET Core', 'ASP.NET Core'] },
      ],
    },
  });
});

/**
 * GET /api/cost-estimator/config
 * Get estimator configuration (for transparency)
 */
costEstimatorRoutes.get('/config', (c) => {
  return c.json({
    data: {
      baseRates: {
        description: 'Base person-hours per line of code by language',
        values: {
          cobol: '0.08 hours/LOC',
          fortran: '0.07 hours/LOC',
          vb6: '0.06 hours/LOC',
          vba: '0.05 hours/LOC',
          'java-legacy': '0.04 hours/LOC',
        },
      },
      complexityMultipliers: {
        description: 'Effort multipliers by complexity level',
        values: {
          trivial: '0.5x',
          low: '0.75x',
          medium: '1.0x',
          high: '1.5x',
          very_high: '2.0x',
          extreme: '3.0x',
        },
      },
      methodology: 'Estimates based on analysis of code volume, cyclomatic complexity, data structures, external integrations, and language-specific factors',
      contingency: '20% default contingency included',
    },
  });
});
