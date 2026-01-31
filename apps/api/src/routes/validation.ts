/**
 * Incremental Validation API Routes
 * 
 * Real-time validation during migration that catches regressions immediately.
 * Supports continuous validation as code changes.
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { generateId } from '@migrationpilot/core';

export const validationRoutes = new Hono();

// Types for validation
interface ValidationSession {
  id: string;
  projectId: string;
  status: 'active' | 'paused' | 'completed';
  config: ValidationConfig;
  statistics: ValidationStatistics;
  results: ValidationResult[];
  createdAt: string;
  updatedAt: string;
}

interface ValidationConfig {
  continuousMode: boolean;
  tolerance: {
    numeric: number;
    string: 'exact' | 'trim' | 'case-insensitive';
  };
  stopOnFailure: boolean;
  parallelExecutions: number;
  retryOnFailure: number;
}

interface ValidationStatistics {
  totalValidations: number;
  passed: number;
  failed: number;
  skipped: number;
  averageExecutionTimeMs: number;
  lastValidationAt: string | null;
  regressionsCaught: number;
}

interface ValidationResult {
  id: string;
  testCaseId: string;
  testCaseName: string;
  status: 'passed' | 'failed' | 'skipped' | 'error';
  legacyOutput: unknown;
  modernOutput: unknown;
  differences?: Array<{
    path: string;
    expected: unknown;
    actual: unknown;
    severity: 'critical' | 'warning' | 'info';
  }>;
  executionTimeMs: number;
  timestamp: string;
}

interface IncrementalValidationRequest {
  moduleId: string;
  testCases: Array<{
    id: string;
    name: string;
    inputs: Record<string, unknown>;
    expectedOutput?: unknown;
  }>;
  legacyEndpoint?: string;
  modernEndpoint?: string;
}

// In-memory storage (use Redis in production)
const validationSessions = new Map<string, ValidationSession>();
const validationQueues = new Map<string, IncrementalValidationRequest[]>();

const StartSessionSchema = z.object({
  projectId: z.string(),
  config: z.object({
    continuousMode: z.boolean().optional().default(false),
    tolerance: z.object({
      numeric: z.number().optional().default(0.001),
      string: z.enum(['exact', 'trim', 'case-insensitive']).optional().default('exact'),
    }).optional(),
    stopOnFailure: z.boolean().optional().default(false),
    parallelExecutions: z.number().int().min(1).max(10).optional().default(1),
    retryOnFailure: z.number().int().min(0).max(3).optional().default(0),
  }).optional(),
});

const ValidateSchema = z.object({
  sessionId: z.string(),
  moduleId: z.string(),
  testCases: z.array(z.object({
    id: z.string(),
    name: z.string(),
    inputs: z.record(z.unknown()),
    expectedOutput: z.unknown().optional(),
  })).min(1),
  legacyOutput: z.record(z.unknown()).optional(),
  modernOutput: z.record(z.unknown()).optional(),
});

const BatchValidateSchema = z.object({
  sessionId: z.string(),
  validations: z.array(z.object({
    testCaseId: z.string(),
    testCaseName: z.string(),
    legacyOutput: z.unknown(),
    modernOutput: z.unknown(),
  })).min(1),
});

/**
 * POST /sessions
 * Start a new validation session
 */
validationRoutes.post('/sessions', async (c) => {
  const body = await c.req.json();
  const parsed = StartSessionSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const session: ValidationSession = {
    id: `vs_${generateId()}`,
    projectId: parsed.data.projectId,
    status: 'active',
    config: {
      continuousMode: parsed.data.config?.continuousMode ?? false,
      tolerance: {
        numeric: parsed.data.config?.tolerance?.numeric ?? 0.001,
        string: parsed.data.config?.tolerance?.string ?? 'exact',
      },
      stopOnFailure: parsed.data.config?.stopOnFailure ?? false,
      parallelExecutions: parsed.data.config?.parallelExecutions ?? 1,
      retryOnFailure: parsed.data.config?.retryOnFailure ?? 0,
    },
    statistics: {
      totalValidations: 0,
      passed: 0,
      failed: 0,
      skipped: 0,
      averageExecutionTimeMs: 0,
      lastValidationAt: null,
      regressionsCaught: 0,
    },
    results: [],
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  };

  validationSessions.set(session.id, session);
  validationQueues.set(session.id, []);

  return c.json({
    data: {
      sessionId: session.id,
      status: session.status,
      config: session.config,
    },
  });
});

/**
 * GET /sessions/:id
 * Get session status and statistics
 */
validationRoutes.get('/sessions/:id', async (c) => {
  const sessionId = c.req.param('id');
  const session = validationSessions.get(sessionId);

  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  return c.json({
    data: {
      id: session.id,
      projectId: session.projectId,
      status: session.status,
      config: session.config,
      statistics: session.statistics,
      resultCount: session.results.length,
    },
  });
});

/**
 * POST /validate
 * Validate a single test case or batch incrementally
 */
validationRoutes.post('/validate', async (c) => {
  const body = await c.req.json();
  const parsed = ValidateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const session = validationSessions.get(parsed.data.sessionId);
  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  if (session.status !== 'active') {
    return c.json({ error: 'Session is not active' }, 400);
  }

  const results: ValidationResult[] = [];
  const startTime = Date.now();

  for (const testCase of parsed.data.testCases) {
    const result = performValidation(
      testCase,
      parsed.data.legacyOutput,
      parsed.data.modernOutput,
      session.config.tolerance
    );
    
    results.push(result);
    session.results.push(result);
    
    // Update statistics
    session.statistics.totalValidations++;
    if (result.status === 'passed') {
      session.statistics.passed++;
    } else if (result.status === 'failed') {
      session.statistics.failed++;
      session.statistics.regressionsCaught++;
      
      if (session.config.stopOnFailure) {
        break;
      }
    } else if (result.status === 'skipped') {
      session.statistics.skipped++;
    }
  }

  const totalTime = Date.now() - startTime;
  session.statistics.averageExecutionTimeMs = 
    (session.statistics.averageExecutionTimeMs * (session.statistics.totalValidations - results.length) + totalTime) 
    / session.statistics.totalValidations;
  session.statistics.lastValidationAt = new Date().toISOString();
  session.updatedAt = new Date().toISOString();

  // Check for regressions
  const regressions = results.filter(r => r.status === 'failed' && r.differences?.some(d => d.severity === 'critical'));

  return c.json({
    data: {
      results,
      summary: {
        total: results.length,
        passed: results.filter(r => r.status === 'passed').length,
        failed: results.filter(r => r.status === 'failed').length,
        regressions: regressions.length,
        executionTimeMs: totalTime,
      },
      sessionStatistics: session.statistics,
    },
  });
});

/**
 * POST /validate/batch
 * Validate multiple outputs at once
 */
validationRoutes.post('/validate/batch', async (c) => {
  const body = await c.req.json();
  const parsed = BatchValidateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const session = validationSessions.get(parsed.data.sessionId);
  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  const results: ValidationResult[] = [];
  // Note: timing is handled in compareOutputs individually

  for (const validation of parsed.data.validations) {
    const result = compareOutputs(
      validation.testCaseId,
      validation.testCaseName,
      validation.legacyOutput,
      validation.modernOutput,
      session.config.tolerance
    );
    
    results.push(result);
    session.results.push(result);
    
    session.statistics.totalValidations++;
    if (result.status === 'passed') session.statistics.passed++;
    else if (result.status === 'failed') session.statistics.failed++;
  }

  session.statistics.lastValidationAt = new Date().toISOString();
  session.updatedAt = new Date().toISOString();

  return c.json({
    data: {
      results,
      summary: {
        total: results.length,
        passed: results.filter(r => r.status === 'passed').length,
        failed: results.filter(r => r.status === 'failed').length,
      },
    },
  });
});

/**
 * GET /sessions/:id/results
 * Get validation results with filtering
 */
validationRoutes.get('/sessions/:id/results', async (c) => {
  const sessionId = c.req.param('id');
  const session = validationSessions.get(sessionId);

  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  const status = c.req.query('status');
  const limit = parseInt(c.req.query('limit') || '100', 10);
  const offset = parseInt(c.req.query('offset') || '0', 10);

  let results = session.results;

  if (status) {
    results = results.filter(r => r.status === status);
  }

  const paginated = results.slice(offset, offset + limit);

  return c.json({
    data: {
      results: paginated,
      pagination: {
        total: results.length,
        limit,
        offset,
        hasMore: offset + limit < results.length,
      },
    },
  });
});

/**
 * GET /sessions/:id/regressions
 * Get only failed validations (regressions)
 */
validationRoutes.get('/sessions/:id/regressions', async (c) => {
  const sessionId = c.req.param('id');
  const session = validationSessions.get(sessionId);

  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  const regressions = session.results.filter(
    r => r.status === 'failed' && r.differences && r.differences.length > 0
  );

  // Group by severity
  const bySeverity = {
    critical: regressions.filter(r => r.differences?.some(d => d.severity === 'critical')),
    warning: regressions.filter(r => !r.differences?.some(d => d.severity === 'critical') && r.differences?.some(d => d.severity === 'warning')),
    info: regressions.filter(r => r.differences?.every(d => d.severity === 'info')),
  };

  return c.json({
    data: {
      total: regressions.length,
      bySeverity: {
        critical: bySeverity.critical.length,
        warning: bySeverity.warning.length,
        info: bySeverity.info.length,
      },
      regressions: regressions.slice(0, 50), // Limit to first 50
    },
  });
});

/**
 * POST /sessions/:id/pause
 * Pause a validation session
 */
validationRoutes.post('/sessions/:id/pause', async (c) => {
  const sessionId = c.req.param('id');
  const session = validationSessions.get(sessionId);

  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  session.status = 'paused';
  session.updatedAt = new Date().toISOString();

  return c.json({ data: { message: 'Session paused', status: session.status } });
});

/**
 * POST /sessions/:id/resume
 * Resume a paused validation session
 */
validationRoutes.post('/sessions/:id/resume', async (c) => {
  const sessionId = c.req.param('id');
  const session = validationSessions.get(sessionId);

  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  if (session.status !== 'paused') {
    return c.json({ error: 'Session is not paused' }, 400);
  }

  session.status = 'active';
  session.updatedAt = new Date().toISOString();

  return c.json({ data: { message: 'Session resumed', status: session.status } });
});

/**
 * POST /sessions/:id/complete
 * Complete a validation session and generate report
 */
validationRoutes.post('/sessions/:id/complete', async (c) => {
  const sessionId = c.req.param('id');
  const session = validationSessions.get(sessionId);

  if (!session) {
    return c.json({ error: 'Session not found' }, 404);
  }

  session.status = 'completed';
  session.updatedAt = new Date().toISOString();

  // Generate summary report
  const report = {
    sessionId: session.id,
    projectId: session.projectId,
    completedAt: new Date().toISOString(),
    duration: {
      start: session.createdAt,
      end: session.updatedAt,
    },
    statistics: session.statistics,
    passRate: session.statistics.totalValidations > 0
      ? Math.round((session.statistics.passed / session.statistics.totalValidations) * 100)
      : 0,
    verdict: session.statistics.failed === 0 ? 'PASS' : 'FAIL',
    criticalRegressions: session.results.filter(
      r => r.status === 'failed' && r.differences?.some(d => d.severity === 'critical')
    ).length,
  };

  return c.json({ data: report });
});

/**
 * DELETE /sessions/:id
 * Delete a validation session
 */
validationRoutes.delete('/sessions/:id', async (c) => {
  const sessionId = c.req.param('id');

  if (validationSessions.has(sessionId)) {
    validationSessions.delete(sessionId);
    validationQueues.delete(sessionId);
    return c.json({ data: { message: 'Session deleted' } });
  }

  return c.json({ error: 'Session not found' }, 404);
});

// Helper functions

function performValidation(
  testCase: { id: string; name: string; inputs: Record<string, unknown>; expectedOutput?: unknown },
  legacyOutput: Record<string, unknown> | undefined,
  modernOutput: Record<string, unknown> | undefined,
  tolerance: { numeric: number; string: 'exact' | 'trim' | 'case-insensitive' }
): ValidationResult {
  const startTime = Date.now();

  // If we have expected output, compare modern against expected
  if (testCase.expectedOutput !== undefined && modernOutput) {
    return compareOutputs(
      testCase.id,
      testCase.name,
      testCase.expectedOutput,
      modernOutput,
      tolerance
    );
  }

  // If we have both legacy and modern, compare them
  if (legacyOutput && modernOutput) {
    return compareOutputs(
      testCase.id,
      testCase.name,
      legacyOutput,
      modernOutput,
      tolerance
    );
  }

  // Not enough data to validate
  return {
    id: `vr_${generateId()}`,
    testCaseId: testCase.id,
    testCaseName: testCase.name,
    status: 'skipped',
    legacyOutput,
    modernOutput,
    executionTimeMs: Date.now() - startTime,
    timestamp: new Date().toISOString(),
  };
}

function compareOutputs(
  testCaseId: string,
  testCaseName: string,
  expected: unknown,
  actual: unknown,
  tolerance: { numeric: number; string: 'exact' | 'trim' | 'case-insensitive' }
): ValidationResult {
  const startTime = Date.now();
  const differences: ValidationResult['differences'] = [];

  compareValues(expected, actual, '', differences, tolerance);

  const status = differences.length === 0 ? 'passed' : 'failed';
  // const hasCritical = differences.some(d => d.severity === 'critical');

  return {
    id: `vr_${generateId()}`,
    testCaseId,
    testCaseName,
    status,
    legacyOutput: expected,
    modernOutput: actual,
    differences: differences.length > 0 ? differences : undefined,
    executionTimeMs: Date.now() - startTime,
    timestamp: new Date().toISOString(),
  };
}

function compareValues(
  expected: unknown,
  actual: unknown,
  path: string,
  differences: NonNullable<ValidationResult['differences']>,
  tolerance: { numeric: number; string: 'exact' | 'trim' | 'case-insensitive' }
): void {
  // Handle null/undefined
  if (expected === null || expected === undefined) {
    if (actual !== null && actual !== undefined) {
      differences.push({
        path: path || 'root',
        expected,
        actual,
        severity: 'warning',
      });
    }
    return;
  }

  if (actual === null || actual === undefined) {
    differences.push({
      path: path || 'root',
      expected,
      actual,
      severity: 'critical',
    });
    return;
  }

  // Compare by type
  if (typeof expected === 'number' && typeof actual === 'number') {
    if (Math.abs(expected - actual) > tolerance.numeric) {
      differences.push({
        path: path || 'root',
        expected,
        actual,
        severity: Math.abs(expected - actual) > 1 ? 'critical' : 'warning',
      });
    }
    return;
  }

  if (typeof expected === 'string' && typeof actual === 'string') {
    let exp = expected;
    let act = actual;
    
    if (tolerance.string === 'trim') {
      exp = exp.trim();
      act = act.trim();
    } else if (tolerance.string === 'case-insensitive') {
      exp = exp.toLowerCase();
      act = act.toLowerCase();
    }
    
    if (exp !== act) {
      differences.push({
        path: path || 'root',
        expected,
        actual,
        severity: 'warning',
      });
    }
    return;
  }

  if (Array.isArray(expected) && Array.isArray(actual)) {
    if (expected.length !== actual.length) {
      differences.push({
        path: `${path}.length`,
        expected: expected.length,
        actual: actual.length,
        severity: 'critical',
      });
    }
    
    const minLen = Math.min(expected.length, actual.length);
    for (let i = 0; i < minLen; i++) {
      compareValues(expected[i], actual[i], `${path}[${i}]`, differences, tolerance);
    }
    return;
  }

  if (typeof expected === 'object' && typeof actual === 'object') {
    const expObj = expected as Record<string, unknown>;
    const actObj = actual as Record<string, unknown>;
    const allKeys = new Set([...Object.keys(expObj), ...Object.keys(actObj)]);
    
    for (const key of allKeys) {
      const newPath = path ? `${path}.${key}` : key;
      
      if (!(key in expObj)) {
        differences.push({
          path: newPath,
          expected: undefined,
          actual: actObj[key],
          severity: 'info',
        });
      } else if (!(key in actObj)) {
        differences.push({
          path: newPath,
          expected: expObj[key],
          actual: undefined,
          severity: 'critical',
        });
      } else {
        compareValues(expObj[key], actObj[key], newPath, differences, tolerance);
      }
    }
    return;
  }

  // Type mismatch
  if (typeof expected !== typeof actual) {
    differences.push({
      path: path || 'root',
      expected,
      actual,
      severity: 'critical',
    });
  } else if (expected !== actual) {
    differences.push({
      path: path || 'root',
      expected,
      actual,
      severity: 'warning',
    });
  }
}
