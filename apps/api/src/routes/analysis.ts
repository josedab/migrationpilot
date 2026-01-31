/**
 * Code analysis routes
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { createParser } from '@migrationpilot/parsers';
import { MigrationOrchestrator } from '@migrationpilot/agents';
import type { SourceLanguage } from '@migrationpilot/core';
import { generateId } from '@migrationpilot/core';
import { getUserId } from '../middleware/index.js';

export const analysisRoutes = new Hono();

const AnalyzeSchema = z.object({
  projectId: z.string(),
  filename: z.string(),
  content: z.string(),
  language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
});

// Analyze source code (parse only)
analysisRoutes.post('/parse', async (c) => {
  const body = await c.req.json();
  const parsed = AnalyzeSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const parser = createParser(parsed.data.language as SourceLanguage);
    const result = parser.parse(parsed.data.content, parsed.data.filename);

    return c.json({
      data: {
        success: result.success,
        dataStructures: result.dataStructures,
        procedures: result.procedures,
        errors: result.errors,
        warnings: result.warnings,
        metadata: result.metadata,
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Parse error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

// Full AI analysis
analysisRoutes.post('/analyze', async (c) => {
  const body = await c.req.json();
  const parsed = AnalyzeSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const userId = getUserId(c);
    const orchestrator = new MigrationOrchestrator();
    const result = await orchestrator.analyzeOnly(
      parsed.data.content,
      parsed.data.filename,
      parsed.data.language as SourceLanguage,
      parsed.data.projectId,
      userId
    );

    if (!result) {
      return c.json({ error: 'Analysis failed' }, 500);
    }

    return c.json({ data: result });
  } catch (error) {
    return c.json(
      {
        error: 'Analysis error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

// Extract business rules
analysisRoutes.post('/extract-rules', async (c) => {
  const body = await c.req.json();
  
  if (!body.content || !body.language) {
    return c.json({ error: 'content and language required' }, 400);
  }

  try {
    // Parse first
    const parser = createParser(body.language as SourceLanguage);
    const parseResult = parser.parse(body.content, body.filename || 'input');

    if (!parseResult.success) {
      return c.json({ error: 'Parse failed', errors: parseResult.errors }, 400);
    }

    // Use orchestrator for AI-powered rule extraction
    const orchestrator = new MigrationOrchestrator();
    const analysis = await orchestrator.analyzeOnly(
      body.content,
      body.filename || 'input',
      body.language as SourceLanguage,
      body.projectId || generateId(),
      'user'
    );

    return c.json({
      data: {
        rules: analysis?.businessRules || [],
        procedures: parseResult.procedures,
        complexity: analysis?.complexity,
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Extraction error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});
