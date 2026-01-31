/**
 * Migration execution routes
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { MigrationOrchestrator, type MigrationConfig } from '@migrationpilot/agents';
import type { SourceLanguage, TargetLanguage } from '@migrationpilot/core';
import { generateId, DEFAULT_PROJECT_SETTINGS } from '@migrationpilot/core';
import { streamSSE } from 'hono/streaming';
import { getUserId } from '../middleware/index.js';

export const migrationRoutes = new Hono();

const MigrateSchema = z.object({
  projectId: z.string(),
  filename: z.string(),
  content: z.string(),
  sourceLanguage: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
  targetLanguage: z.enum(['java', 'python', 'typescript', 'go', 'csharp']),
  targetFramework: z.string(),
  options: z.object({
    enableStranglerFig: z.boolean().optional(),
    generateTests: z.boolean().optional(),
    generateDocumentation: z.boolean().optional(),
    humanReviewRequired: z.boolean().optional(),
    confidenceThreshold: z.number().min(0).max(1).optional(),
  }).optional(),
});

// In-memory job storage
const jobs = new Map<string, {
  id: string;
  status: 'pending' | 'running' | 'completed' | 'failed';
  progress: number;
  phase: string;
  result?: unknown;
  error?: string;
  startedAt: Date;
  completedAt?: Date;
}>();

// Start migration job
migrationRoutes.post('/start', async (c) => {
  const body = await c.req.json();
  const parsed = MigrateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const jobId = generateId();
  const job = {
    id: jobId,
    status: 'pending' as const,
    progress: 0,
    phase: 'initializing',
    startedAt: new Date(),
  };
  jobs.set(jobId, job);

  // Start async migration
  runMigration(jobId, parsed.data);

  return c.json({
    data: {
      jobId,
      status: 'pending',
      message: 'Migration job started',
    },
  }, 202);
});

// Get job status
migrationRoutes.get('/jobs/:id', (c) => {
  const job = jobs.get(c.req.param('id'));
  
  if (!job) {
    return c.json({ error: 'Job not found' }, 404);
  }

  return c.json({ data: job });
});

// Stream job progress (SSE)
migrationRoutes.get('/jobs/:id/stream', (c) => {
  const jobId = c.req.param('id');
  const job = jobs.get(jobId);
  
  if (!job) {
    return c.json({ error: 'Job not found' }, 404);
  }

  return streamSSE(c, async (stream) => {
    let lastProgress = -1;

    while (true) {
      const currentJob = jobs.get(jobId);
      if (!currentJob) break;

      if (currentJob.progress !== lastProgress || currentJob.status !== 'running') {
        await stream.writeSSE({
          data: JSON.stringify({
            status: currentJob.status,
            progress: currentJob.progress,
            phase: currentJob.phase,
          }),
          event: 'progress',
        });
        lastProgress = currentJob.progress;
      }

      if (currentJob.status === 'completed' || currentJob.status === 'failed') {
        await stream.writeSSE({
          data: JSON.stringify({
            status: currentJob.status,
            result: currentJob.result,
            error: currentJob.error,
          }),
          event: 'complete',
        });
        break;
      }

      await new Promise(resolve => setTimeout(resolve, 500));
    }
  });
});

// Execute synchronous migration (for small files)
migrationRoutes.post('/execute', async (c) => {
  const body = await c.req.json();
  const parsed = MigrateSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const userId = getUserId(c);
    const orchestrator = new MigrationOrchestrator();
    
    const config: MigrationConfig = {
      projectId: parsed.data.projectId,
      userId: userId,
      sourceLanguage: parsed.data.sourceLanguage as SourceLanguage,
      targetLanguage: parsed.data.targetLanguage as TargetLanguage,
      targetFramework: parsed.data.targetFramework,
      options: {
        ...DEFAULT_PROJECT_SETTINGS,
        ...parsed.data.options,
      },
    };

    const result = await orchestrator.migrate(
      parsed.data.content,
      parsed.data.filename,
      config
    );

    return c.json({ data: result });
  } catch (error) {
    return c.json(
      {
        error: 'Migration error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

// Helper to run migration asynchronously
async function runMigration(jobId: string, data: z.infer<typeof MigrateSchema>) {
  const job = jobs.get(jobId)!;
  job.status = 'running';

  try {
    const orchestrator = new MigrationOrchestrator();
    
    const config: MigrationConfig = {
      projectId: data.projectId,
      userId: 'user',
      sourceLanguage: data.sourceLanguage as SourceLanguage,
      targetLanguage: data.targetLanguage as TargetLanguage,
      targetFramework: data.targetFramework,
      options: {
        ...DEFAULT_PROJECT_SETTINGS,
        ...data.options,
      },
      callbacks: {
        onProgress: (phase, progress, _message) => {
          const currentJob = jobs.get(jobId);
          if (currentJob) {
            currentJob.phase = phase;
            currentJob.progress = progress;
          }
        },
      },
    };

    const result = await orchestrator.migrate(
      data.content,
      data.filename,
      config
    );

    job.status = 'completed';
    job.result = result;
    job.completedAt = new Date();
  } catch (error) {
    job.status = 'failed';
    job.error = error instanceof Error ? error.message : 'Unknown error';
    job.completedAt = new Date();
  }
}
