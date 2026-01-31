/**
 * Code Explainer API Routes
 * 
 * Endpoints for AI-powered code explanation and Q&A
 */

import { Hono } from 'hono';
import { z } from 'zod';
import { ExplainerAgent } from '@migrationpilot/agents';
import type { SourceLanguage } from '@migrationpilot/core';
import { generateId } from '@migrationpilot/core';
import { getUserId } from '../middleware/index.js';

export const explainerRoutes = new Hono();

// Store active sessions in memory (in production, use Redis)
const activeSessions = new Map<string, {
  agent: ExplainerAgent;
  createdAt: Date;
  lastAccess: Date;
}>();

// Clean up old sessions periodically (30 min timeout)
const SESSION_TIMEOUT_MS = 30 * 60 * 1000;

function cleanupSessions() {
  const now = Date.now();
  for (const [sessionId, session] of activeSessions) {
    if (now - session.lastAccess.getTime() > SESSION_TIMEOUT_MS) {
      activeSessions.delete(sessionId);
    }
  }
}
setInterval(cleanupSessions, 5 * 60 * 1000); // Run every 5 minutes

const ExplainCodeSchema = z.object({
  code: z.string().min(1),
  language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
  filename: z.string().default('input'),
  projectId: z.string().optional(),
});

const ExplainSectionSchema = z.object({
  code: z.string().min(1),
  language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
  filename: z.string().default('input'),
  startLine: z.number().int().min(1),
  endLine: z.number().int().min(1),
  projectId: z.string().optional(),
});

const StartSessionSchema = z.object({
  code: z.string().min(1),
  language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
  filename: z.string().default('input'),
});

const AskQuestionSchema = z.object({
  sessionId: z.string(),
  question: z.string().min(1),
});

const ExplainParagraphSchema = z.object({
  code: z.string().min(1),
  paragraphName: z.string().min(1),
  filename: z.string().default('input'),
  projectId: z.string().optional(),
});

const ExplainDataItemSchema = z.object({
  code: z.string().min(1),
  dataItemName: z.string().min(1),
  language: z.enum(['cobol', 'fortran', 'vb6', 'vba', 'java-legacy']),
  filename: z.string().default('input'),
  projectId: z.string().optional(),
});

/**
 * POST /explain
 * Get a full explanation of a code file
 */
explainerRoutes.post('/explain', async (c) => {
  const body = await c.req.json();
  const parsed = ExplainCodeSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const userId = getUserId(c);
    const agent = new ExplainerAgent();
    const context = {
      projectId: parsed.data.projectId || generateId(),
      sessionId: generateId(),
      userId: userId,
    };

    const result = await agent.explainCode(
      context,
      parsed.data.code,
      parsed.data.language as SourceLanguage,
      parsed.data.filename
    );

    if (!result.success) {
      return c.json({ error: 'Explanation failed', message: result.error }, 500);
    }

    return c.json({ data: result.data });
  } catch (error) {
    return c.json(
      {
        error: 'Explanation error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /explain/section
 * Explain a specific section of code by line numbers
 */
explainerRoutes.post('/explain/section', async (c) => {
  const body = await c.req.json();
  const parsed = ExplainSectionSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  if (parsed.data.endLine < parsed.data.startLine) {
    return c.json({ error: 'endLine must be >= startLine' }, 400);
  }

  try {
    const agent = new ExplainerAgent();
    const context = {
      projectId: parsed.data.projectId || generateId(),
      sessionId: generateId(),
      userId: 'user',
    };

    const result = await agent.explainSection(
      context,
      parsed.data.code,
      parsed.data.language as SourceLanguage,
      parsed.data.filename,
      parsed.data.startLine,
      parsed.data.endLine
    );

    if (!result.success) {
      return c.json({ error: 'Section explanation failed', message: result.error }, 500);
    }

    return c.json({ data: result.data });
  } catch (error) {
    return c.json(
      {
        error: 'Section explanation error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /explain/paragraph
 * Explain a specific COBOL paragraph by name
 */
explainerRoutes.post('/explain/paragraph', async (c) => {
  const body = await c.req.json();
  const parsed = ExplainParagraphSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const agent = new ExplainerAgent();
    const context = {
      projectId: parsed.data.projectId || generateId(),
      sessionId: generateId(),
      userId: 'user',
    };

    const result = await agent.explainParagraph(
      context,
      parsed.data.code,
      parsed.data.paragraphName,
      parsed.data.filename
    );

    if (!result.success) {
      return c.json({ error: 'Paragraph explanation failed', message: result.error }, 500);
    }

    return c.json({ data: result.data });
  } catch (error) {
    return c.json(
      {
        error: 'Paragraph explanation error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /explain/data-item
 * Explain a specific data item/variable
 */
explainerRoutes.post('/explain/data-item', async (c) => {
  const body = await c.req.json();
  const parsed = ExplainDataItemSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const agent = new ExplainerAgent();
    const context = {
      projectId: parsed.data.projectId || generateId(),
      sessionId: generateId(),
      userId: 'user',
    };

    const result = await agent.explainDataItem(
      context,
      parsed.data.code,
      parsed.data.dataItemName,
      parsed.data.language as SourceLanguage,
      parsed.data.filename
    );

    if (!result.success) {
      return c.json({ error: 'Data item explanation failed', message: result.error }, 500);
    }

    return c.json({ data: result.data });
  } catch (error) {
    return c.json(
      {
        error: 'Data item explanation error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /explain/executive-summary
 * Generate an executive summary for non-technical stakeholders
 */
explainerRoutes.post('/explain/executive-summary', async (c) => {
  const body = await c.req.json();
  const parsed = ExplainCodeSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const agent = new ExplainerAgent();
    const context = {
      projectId: parsed.data.projectId || generateId(),
      sessionId: generateId(),
      userId: 'user',
    };

    const result = await agent.generateExecutiveSummary(
      context,
      parsed.data.code,
      parsed.data.language as SourceLanguage,
      parsed.data.filename
    );

    if (!result.success) {
      return c.json({ error: 'Executive summary generation failed', message: result.error }, 500);
    }

    return c.json({ data: result.data });
  } catch (error) {
    return c.json(
      {
        error: 'Executive summary error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /qa/start
 * Start an interactive Q&A session
 */
explainerRoutes.post('/qa/start', async (c) => {
  const body = await c.req.json();
  const parsed = StartSessionSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  try {
    const sessionId = generateId();
    const agent = new ExplainerAgent();
    
    // Initialize the session
    agent.startSession(
      sessionId,
      parsed.data.code,
      parsed.data.language as SourceLanguage,
      parsed.data.filename
    );

    // Store the agent for future Q&A
    activeSessions.set(sessionId, {
      agent,
      createdAt: new Date(),
      lastAccess: new Date(),
    });

    return c.json({
      data: {
        sessionId,
        message: 'Q&A session started. Use /qa/ask to ask questions.',
        expiresIn: SESSION_TIMEOUT_MS,
      },
    });
  } catch (error) {
    return c.json(
      {
        error: 'Session start error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * POST /qa/ask
 * Ask a question in an active Q&A session
 */
explainerRoutes.post('/qa/ask', async (c) => {
  const body = await c.req.json();
  const parsed = AskQuestionSchema.safeParse(body);

  if (!parsed.success) {
    return c.json({ error: 'Validation error', details: parsed.error.issues }, 400);
  }

  const sessionData = activeSessions.get(parsed.data.sessionId);
  if (!sessionData) {
    return c.json({ 
      error: 'Session not found', 
      message: 'Session may have expired. Start a new session with /qa/start' 
    }, 404);
  }

  sessionData.lastAccess = new Date();

  try {
    const context = {
      projectId: generateId(),
      sessionId: parsed.data.sessionId,
      userId: 'user',
    };

    const result = await sessionData.agent.askQuestion(
      context,
      parsed.data.sessionId,
      parsed.data.question
    );

    if (!result.success) {
      return c.json({ error: 'Question failed', message: result.error }, 500);
    }

    return c.json({ data: result.data });
  } catch (error) {
    return c.json(
      {
        error: 'Q&A error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      500
    );
  }
});

/**
 * DELETE /qa/:sessionId
 * End a Q&A session
 */
explainerRoutes.delete('/qa/:sessionId', async (c) => {
  const sessionId = c.req.param('sessionId');
  
  if (activeSessions.has(sessionId)) {
    activeSessions.delete(sessionId);
    return c.json({ data: { message: 'Session ended successfully' } });
  }
  
  return c.json({ error: 'Session not found' }, 404);
});

/**
 * GET /qa/:sessionId
 * Get session info
 */
explainerRoutes.get('/qa/:sessionId', async (c) => {
  const sessionId = c.req.param('sessionId');
  const sessionData = activeSessions.get(sessionId);
  
  if (!sessionData) {
    return c.json({ error: 'Session not found' }, 404);
  }

  const session = sessionData.agent.getQASession(sessionId);
  
  return c.json({
    data: {
      sessionId,
      createdAt: sessionData.createdAt,
      lastAccess: sessionData.lastAccess,
      conversationLength: session?.conversationHistory?.length || 0,
    },
  });
});
