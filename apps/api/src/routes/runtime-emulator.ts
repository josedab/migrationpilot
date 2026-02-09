/**
 * Runtime Emulator API Routes
 * Endpoints for legacy code execution and validation
 */

import { Hono } from 'hono';
import {
  LegacyRuntimeEmulator,
  type LegacyProgram,
  type VariableValue,
  type EmulatorConfig,
} from '@migrationpilot/runtime-emulator';

const app = new Hono();

// Store emulator instances per session
const emulators = new Map<string, LegacyRuntimeEmulator>();

function getOrCreateEmulator(sessionId: string, config?: Partial<EmulatorConfig>): LegacyRuntimeEmulator {
  let emulator = emulators.get(sessionId);
  if (!emulator) {
    emulator = new LegacyRuntimeEmulator(config);
    emulators.set(sessionId, emulator);
  }
  return emulator;
}

// Load a program into the emulator
app.post('/sessions/:sessionId/load', async (c) => {
  const sessionId = c.req.param('sessionId');
  const body = await c.req.json<{
    program: LegacyProgram;
    config?: Partial<EmulatorConfig>;
  }>();

  const emulator = getOrCreateEmulator(sessionId, body.config);
  await emulator.loadProgram(body.program);

  return c.json({
    sessionId,
    programId: body.program.id,
    programName: body.program.name,
    language: body.program.language,
    status: 'loaded',
  });
});

// Execute a loaded program
app.post('/sessions/:sessionId/execute', async (c) => {
  const sessionId = c.req.param('sessionId');
  const body = await c.req.json<{
    inputs?: Record<string, VariableValue>;
    fileData?: Record<string, string[]>;
    stdinData?: string[];
  }>();

  const emulator = emulators.get(sessionId);
  if (!emulator) {
    return c.json({ error: 'Session not found. Load a program first.' }, 404);
  }

  // Provide file data if specified
  if (body.fileData) {
    for (const [fileName, records] of Object.entries(body.fileData)) {
      emulator.loadFileData(fileName, records);
    }
  }

  // Provide stdin data if specified
  if (body.stdinData) {
    for (const input of body.stdinData) {
      emulator.provideInput(input);
    }
  }

  // Convert inputs to Map
  const inputs = new Map<string, VariableValue>(
    Object.entries(body.inputs || {})
  );

  const result = await emulator.execute(inputs);

  return c.json({
    sessionId,
    result: {
      success: result.success,
      returnCode: result.returnCode,
      output: result.output,
      errors: result.errors,
      performance: result.performance,
    },
  });
});

// Execute with full trace enabled
app.post('/sessions/:sessionId/execute-with-trace', async (c) => {
  const sessionId = c.req.param('sessionId');
  const body = await c.req.json<{
    inputs?: Record<string, VariableValue>;
    fileData?: Record<string, string[]>;
    stdinData?: string[];
  }>();

  // Create new emulator with tracing enabled
  const emulator = new LegacyRuntimeEmulator({
    enableTrace: true,
    traceLevel: 'verbose',
  });
  emulators.set(sessionId, emulator);

  // Load the program if session had one
  const existingEmulator = emulators.get(sessionId);
  if (!existingEmulator) {
    return c.json({ error: 'Session not found. Load a program first.' }, 404);
  }

  // Provide file data if specified
  if (body.fileData) {
    for (const [fileName, records] of Object.entries(body.fileData)) {
      emulator.loadFileData(fileName, records);
    }
  }

  // Provide stdin data if specified
  if (body.stdinData) {
    for (const input of body.stdinData) {
      emulator.provideInput(input);
    }
  }

  const inputs = new Map<string, VariableValue>(
    Object.entries(body.inputs || {})
  );

  const result = await emulator.execute(inputs);

  return c.json({
    sessionId,
    result: {
      success: result.success,
      returnCode: result.returnCode,
      output: result.output,
      trace: result.trace,
      errors: result.errors,
      performance: result.performance,
    },
  });
});

// Get current memory state
app.get('/sessions/:sessionId/memory', (c) => {
  const sessionId = c.req.param('sessionId');
  const emulator = emulators.get(sessionId);

  if (!emulator) {
    return c.json({ error: 'Session not found' }, 404);
  }

  return c.json({
    sessionId,
    memory: emulator.getMemory(),
  });
});

// Set a breakpoint
app.post('/sessions/:sessionId/breakpoints', async (c) => {
  const sessionId = c.req.param('sessionId');
  const body = await c.req.json<{
    location: string | number;
  }>();

  const emulator = emulators.get(sessionId);
  if (!emulator) {
    return c.json({ error: 'Session not found' }, 404);
  }

  emulator.setBreakpoint(body.location);

  return c.json({
    sessionId,
    breakpoint: body.location,
    status: 'set',
  });
});

// Clear a breakpoint
app.delete('/sessions/:sessionId/breakpoints/:location', (c) => {
  const sessionId = c.req.param('sessionId');
  const location = c.req.param('location');

  const emulator = emulators.get(sessionId);
  if (!emulator) {
    return c.json({ error: 'Session not found' }, 404);
  }

  const loc = isNaN(Number(location)) ? location : Number(location);
  emulator.clearBreakpoint(loc);

  return c.json({
    sessionId,
    breakpoint: loc,
    status: 'cleared',
  });
});

// Reset the emulator
app.post('/sessions/:sessionId/reset', (c) => {
  const sessionId = c.req.param('sessionId');
  const emulator = emulators.get(sessionId);

  if (!emulator) {
    return c.json({ error: 'Session not found' }, 404);
  }

  emulator.reset();

  return c.json({
    sessionId,
    status: 'reset',
  });
});

// Validate legacy vs modern output
app.post('/sessions/:sessionId/validate', async (c) => {
  const sessionId = c.req.param('sessionId');
  const body = await c.req.json<{
    legacyInputs: Record<string, VariableValue>;
    modernResult: unknown;
    fileData?: Record<string, string[]>;
  }>();

  const emulator = emulators.get(sessionId);
  if (!emulator) {
    return c.json({ error: 'Session not found' }, 404);
  }

  // Provide file data if specified
  if (body.fileData) {
    for (const [fileName, records] of Object.entries(body.fileData)) {
      emulator.loadFileData(fileName, records);
    }
  }

  // Execute legacy code
  const inputs = new Map<string, VariableValue>(
    Object.entries(body.legacyInputs)
  );
  const legacyResult = await emulator.execute(inputs);

  // Validate against modern result
  const validation = emulator.validate(legacyResult, body.modernResult);

  return c.json({
    sessionId,
    validation: {
      isEquivalent: validation.isEquivalent,
      differences: validation.differences,
      coverage: validation.coverage,
      confidence: validation.confidence,
    },
    legacyResult: {
      success: legacyResult.success,
      returnCode: legacyResult.returnCode,
      output: legacyResult.output,
    },
  });
});

// Batch validation with multiple test cases
app.post('/sessions/:sessionId/validate-batch', async (c) => {
  const sessionId = c.req.param('sessionId');
  const body = await c.req.json<{
    testCases: Array<{
      name: string;
      inputs: Record<string, VariableValue>;
      expectedOutput: unknown;
      fileData?: Record<string, string[]>;
    }>;
  }>();

  const emulator = emulators.get(sessionId);
  if (!emulator) {
    return c.json({ error: 'Session not found' }, 404);
  }

  const results = [];
  let passCount = 0;
  let failCount = 0;

  for (const testCase of body.testCases) {
    emulator.reset();

    if (testCase.fileData) {
      for (const [fileName, records] of Object.entries(testCase.fileData)) {
        emulator.loadFileData(fileName, records);
      }
    }

    const inputs = new Map<string, VariableValue>(
      Object.entries(testCase.inputs)
    );
    const legacyResult = await emulator.execute(inputs);
    const validation = emulator.validate(legacyResult, testCase.expectedOutput);

    if (validation.isEquivalent) {
      passCount++;
    } else {
      failCount++;
    }

    results.push({
      name: testCase.name,
      passed: validation.isEquivalent,
      differences: validation.differences,
      confidence: validation.confidence,
      legacyReturnCode: legacyResult.returnCode,
    });
  }

  return c.json({
    sessionId,
    summary: {
      total: body.testCases.length,
      passed: passCount,
      failed: failCount,
      passRate: body.testCases.length > 0 ? passCount / body.testCases.length : 0,
    },
    results,
  });
});

// Delete a session
app.delete('/sessions/:sessionId', (c) => {
  const sessionId = c.req.param('sessionId');
  const existed = emulators.delete(sessionId);

  return c.json({
    sessionId,
    status: existed ? 'deleted' : 'not_found',
  });
});

// List active sessions
app.get('/sessions', (c) => {
  const sessions = Array.from(emulators.keys());

  return c.json({
    sessions,
    count: sessions.length,
  });
});

// Health check
app.get('/health', (c) => {
  return c.json({
    status: 'healthy',
    activeSessions: emulators.size,
  });
});

export default app;
