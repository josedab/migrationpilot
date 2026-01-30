/**
 * Transaction Replay Engine - Replays mainframe transactions in modern systems
 */

import type {
  ExecutionTrace,
  ComparisonResult,
  FieldDifference,
  ToleranceConfig,
} from '../types.js';

// ============================================================================
// Replay Types
// ============================================================================

export interface TransactionReplayConfig {
  projectId: string;
  targetEndpoint: string;
  targetType: 'rest' | 'grpc' | 'graphql' | 'custom';
  
  // Execution settings
  maxConcurrency: number;
  timeoutMs: number;
  retryCount: number;
  retryDelayMs: number;
  
  // Data transformation
  transformers?: DataTransformer[];
  inputMapping?: Record<string, string>;
  outputMapping?: Record<string, string>;
  
  // Comparison settings
  toleranceConfig: ToleranceConfig;
  strictMode: boolean;
  
  // Filtering
  includeTransactionTypes?: string[];
  excludeTransactionTypes?: string[];
  dateRange?: { start: Date; end: Date };
  
  // Callbacks
  onReplayStart?: (trace: ExecutionTrace) => void;
  onReplayComplete?: (result: TransactionReplayResult) => void;
  onError?: (error: Error, trace: ExecutionTrace) => void;
}

export interface DataTransformer {
  name: string;
  inputPath?: string;
  outputPath?: string;
  transform: (value: unknown, context: TransformContext) => unknown;
}

export interface TransformContext {
  trace: ExecutionTrace;
  field: string;
  direction: 'input' | 'output';
}

export interface TransactionReplayResult {
  id: string;
  traceId: string;
  projectId: string;
  transactionName: string;
  
  // Execution status
  status: 'success' | 'failure' | 'timeout' | 'error' | 'mismatch';
  
  // Timing
  startedAt: Date;
  completedAt: Date;
  durationMs: number;
  originalDurationMs: number;
  
  // Request/Response
  transformedInput: unknown;
  expectedOutput: unknown;
  actualOutput: unknown;
  
  // Comparison
  comparison: ComparisonResult;
  
  // Errors
  error?: string;
  errorStack?: string;
  
  // Metadata
  retryCount: number;
}

export interface TransactionReplaySession {
  id: string;
  projectId: string;
  config: TransactionReplayConfig;
  
  // Status
  status: 'pending' | 'running' | 'paused' | 'completed' | 'failed' | 'cancelled';
  
  // Progress
  totalTraces: number;
  processedTraces: number;
  successfulReplays: number;
  failedReplays: number;
  mismatchedReplays: number;
  
  // Timing
  startedAt?: Date;
  completedAt?: Date;
  
  // Results
  results: TransactionReplayResult[];
  summary?: TransactionReplaySummary;
}

export interface TransactionReplaySummary {
  sessionId: string;
  projectId: string;
  
  // Counts
  totalTransactions: number;
  successfulTransactions: number;
  failedTransactions: number;
  mismatchedTransactions: number;
  timedOutTransactions: number;
  
  // Percentages
  successRate: number;
  mismatchRate: number;
  errorRate: number;
  
  // Timing
  totalDurationMs: number;
  averageDurationMs: number;
  minDurationMs: number;
  maxDurationMs: number;
  percentile95Ms: number;
  
  // Comparison with original
  averageSpeedup: number;
  
  // Common issues
  topErrors: ErrorSummary[];
  topMismatches: MismatchSummary[];
  
  // Recommendations
  recommendations: string[];
}

export interface ErrorSummary {
  errorType: string;
  message: string;
  count: number;
  percentage: number;
  exampleTraceIds: string[];
}

export interface MismatchSummary {
  fieldPath: string;
  mismatchType: string;
  count: number;
  percentage: number;
  examples: { expected: unknown; actual: unknown }[];
}

// ============================================================================
// Transaction Replay Engine
// ============================================================================

export class TransactionReplayEngine {
  private sessions: Map<string, TransactionReplaySession> = new Map();
  private activeReplays: Set<string> = new Set();

  /**
   * Create a new replay session
   */
  createSession(config: TransactionReplayConfig, traces: ExecutionTrace[]): TransactionReplaySession {
    const sessionId = `replay_${Date.now()}_${Math.random().toString(36).slice(2)}`;

    const session: TransactionReplaySession = {
      id: sessionId,
      projectId: config.projectId,
      config,
      status: 'pending',
      totalTraces: traces.length,
      processedTraces: 0,
      successfulReplays: 0,
      failedReplays: 0,
      mismatchedReplays: 0,
      results: [],
    };

    // Store traces for replay
    (session as TransactionReplaySessionInternal).traces = traces;

    this.sessions.set(sessionId, session);
    return session;
  }

  /**
   * Start replay session
   */
  async startSession(sessionId: string): Promise<TransactionReplaySession> {
    const session = this.sessions.get(sessionId) as TransactionReplaySessionInternal | undefined;
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    if (session.status !== 'pending' && session.status !== 'paused') {
      throw new Error(`Cannot start session with status: ${session.status}`);
    }

    session.status = 'running';
    session.startedAt = new Date();
    this.activeReplays.add(sessionId);

    try {
      await this.processTraces(session);
      session.status = 'completed';
      session.summary = this.generateSummary(session);
    } catch (error) {
      session.status = 'failed';
      throw error;
    } finally {
      session.completedAt = new Date();
      this.activeReplays.delete(sessionId);
    }

    return session;
  }

  /**
   * Pause replay session
   */
  pauseSession(sessionId: string): void {
    const session = this.sessions.get(sessionId);
    if (session && session.status === 'running') {
      session.status = 'paused';
    }
  }

  /**
   * Cancel replay session
   */
  cancelSession(sessionId: string): void {
    const session = this.sessions.get(sessionId);
    if (session && (session.status === 'running' || session.status === 'paused')) {
      session.status = 'cancelled';
      this.activeReplays.delete(sessionId);
    }
  }

  /**
   * Get session by ID
   */
  getSession(sessionId: string): TransactionReplaySession | null {
    return this.sessions.get(sessionId) || null;
  }

  /**
   * Get all sessions for a project
   */
  getSessionsByProject(projectId: string): TransactionReplaySession[] {
    return Array.from(this.sessions.values())
      .filter(s => s.projectId === projectId);
  }

  /**
   * Replay a single trace
   */
  async replayTrace(
    trace: ExecutionTrace,
    config: TransactionReplayConfig
  ): Promise<TransactionReplayResult> {
    const resultId = `result_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    const startTime = Date.now();

    try {
      // Transform input
      const transformedInput = this.transformInput(trace, config);

      // Execute against target
      const actualOutput = await this.executeReplay(transformedInput, config);

      // Get expected output from trace
      const expectedOutput = this.extractExpectedOutput(trace);

      // Compare outputs
      const comparison = this.compareOutputs(
        expectedOutput,
        actualOutput,
        config.toleranceConfig
      );

      const status = comparison.equivalent ? 'success' : 'mismatch';

      return {
        id: resultId,
        traceId: trace.id,
        projectId: config.projectId,
        transactionName: trace.programName,
        status,
        startedAt: new Date(startTime),
        completedAt: new Date(),
        durationMs: Date.now() - startTime,
        originalDurationMs: trace.durationMs,
        transformedInput,
        expectedOutput,
        actualOutput,
        comparison,
        retryCount: 0,
      };
    } catch (error) {
      return {
        id: resultId,
        traceId: trace.id,
        projectId: config.projectId,
        transactionName: trace.programName,
        status: 'error',
        startedAt: new Date(startTime),
        completedAt: new Date(),
        durationMs: Date.now() - startTime,
        originalDurationMs: trace.durationMs,
        transformedInput: null,
        expectedOutput: null,
        actualOutput: null,
        comparison: {
          equivalent: false,
          differences: [],
          legacyOnly: [],
          modernOnly: [],
          matchedFields: 0,
          totalFields: 0,
          confidence: 0,
        },
        error: error instanceof Error ? error.message : 'Unknown error',
        errorStack: error instanceof Error ? error.stack : undefined,
        retryCount: 0,
      };
    }
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private async processTraces(session: TransactionReplaySessionInternal): Promise<void> {
    const { config, traces } = session;
    const concurrency = config.maxConcurrency || 1;

    // Filter traces based on config
    let filteredTraces = traces;
    if (config.includeTransactionTypes?.length) {
      filteredTraces = filteredTraces.filter(t =>
        config.includeTransactionTypes!.includes(t.programName)
      );
    }
    if (config.excludeTransactionTypes?.length) {
      filteredTraces = filteredTraces.filter(t =>
        !config.excludeTransactionTypes!.includes(t.programName)
      );
    }
    if (config.dateRange) {
      filteredTraces = filteredTraces.filter(t =>
        t.capturedAt >= config.dateRange!.start &&
        t.capturedAt <= config.dateRange!.end
      );
    }

    session.totalTraces = filteredTraces.length;

    // Process in batches with concurrency limit
    for (let i = 0; i < filteredTraces.length; i += concurrency) {
      if (session.status === 'paused' || session.status === 'cancelled') {
        break;
      }

      const batch = filteredTraces.slice(i, i + concurrency);
      const results = await Promise.all(
        batch.map(trace => this.replayWithRetry(trace, config))
      );

      for (const result of results) {
        session.results.push(result);
        session.processedTraces++;

        if (result.status === 'success') {
          session.successfulReplays++;
        } else if (result.status === 'mismatch') {
          session.mismatchedReplays++;
        } else {
          session.failedReplays++;
        }
      }
    }
  }

  private async replayWithRetry(
    trace: ExecutionTrace,
    config: TransactionReplayConfig
  ): Promise<TransactionReplayResult> {
    let lastResult: TransactionReplayResult | null = null;
    const maxRetries = config.retryCount || 0;

    for (let attempt = 0; attempt <= maxRetries; attempt++) {
      lastResult = await this.replayTrace(trace, config);

      if (lastResult.status === 'success' || lastResult.status === 'mismatch') {
        return lastResult;
      }

      if (attempt < maxRetries) {
        await this.delay(config.retryDelayMs || 1000);
        lastResult.retryCount = attempt + 1;
      }
    }

    return lastResult!;
  }

  private transformInput(
    trace: ExecutionTrace,
    config: TransactionReplayConfig
  ): unknown {
    let input: Record<string, unknown> = { ...trace.inputs.parameters };

    // Apply field mapping
    if (config.inputMapping) {
      const mapped: Record<string, unknown> = {};
      for (const [from, to] of Object.entries(config.inputMapping)) {
        if (from in input) {
          mapped[to] = input[from];
        }
      }
      input = mapped;
    }

    // Apply transformers
    if (config.transformers) {
      for (const transformer of config.transformers) {
        if (transformer.inputPath) {
          const value = this.getNestedValue(input, transformer.inputPath);
          const transformed = transformer.transform(value, {
            trace,
            field: transformer.inputPath,
            direction: 'input',
          });
          this.setNestedValue(input, transformer.inputPath, transformed);
        }
      }
    }

    return input;
  }

  private extractExpectedOutput(trace: ExecutionTrace): unknown {
    return {
      ...trace.outputs.parameters,
      _files: trace.outputs.files,
      _database: trace.outputs.database,
    };
  }

  private async executeReplay(
    _input: unknown,
    _config: TransactionReplayConfig
  ): Promise<unknown> {
    // In a real implementation, this would make HTTP/gRPC calls
    // For now, simulate with mock response
    await this.delay(Math.random() * 100);

    // Simulate occasional errors
    if (Math.random() < 0.05) {
      throw new Error('Connection timeout');
    }

    return { status: 'ok', data: {} };
  }

  private compareOutputs(
    expected: unknown,
    actual: unknown,
    tolerance: ToleranceConfig
  ): ComparisonResult {
    const differences: FieldDifference[] = [];
    const legacyOnly: string[] = [];
    const modernOnly: string[] = [];
    let matchedFields = 0;
    let totalFields = 0;

    const compareObjects = (
      exp: Record<string, unknown>,
      act: Record<string, unknown>,
      path: string = ''
    ) => {
      const allKeys = new Set([...Object.keys(exp), ...Object.keys(act)]);

      for (const key of allKeys) {
        const fullPath = path ? `${path}.${key}` : key;
        totalFields++;

        if (tolerance.ignoreFields?.includes(fullPath)) {
          matchedFields++;
          continue;
        }

        if (!(key in exp)) {
          modernOnly.push(fullPath);
          continue;
        }

        if (!(key in act)) {
          legacyOnly.push(fullPath);
          continue;
        }

        const expValue = exp[key];
        const actValue = act[key];

        if (this.valuesEqual(expValue, actValue, tolerance)) {
          matchedFields++;
        } else {
          differences.push({
            path: fullPath,
            legacyValue: expValue,
            modernValue: actValue,
            differenceType: 'value',
            severity: 'warning',
          });
        }
      }
    };

    if (expected && actual && typeof expected === 'object' && typeof actual === 'object') {
      compareObjects(
        expected as Record<string, unknown>,
        actual as Record<string, unknown>
      );
    }

    const equivalent = differences.length === 0 && legacyOnly.length === 0 && modernOnly.length === 0;
    const confidence = totalFields > 0 ? matchedFields / totalFields : 1;

    return {
      equivalent,
      differences,
      legacyOnly,
      modernOnly,
      matchedFields,
      totalFields,
      confidence,
    };
  }

  private valuesEqual(
    a: unknown,
    b: unknown,
    tolerance: ToleranceConfig
  ): boolean {
    if (a === b) return true;

    // Numeric tolerance
    if (typeof a === 'number' && typeof b === 'number') {
      return Math.abs(a - b) <= tolerance.numericTolerance;
    }

    // String normalization
    if (typeof a === 'string' && typeof b === 'string') {
      let normA = a;
      let normB = b;

      switch (tolerance.stringNormalization) {
        case 'trim':
          normA = a.trim();
          normB = b.trim();
          break;
        case 'lowercase':
          normA = a.toLowerCase();
          normB = b.toLowerCase();
          break;
        case 'alphanumeric':
          normA = a.replace(/[^a-zA-Z0-9]/g, '');
          normB = b.replace(/[^a-zA-Z0-9]/g, '');
          break;
      }

      return normA === normB;
    }

    // Date tolerance
    if (a instanceof Date && b instanceof Date) {
      return Math.abs(a.getTime() - b.getTime()) <= tolerance.dateToleranceMs;
    }

    return JSON.stringify(a) === JSON.stringify(b);
  }

  private generateSummary(session: TransactionReplaySession): TransactionReplaySummary {
    const results = session.results;
    const durations = results.map(r => r.durationMs).sort((a, b) => a - b);

    // Calculate timing stats
    const totalDuration = durations.reduce((sum, d) => sum + d, 0);
    const avgDuration = results.length > 0 ? totalDuration / results.length : 0;
    const minDuration = durations[0] || 0;
    const maxDuration = durations[durations.length - 1] || 0;
    const p95Index = Math.floor(durations.length * 0.95);
    const percentile95 = durations[p95Index] || 0;

    // Calculate speedup
    const speedups = results
      .filter(r => r.originalDurationMs > 0 && r.durationMs > 0)
      .map(r => r.originalDurationMs / r.durationMs);
    const avgSpeedup = speedups.length > 0
      ? speedups.reduce((sum, s) => sum + s, 0) / speedups.length
      : 1;

    // Group errors
    const errorCounts = new Map<string, { count: number; message: string; traceIds: string[] }>();
    for (const result of results) {
      if (result.error) {
        const key = result.error.split('\n')[0] || 'Unknown error';
        const existing = errorCounts.get(key) || { count: 0, message: key, traceIds: [] as string[] };
        existing.count++;
        existing.traceIds.push(result.traceId);
        errorCounts.set(key, existing);
      }
    }

    const topErrors: ErrorSummary[] = Array.from(errorCounts.values())
      .sort((a, b) => b.count - a.count)
      .slice(0, 5)
      .map(e => ({
        errorType: 'Error',
        message: e.message,
        count: e.count,
        percentage: (e.count / results.length) * 100,
        exampleTraceIds: e.traceIds.slice(0, 3),
      }));

    // Group mismatches
    const mismatchCounts = new Map<string, { count: number; examples: { expected: unknown; actual: unknown }[] }>();
    for (const result of results) {
      for (const diff of result.comparison.differences) {
        const existing = mismatchCounts.get(diff.path) || { count: 0, examples: [] };
        existing.count++;
        if (existing.examples.length < 3) {
          existing.examples.push({ expected: diff.legacyValue, actual: diff.modernValue });
        }
        mismatchCounts.set(diff.path, existing);
      }
    }

    const topMismatches: MismatchSummary[] = Array.from(mismatchCounts.entries())
      .sort((a, b) => b[1].count - a[1].count)
      .slice(0, 5)
      .map(([path, data]) => ({
        fieldPath: path,
        mismatchType: 'value',
        count: data.count,
        percentage: (data.count / results.length) * 100,
        examples: data.examples,
      }));

    // Generate recommendations
    const recommendations: string[] = [];
    if (session.failedReplays > session.totalTraces * 0.1) {
      recommendations.push('High error rate detected. Review connection settings and retry configuration.');
    }
    if (session.mismatchedReplays > session.totalTraces * 0.05) {
      recommendations.push('Significant output mismatches found. Review data transformation rules.');
    }
    if (avgSpeedup < 1) {
      recommendations.push('Modern system is slower than legacy. Consider performance optimization.');
    }

    return {
      sessionId: session.id,
      projectId: session.projectId,
      totalTransactions: session.totalTraces,
      successfulTransactions: session.successfulReplays,
      failedTransactions: session.failedReplays,
      mismatchedTransactions: session.mismatchedReplays,
      timedOutTransactions: results.filter(r => r.status === 'timeout').length,
      successRate: session.totalTraces > 0
        ? (session.successfulReplays / session.totalTraces) * 100
        : 0,
      mismatchRate: session.totalTraces > 0
        ? (session.mismatchedReplays / session.totalTraces) * 100
        : 0,
      errorRate: session.totalTraces > 0
        ? (session.failedReplays / session.totalTraces) * 100
        : 0,
      totalDurationMs: totalDuration,
      averageDurationMs: avgDuration,
      minDurationMs: minDuration,
      maxDurationMs: maxDuration,
      percentile95Ms: percentile95,
      averageSpeedup: avgSpeedup,
      topErrors,
      topMismatches,
      recommendations,
    };
  }

  private getNestedValue(obj: Record<string, unknown>, path: string): unknown {
    return path.split('.').reduce((o: unknown, k) => {
      if (o && typeof o === 'object' && k in (o as Record<string, unknown>)) {
        return (o as Record<string, unknown>)[k];
      }
      return undefined;
    }, obj);
  }

  private setNestedValue(obj: Record<string, unknown>, path: string, value: unknown): void {
    const keys = path.split('.');
    const lastKey = keys.pop();
    if (!lastKey) return;
    
    const target = keys.reduce((o: unknown, k) => {
      if (o && typeof o === 'object') {
        const record = o as Record<string, unknown>;
        if (!(k in record)) {
          record[k] = {};
        }
        return record[k];
      }
      return {};
    }, obj) as Record<string, unknown>;
    target[lastKey] = value;
  }

  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// Internal type with traces array
interface TransactionReplaySessionInternal extends TransactionReplaySession {
  traces: ExecutionTrace[];
}

// ============================================================================
// Factory
// ============================================================================

export function createTransactionReplayEngine(): TransactionReplayEngine {
  return new TransactionReplayEngine();
}
