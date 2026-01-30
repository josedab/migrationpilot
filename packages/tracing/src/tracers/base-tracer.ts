/**
 * Base Tracer Implementation
 * 
 * Abstract base class for language-specific tracers
 */

import { generateId } from '@migrationpilot/core';
import type { SourceLanguage } from '@migrationpilot/core';
import type {
  ITracer,
  TracerConfig,
  ExecutionTrace,
  TraceEvent,
  TraceEventData,
  TraceFilter,
  CapturedIO,
  CallStackEntry,
  TraceMetrics,
} from '../types.js';
import { DEFAULT_TRACER_CONFIG } from '../types.js';

export abstract class BaseTracer implements ITracer {
  abstract readonly language: SourceLanguage;
  readonly config: TracerConfig;
  
  protected traces: Map<string, ExecutionTrace> = new Map();
  protected activeTraces: Map<string, ActiveTraceState> = new Map();
  protected initialized = false;

  constructor(config: Partial<TracerConfig> & { language: SourceLanguage; projectId: string }) {
    this.config = {
      ...DEFAULT_TRACER_CONFIG,
      ...config,
    } as TracerConfig;
  }

  async initialize(): Promise<void> {
    if (this.initialized) return;
    await this.onInitialize();
    this.initialized = true;
  }

  async shutdown(): Promise<void> {
    // End any active traces
    for (const sessionId of this.activeTraces.keys()) {
      this.endTrace(sessionId, {});
    }
    await this.onShutdown();
    this.initialized = false;
  }

  startTrace(programName: string, inputs: Record<string, unknown>): string {
    const sessionId = generateId();
    const startTime = Date.now();
    
    const state: ActiveTraceState = {
      sessionId,
      projectId: this.config.projectId,
      programName,
      language: this.language,
      startTime,
      events: [],
      callStack: [],
      currentDepth: 0,
      sequenceCounter: 0,
      inputs: this.captureInputs(inputs),
      metrics: this.createEmptyMetrics(),
    };
    
    this.activeTraces.set(sessionId, state);
    
    // Record program start event
    this.recordEvent(sessionId, {
      type: 'program_start',
      location: { file: programName, line: 0 },
      data: { name: programName, parameters: this.anonymizeIfNeeded(inputs) as Record<string, unknown> },
    });
    
    return sessionId;
  }

  endTrace(sessionId: string, outputs: Record<string, unknown>): ExecutionTrace {
    const state = this.activeTraces.get(sessionId);
    if (!state) {
      throw new Error(`No active trace found for session ${sessionId}`);
    }
    
    const endTime = Date.now();
    
    // Record program end event
    this.recordEvent(sessionId, {
      type: 'program_end',
      location: { file: state.programName, line: 0 },
      data: { returnValue: this.anonymizeIfNeeded(outputs) },
    });
    
    const trace: ExecutionTrace = {
      id: generateId(),
      sessionId,
      projectId: state.projectId,
      programName: state.programName,
      language: state.language,
      startTime: state.startTime,
      endTime,
      durationMs: endTime - state.startTime,
      inputs: state.inputs,
      outputs: this.captureOutputs(outputs),
      events: state.events,
      callStack: state.callStack,
      metrics: this.finalizeMetrics(state),
      capturedAt: new Date(),
    };
    
    this.traces.set(sessionId, trace);
    this.activeTraces.delete(sessionId);
    
    // Persist if configured
    if (this.config.storageBackend !== 'memory') {
      this.persistTrace(trace).catch(err => {
        console.error('Failed to persist trace:', err);
      });
    }
    
    return trace;
  }

  recordEvent(
    sessionId: string,
    event: Omit<TraceEvent, 'id' | 'sessionId' | 'timestamp' | 'sequenceNumber'>
  ): void {
    const state = this.activeTraces.get(sessionId);
    if (!state) return;
    
    // Check sampling
    if (this.config.samplingRate < 1.0 && Math.random() > this.config.samplingRate) {
      return;
    }
    
    // Check max events
    if (state.events.length >= this.config.maxEventsPerTrace) {
      return;
    }
    
    const fullEvent: TraceEvent = {
      id: generateId(),
      sessionId,
      timestamp: Date.now(),
      sequenceNumber: state.sequenceCounter++,
      type: event.type,
      location: event.location,
      data: (this.anonymizeIfNeeded(event.data) as TraceEventData) || {},
      metadata: event.metadata,
    };
    
    state.events.push(fullEvent);
    this.updateMetrics(state, fullEvent);
    this.updateCallStack(state, fullEvent);
  }

  getTrace(sessionId: string): ExecutionTrace | null {
    return this.traces.get(sessionId) ?? null;
  }

  listTraces(filter?: TraceFilter): ExecutionTrace[] {
    let traces = Array.from(this.traces.values());
    
    if (filter) {
      if (filter.projectId) {
        traces = traces.filter(t => t.projectId === filter.projectId);
      }
      if (filter.programName) {
        traces = traces.filter(t => t.programName === filter.programName);
      }
      if (filter.startTimeAfter) {
        traces = traces.filter(t => t.startTime >= filter.startTimeAfter!.getTime());
      }
      if (filter.startTimeBefore) {
        traces = traces.filter(t => t.startTime <= filter.startTimeBefore!.getTime());
      }
      if (filter.minDuration !== undefined) {
        traces = traces.filter(t => t.durationMs >= filter.minDuration!);
      }
      if (filter.maxDuration !== undefined) {
        traces = traces.filter(t => t.durationMs <= filter.maxDuration!);
      }
      if (filter.hasErrors !== undefined) {
        traces = traces.filter(t => 
          (t.metrics.errorsEncountered > 0) === filter.hasErrors
        );
      }
      if (filter.offset) {
        traces = traces.slice(filter.offset);
      }
      if (filter.limit) {
        traces = traces.slice(0, filter.limit);
      }
    }
    
    return traces;
  }

  // Abstract methods for subclasses
  protected abstract onInitialize(): Promise<void>;
  protected abstract onShutdown(): Promise<void>;

  // Helper methods
  protected captureInputs(inputs: Record<string, unknown>): CapturedIO {
    return {
      parameters: this.anonymizeIfNeeded(inputs) as Record<string, unknown>,
      files: [],
      database: [],
      screens: [],
      messages: [],
    };
  }

  protected captureOutputs(outputs: Record<string, unknown>): CapturedIO {
    return {
      parameters: this.anonymizeIfNeeded(outputs) as Record<string, unknown>,
      files: [],
      database: [],
      screens: [],
      messages: [],
    };
  }

  protected anonymizeIfNeeded(data: unknown): unknown {
    if (!this.config.anonymizePII || !data) return data;
    
    if (typeof data === 'string') {
      return this.anonymizeString(data);
    }
    
    if (Array.isArray(data)) {
      return data.map(item => this.anonymizeIfNeeded(item));
    }
    
    if (typeof data === 'object') {
      const result: Record<string, unknown> = {};
      for (const [key, value] of Object.entries(data)) {
        if (this.config.excludeFields?.includes(key)) {
          result[key] = '[REDACTED]';
        } else {
          result[key] = this.anonymizeIfNeeded(value);
        }
      }
      return result;
    }
    
    return data;
  }

  protected anonymizeString(str: string): string {
    if (!this.config.piiPatterns) return str;
    
    let result = str;
    for (const pattern of this.config.piiPatterns) {
      result = result.replace(pattern, '[PII_REDACTED]');
    }
    return result;
  }

  protected createEmptyMetrics(): TraceMetrics {
    return {
      totalEvents: 0,
      proceduresCalled: 0,
      branchesTaken: 0,
      loopIterations: 0,
      fileOperations: 0,
      dbOperations: 0,
      externalCalls: 0,
      errorsEncountered: 0,
      uniqueCodePaths: 0,
    };
  }

  protected updateMetrics(state: ActiveTraceState, event: TraceEvent): void {
    state.metrics.totalEvents++;
    
    switch (event.type) {
      case 'procedure_entry':
        state.metrics.proceduresCalled++;
        break;
      case 'branch_taken':
        state.metrics.branchesTaken++;
        break;
      case 'loop_iteration':
        state.metrics.loopIterations++;
        break;
      case 'file_open':
      case 'file_read':
      case 'file_write':
      case 'file_close':
        state.metrics.fileOperations++;
        break;
      case 'db_query':
        state.metrics.dbOperations++;
        break;
      case 'external_call':
        state.metrics.externalCalls++;
        break;
      case 'error':
        state.metrics.errorsEncountered++;
        break;
    }
  }

  protected updateCallStack(state: ActiveTraceState, event: TraceEvent): void {
    if (event.type === 'procedure_entry') {
      const entry: CallStackEntry = {
        procedure: event.data.name || 'unknown',
        enteredAt: event.timestamp,
        parameters: event.data.parameters,
        depth: state.currentDepth++,
      };
      state.callStack.push(entry);
    } else if (event.type === 'procedure_exit') {
      const lastEntry = state.callStack[state.callStack.length - 1];
      if (lastEntry && !lastEntry.exitedAt) {
        lastEntry.exitedAt = event.timestamp;
        lastEntry.returnValue = event.data.returnValue;
        state.currentDepth--;
      }
    }
  }

  protected finalizeMetrics(state: ActiveTraceState): TraceMetrics {
    // Calculate unique code paths from branch events
    const paths = new Set<string>();
    for (const event of state.events) {
      if (event.type === 'branch_taken' || event.type === 'branch_not_taken') {
        paths.add(`${event.location.file}:${event.location.line}:${event.type}`);
      }
    }
    state.metrics.uniqueCodePaths = paths.size;
    return state.metrics;
  }

  protected async persistTrace(_trace: ExecutionTrace): Promise<void> {
    // Override in subclass if persistence needed
  }
}

interface ActiveTraceState {
  sessionId: string;
  projectId: string;
  programName: string;
  language: SourceLanguage;
  startTime: number;
  events: TraceEvent[];
  callStack: CallStackEntry[];
  currentDepth: number;
  sequenceCounter: number;
  inputs: CapturedIO;
  metrics: TraceMetrics;
}

// Re-export default config
export { DEFAULT_TRACER_CONFIG };
