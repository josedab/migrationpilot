/**
 * Execution Trace Recorder
 * 
 * Records execution traces for comparison between legacy and modern systems
 */

import type { ExecutionTrace, TraceEvent, SystemAdapter, ExecutionResult } from './types.js';
import { randomUUID } from 'crypto';

export interface TraceRecorderConfig {
  captureVariables: boolean;
  captureBranches: boolean;
  maxEvents: number;
}

export interface TraceComparison {
  matches: boolean;
  totalEvents: { legacy: number; modern: number };
  matchedEvents: number;
  differences: TraceDifference[];
}

export interface TraceDifference {
  index: number;
  legacyEvent?: TraceEvent;
  modernEvent?: TraceEvent;
  type: 'missing' | 'extra' | 'mismatch';
  description: string;
}

export class TraceRecorder {
  constructor(_config?: Partial<TraceRecorderConfig>) {
    // Configuration available for future enhancements
  }

  /**
   * Record execution trace from a system adapter
   */
  async record(
    adapter: SystemAdapter,
    inputs: Record<string, unknown>,
    systemType: 'legacy' | 'modern'
  ): Promise<ExecutionTrace> {
    const sessionId = randomUUID();
    const startTime = Date.now();
    const events: TraceEvent[] = [];

    // Record input event
    events.push({
      timestamp: Date.now(),
      type: 'input',
      location: 'entry',
      data: inputs,
    });

    // Execute and capture result
    let result: ExecutionResult;
    try {
      result = await adapter.execute(inputs);
    } catch (error) {
      events.push({
        timestamp: Date.now(),
        type: 'return',
        location: 'error',
        data: { error: error instanceof Error ? error.message : String(error) },
      });
      
      return {
        sessionId,
        systemType,
        startTime,
        endTime: Date.now(),
        inputs,
        output: null,
        events,
      };
    }

    // Record output event
    events.push({
      timestamp: Date.now(),
      type: 'output',
      location: 'exit',
      data: result.output,
    });

    return {
      sessionId,
      systemType,
      startTime,
      endTime: Date.now(),
      inputs,
      output: result.output,
      events,
    };
  }

  /**
   * Record traces from both systems
   */
  async recordBoth(
    legacyAdapter: SystemAdapter,
    modernAdapter: SystemAdapter,
    inputs: Record<string, unknown>
  ): Promise<{ legacy: ExecutionTrace; modern: ExecutionTrace }> {
    const [legacy, modern] = await Promise.all([
      this.record(legacyAdapter, inputs, 'legacy'),
      this.record(modernAdapter, inputs, 'modern'),
    ]);

    return { legacy, modern };
  }

  /**
   * Compare two execution traces
   */
  compareTraces(legacy: ExecutionTrace, modern: ExecutionTrace): TraceComparison {
    const differences: TraceDifference[] = [];
    let matchedEvents = 0;

    // Compare outputs first
    if (!this.deepEquals(legacy.output, modern.output)) {
      differences.push({
        index: -1,
        legacyEvent: { timestamp: 0, type: 'output', location: 'exit', data: legacy.output },
        modernEvent: { timestamp: 0, type: 'output', location: 'exit', data: modern.output },
        type: 'mismatch',
        description: 'Output values differ',
      });
    }

    // Compare event sequences
    const legacyEvents = legacy.events.filter(e => e.type !== 'input' && e.type !== 'output');
    const modernEvents = modern.events.filter(e => e.type !== 'input' && e.type !== 'output');

    const maxLen = Math.max(legacyEvents.length, modernEvents.length);

    for (let i = 0; i < maxLen; i++) {
      const legacyEvent = legacyEvents[i];
      const modernEvent = modernEvents[i];

      if (!legacyEvent) {
        differences.push({
          index: i,
          modernEvent,
          type: 'extra',
          description: `Modern system has extra event at index ${i}`,
        });
      } else if (!modernEvent) {
        differences.push({
          index: i,
          legacyEvent,
          type: 'missing',
          description: `Modern system missing event at index ${i}`,
        });
      } else if (!this.eventsMatch(legacyEvent, modernEvent)) {
        differences.push({
          index: i,
          legacyEvent,
          modernEvent,
          type: 'mismatch',
          description: `Events differ at index ${i}`,
        });
      } else {
        matchedEvents++;
      }
    }

    return {
      matches: differences.length === 0,
      totalEvents: {
        legacy: legacy.events.length,
        modern: modern.events.length,
      },
      matchedEvents,
      differences,
    };
  }

  /**
   * Check if two events match
   */
  private eventsMatch(a: TraceEvent, b: TraceEvent): boolean {
    if (a.type !== b.type) return false;
    if (a.location !== b.location) return false;
    return this.deepEquals(a.data, b.data);
  }

  /**
   * Deep equality check
   */
  private deepEquals(a: unknown, b: unknown, tolerance = 0.001): boolean {
    if (a === b) return true;
    if (a === null || b === null) return a === b;
    if (a === undefined || b === undefined) return a === b;

    if (typeof a === 'number' && typeof b === 'number') {
      return Math.abs(a - b) <= tolerance;
    }

    if (typeof a !== typeof b) return false;

    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false;
      return a.every((item, i) => this.deepEquals(item, b[i], tolerance));
    }

    if (typeof a === 'object' && typeof b === 'object') {
      const aObj = a as Record<string, unknown>;
      const bObj = b as Record<string, unknown>;
      const keys = new Set([...Object.keys(aObj), ...Object.keys(bObj)]);
      return Array.from(keys).every(key => 
        this.deepEquals(aObj[key], bObj[key], tolerance)
      );
    }

    return a === b;
  }
}

/**
 * Replay recorded traces against systems
 */
export class TraceReplayer {
  
  /**
   * Replay a trace against a system adapter
   */
  async replay(
    trace: ExecutionTrace,
    adapter: SystemAdapter
  ): Promise<ExecutionTrace> {
    const recorder = new TraceRecorder();
    return recorder.record(adapter, trace.inputs, trace.systemType);
  }

  /**
   * Replay multiple traces and compare
   */
  async replayAndCompare(
    traces: ExecutionTrace[],
    legacyAdapter: SystemAdapter,
    modernAdapter: SystemAdapter
  ): Promise<TraceComparison[]> {
    const recorder = new TraceRecorder();
    const comparisons: TraceComparison[] = [];

    for (const trace of traces) {
      const { legacy, modern } = await recorder.recordBoth(
        legacyAdapter,
        modernAdapter,
        trace.inputs
      );
      comparisons.push(recorder.compareTraces(legacy, modern));
    }

    return comparisons;
  }
}
