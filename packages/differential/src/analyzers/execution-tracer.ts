/**
 * Execution Tracer - Records execution traces for comparison
 */

import type {
  ExecutionTrace,
  TraceStep,
  BranchInfo,
  CallInfo,
  MemorySnapshot,
  CodeLocation,
  DataValue,
} from '../types.js';

// ============================================================================
// Execution Tracer
// ============================================================================

export class ExecutionTracer {
  private steps: TraceStep[] = [];
  private branches: BranchInfo[] = [];
  private calls: CallInfo[] = [];
  private memory: MemorySnapshot[] = [];
  private startTime: number = 0;

  /**
   * Start tracing
   */
  start(): void {
    this.startTime = Date.now();
    this.steps = [];
    this.branches = [];
    this.calls = [];
    this.memory = [];
  }

  /**
   * Record a trace step
   */
  recordStep(
    location: CodeLocation,
    operation: string,
    values: Record<string, unknown>
  ): void {
    this.steps.push({
      timestamp: Date.now() - this.startTime,
      location,
      operation,
      values,
    });
  }

  /**
   * Record a branch decision
   */
  recordBranch(
    location: CodeLocation,
    condition: string,
    result: boolean
  ): void {
    const existing = this.branches.find(
      b => b.location.file === location.file && 
           b.location.line === location.line &&
           b.condition === condition
    );

    if (existing) {
      if (existing.result !== result) {
        existing.coverage = 'both';
      }
    } else {
      this.branches.push({
        location,
        condition,
        result,
        coverage: result ? 'taken' : 'not-taken',
      });
    }
  }

  /**
   * Record a function call
   */
  recordCall(
    location: CodeLocation,
    target: string,
    args: unknown[],
    returnValue: unknown,
    duration: number
  ): void {
    this.calls.push({
      location,
      target,
      arguments: args,
      returnValue,
      duration,
    });
  }

  /**
   * Record a memory snapshot
   */
  recordMemory(variables: Record<string, DataValue>): void {
    this.memory.push({
      timestamp: Date.now() - this.startTime,
      variables: { ...variables },
    });
  }

  /**
   * Get the complete execution trace
   */
  getTrace(): ExecutionTrace {
    return {
      steps: [...this.steps],
      branches: [...this.branches],
      calls: [...this.calls],
      memory: [...this.memory],
    };
  }

  /**
   * Get trace statistics
   */
  getStats(): TraceStats {
    return {
      stepCount: this.steps.length,
      branchCount: this.branches.length,
      callCount: this.calls.length,
      memorySnapshotCount: this.memory.length,
      branchCoverage: this.calculateBranchCoverage(),
      uniqueLocations: this.getUniqueLocations().length,
      totalDuration: this.steps.length > 0 
        ? this.steps[this.steps.length - 1]!.timestamp 
        : 0,
    };
  }

  /**
   * Calculate branch coverage percentage
   */
  private calculateBranchCoverage(): number {
    if (this.branches.length === 0) return 100;
    const fullyCovered = this.branches.filter(b => b.coverage === 'both').length;
    return (fullyCovered / this.branches.length) * 100;
  }

  /**
   * Get unique code locations visited
   */
  private getUniqueLocations(): CodeLocation[] {
    const seen = new Set<string>();
    const unique: CodeLocation[] = [];

    for (const step of this.steps) {
      const key = `${step.location.file}:${step.location.line}`;
      if (!seen.has(key)) {
        seen.add(key);
        unique.push(step.location);
      }
    }

    return unique;
  }

  /**
   * Clear the trace
   */
  clear(): void {
    this.steps = [];
    this.branches = [];
    this.calls = [];
    this.memory = [];
    this.startTime = 0;
  }
}

// ============================================================================
// Trace Statistics
// ============================================================================

export interface TraceStats {
  stepCount: number;
  branchCount: number;
  callCount: number;
  memorySnapshotCount: number;
  branchCoverage: number;
  uniqueLocations: number;
  totalDuration: number;
}

// ============================================================================
// Trace Analyzer - Analyzes execution traces
// ============================================================================

export class TraceAnalyzer {
  /**
   * Compare two execution traces
   */
  compare(legacy: ExecutionTrace, migrated: ExecutionTrace): TraceComparisonResult {
    return {
      branchDiffs: this.compareBranches(legacy.branches, migrated.branches),
      callDiffs: this.compareCalls(legacy.calls, migrated.calls),
      pathDiffs: this.comparePaths(legacy.steps, migrated.steps),
      memoryDiffs: this.compareMemory(legacy.memory, migrated.memory),
      summary: this.summarizeComparison(legacy, migrated),
    };
  }

  /**
   * Compare branch decisions
   */
  private compareBranches(
    legacy: BranchInfo[],
    migrated: BranchInfo[]
  ): BranchDiff[] {
    const diffs: BranchDiff[] = [];
    const migratedMap = new Map<string, BranchInfo>();

    for (const b of migrated) {
      const key = `${b.location.file}:${b.location.line}:${b.condition}`;
      migratedMap.set(key, b);
    }

    for (const legacyBranch of legacy) {
      const key = `${legacyBranch.location.file}:${legacyBranch.location.line}:${legacyBranch.condition}`;
      const migratedBranch = migratedMap.get(key);

      if (!migratedBranch) {
        diffs.push({
          type: 'missing-in-migrated',
          location: legacyBranch.location,
          condition: legacyBranch.condition,
          legacy: legacyBranch,
          migrated: undefined,
        });
      } else if (legacyBranch.result !== migratedBranch.result) {
        diffs.push({
          type: 'different-result',
          location: legacyBranch.location,
          condition: legacyBranch.condition,
          legacy: legacyBranch,
          migrated: migratedBranch,
        });
      }
    }

    // Check for branches only in migrated
    for (const [key, migratedBranch] of migratedMap) {
      const exists = legacy.some(
        b => `${b.location.file}:${b.location.line}:${b.condition}` === key
      );
      if (!exists) {
        diffs.push({
          type: 'added-in-migrated',
          location: migratedBranch.location,
          condition: migratedBranch.condition,
          legacy: undefined,
          migrated: migratedBranch,
        });
      }
    }

    return diffs;
  }

  /**
   * Compare function calls
   */
  private compareCalls(
    legacy: CallInfo[],
    migrated: CallInfo[]
  ): CallDiff[] {
    const diffs: CallDiff[] = [];

    // Compare by sequence
    const maxLen = Math.max(legacy.length, migrated.length);
    
    for (let i = 0; i < maxLen; i++) {
      const legacyCall = legacy[i];
      const migratedCall = migrated[i];

      if (!legacyCall && migratedCall) {
        diffs.push({
          type: 'added-in-migrated',
          index: i,
          legacy: undefined,
          migrated: migratedCall,
        });
      } else if (legacyCall && !migratedCall) {
        diffs.push({
          type: 'missing-in-migrated',
          index: i,
          legacy: legacyCall,
          migrated: undefined,
        });
      } else if (legacyCall && migratedCall && legacyCall.target !== migratedCall.target) {
        diffs.push({
          type: 'different-target',
          index: i,
          legacy: legacyCall,
          migrated: migratedCall,
        });
      }
    }

    return diffs;
  }

  /**
   * Compare execution paths
   */
  private comparePaths(
    legacy: TraceStep[],
    migrated: TraceStep[]
  ): PathDiff[] {
    const diffs: PathDiff[] = [];
    
    // Extract unique paths
    const legacyPath = legacy.map(s => `${s.location.file}:${s.location.line}`);
    const migratedPath = migrated.map(s => `${s.location.file}:${s.location.line}`);

    // Simple LCS-based diff (simplified)
    const legacySet = new Set(legacyPath);
    const migratedSet = new Set(migratedPath);

    for (const loc of legacySet) {
      if (!migratedSet.has(loc)) {
        diffs.push({
          type: 'path-not-taken',
          location: loc,
          direction: 'legacy-only',
        });
      }
    }

    for (const loc of migratedSet) {
      if (!legacySet.has(loc)) {
        diffs.push({
          type: 'path-not-taken',
          location: loc,
          direction: 'migrated-only',
        });
      }
    }

    return diffs;
  }

  /**
   * Compare memory states
   */
  private compareMemory(
    legacy: MemorySnapshot[],
    migrated: MemorySnapshot[]
  ): MemoryDiff[] {
    const diffs: MemoryDiff[] = [];

    // Compare final states if available
    const legacyFinal = legacy[legacy.length - 1];
    const migratedFinal = migrated[migrated.length - 1];

    if (legacyFinal && migratedFinal) {
      for (const [varName, legacyValue] of Object.entries(legacyFinal.variables)) {
        const migratedValue = migratedFinal.variables[varName];
        
        if (!migratedValue) {
          diffs.push({
            type: 'missing-variable',
            variable: varName,
            legacy: legacyValue,
            migrated: undefined,
          });
        } else if (!this.valuesEqual(legacyValue, migratedValue)) {
          diffs.push({
            type: 'different-value',
            variable: varName,
            legacy: legacyValue,
            migrated: migratedValue,
          });
        }
      }

      // Check for variables only in migrated
      for (const [varName, migratedValue] of Object.entries(migratedFinal.variables)) {
        if (!(varName in legacyFinal.variables)) {
          diffs.push({
            type: 'added-variable',
            variable: varName,
            legacy: undefined,
            migrated: migratedValue,
          });
        }
      }
    }

    return diffs;
  }

  /**
   * Compare two data values
   */
  private valuesEqual(a: DataValue, b: DataValue): boolean {
    if (a.type !== b.type) return false;
    if (a.type === 'number' || a.type === 'decimal') {
      return Math.abs((a.value as number) - (b.value as number)) < 0.0001;
    }
    return JSON.stringify(a.value) === JSON.stringify(b.value);
  }

  /**
   * Create comparison summary
   */
  private summarizeComparison(
    legacy: ExecutionTrace,
    migrated: ExecutionTrace
  ): TraceComparisonSummary {
    return {
      legacySteps: legacy.steps.length,
      migratedSteps: migrated.steps.length,
      legacyBranches: legacy.branches.length,
      migratedBranches: migrated.branches.length,
      legacyCalls: legacy.calls.length,
      migratedCalls: migrated.calls.length,
      stepRatio: migrated.steps.length / Math.max(1, legacy.steps.length),
    };
  }
}

// ============================================================================
// Diff Types
// ============================================================================

export interface TraceComparisonResult {
  branchDiffs: BranchDiff[];
  callDiffs: CallDiff[];
  pathDiffs: PathDiff[];
  memoryDiffs: MemoryDiff[];
  summary: TraceComparisonSummary;
}

export interface BranchDiff {
  type: 'missing-in-migrated' | 'added-in-migrated' | 'different-result';
  location: CodeLocation;
  condition: string;
  legacy?: BranchInfo;
  migrated?: BranchInfo;
}

export interface CallDiff {
  type: 'missing-in-migrated' | 'added-in-migrated' | 'different-target';
  index: number;
  legacy?: CallInfo;
  migrated?: CallInfo;
}

export interface PathDiff {
  type: 'path-not-taken';
  location: string;
  direction: 'legacy-only' | 'migrated-only';
}

export interface MemoryDiff {
  type: 'missing-variable' | 'added-variable' | 'different-value';
  variable: string;
  legacy?: DataValue;
  migrated?: DataValue;
}

export interface TraceComparisonSummary {
  legacySteps: number;
  migratedSteps: number;
  legacyBranches: number;
  migratedBranches: number;
  legacyCalls: number;
  migratedCalls: number;
  stepRatio: number;
}

// ============================================================================
// Factory
// ============================================================================

export function createExecutionTracer(): ExecutionTracer {
  return new ExecutionTracer();
}

export function createTraceAnalyzer(): TraceAnalyzer {
  return new TraceAnalyzer();
}
