/**
 * Fortran Tracer
 * 
 * Specialized tracer for Fortran programs with support for:
 * - Subroutine/Function calls
 * - DO loops
 * - Array operations
 * - I/O statements (READ, WRITE, OPEN, CLOSE)
 * - COMMON blocks
 */

import type { SourceLanguage } from '@migrationpilot/core';
import { BaseTracer } from './base-tracer.js';
import type { TracerConfig, TraceLocation, FileIO } from '../types.js';

export interface FortranTracerConfig extends Partial<TracerConfig> {
  projectId: string;
  
  // Fortran-specific options
  traceSubroutines: boolean;
  traceFunctions: boolean;
  traceDoLoops: boolean;
  traceArrayOps: boolean;
  traceCommonBlocks: boolean;
  traceIO: boolean;
  
  // Dialect support
  dialect: 'f77' | 'f90' | 'f95' | 'f2003' | 'f2008';
}

const DEFAULT_FORTRAN_CONFIG: Partial<FortranTracerConfig> = {
  traceSubroutines: true,
  traceFunctions: true,
  traceDoLoops: true,
  traceArrayOps: true,
  traceCommonBlocks: true,
  traceIO: true,
  dialect: 'f90',
};

export class FortranTracer extends BaseTracer {
  readonly language: SourceLanguage = 'fortran';
  private fortranConfig: FortranTracerConfig;
  
  constructor(config: FortranTracerConfig) {
    super({ ...config, language: 'fortran' });
    this.fortranConfig = { ...DEFAULT_FORTRAN_CONFIG, ...config };
  }

  protected async onInitialize(): Promise<void> {
    // Initialize Fortran-specific resources
  }

  protected async onShutdown(): Promise<void> {
    // Clean up resources
  }

  /**
   * Record subroutine entry
   */
  recordSubroutineEntry(
    sessionId: string,
    name: string,
    parameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.fortranConfig.traceSubroutines) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_entry',
      location,
      data: { name, parameters },
      metadata: { fortranType: 'SUBROUTINE' },
    });
  }

  /**
   * Record subroutine exit
   */
  recordSubroutineExit(
    sessionId: string,
    name: string,
    modifiedParameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.fortranConfig.traceSubroutines) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_exit',
      location,
      data: { name, parameters: modifiedParameters },
      metadata: { fortranType: 'SUBROUTINE' },
    });
  }

  /**
   * Record function entry
   */
  recordFunctionEntry(
    sessionId: string,
    name: string,
    parameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.fortranConfig.traceFunctions) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_entry',
      location,
      data: { name, parameters },
      metadata: { fortranType: 'FUNCTION' },
    });
  }

  /**
   * Record function exit with return value
   */
  recordFunctionExit(
    sessionId: string,
    name: string,
    returnValue: unknown,
    location: TraceLocation
  ): void {
    if (!this.fortranConfig.traceFunctions) return;
    
    this.recordEvent(sessionId, {
      type: 'procedure_exit',
      location,
      data: { name, returnValue },
      metadata: { fortranType: 'FUNCTION' },
    });
  }

  /**
   * Record DO loop iteration
   */
  recordDoLoop(
    sessionId: string,
    variableName: string,
    currentValue: number,
    startValue: number,
    endValue: number,
    step: number,
    location: TraceLocation
  ): void {
    if (!this.fortranConfig.traceDoLoops) return;
    
    this.recordEvent(sessionId, {
      type: 'loop_iteration',
      location,
      data: {
        name: variableName,
        value: currentValue,
      },
      metadata: { 
        fortranType: 'DO', 
        startValue, 
        endValue, 
        step,
      },
    });
  }

  /**
   * Record IF/ELSE IF/ELSE branch
   */
  recordConditional(
    sessionId: string,
    condition: string,
    result: boolean,
    branchType: 'IF' | 'ELSE IF' | 'ELSE',
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: result ? 'branch_taken' : 'branch_not_taken',
      location,
      data: { condition, conditionResult: result },
      metadata: { fortranType: branchType },
    });
  }

  /**
   * Record SELECT CASE
   */
  recordSelectCase(
    sessionId: string,
    selector: string,
    selectedCase: string,
    location: TraceLocation
  ): void {
    this.recordEvent(sessionId, {
      type: 'branch_taken',
      location,
      data: {
        condition: `SELECT CASE (${selector})`,
        conditionResult: true,
        name: selectedCase,
      },
      metadata: { fortranType: 'SELECT CASE', selector, selectedCase },
    });
  }

  /**
   * Record variable assignment
   */
  recordAssignment(
    sessionId: string,
    variableName: string,
    newValue: unknown,
    previousValue: unknown,
    location: TraceLocation,
    isArrayElement = false,
    arrayIndices?: number[]
  ): void {
    if (!this.config.captureVariables) return;
    
    this.recordEvent(sessionId, {
      type: 'variable_write',
      location,
      data: {
        name: isArrayElement ? `${variableName}(${arrayIndices?.join(',')})` : variableName,
        value: newValue,
        previousValue,
      },
      metadata: { 
        fortranType: 'ASSIGNMENT',
        isArrayElement,
        arrayIndices,
      },
    });
  }

  /**
   * Record array operation (slice, reshape, etc.)
   */
  recordArrayOperation(
    sessionId: string,
    operation: 'slice' | 'reshape' | 'transpose' | 'matmul' | 'dot_product',
    arrayName: string,
    result: unknown,
    location: TraceLocation,
    dimensions?: number[]
  ): void {
    if (!this.fortranConfig.traceArrayOps) return;
    
    this.recordEvent(sessionId, {
      type: 'variable_write',
      location,
      data: {
        name: arrayName,
        value: result,
      },
      metadata: { 
        fortranType: 'ARRAY_OP',
        operation,
        dimensions,
      },
    });
  }

  /**
   * Record COMMON block access
   */
  recordCommonBlockAccess(
    sessionId: string,
    blockName: string,
    variableName: string,
    value: unknown,
    isRead: boolean,
    location: TraceLocation
  ): void {
    if (!this.fortranConfig.traceCommonBlocks) return;
    
    this.recordEvent(sessionId, {
      type: isRead ? 'variable_read' : 'variable_write',
      location,
      data: {
        name: `COMMON/${blockName}/${variableName}`,
        value,
      },
      metadata: { fortranType: 'COMMON', blockName, variableName },
    });
  }

  /**
   * Record OPEN statement
   */
  recordFileOpen(
    sessionId: string,
    unit: number,
    filename: string,
    status: string,
    location: TraceLocation,
    iostat?: number
  ): void {
    if (!this.fortranConfig.traceIO) return;
    
    this.recordEvent(sessionId, {
      type: 'file_open',
      location,
      data: {
        name: filename,
      },
      metadata: { 
        fortranType: 'OPEN',
        unit,
        status,
        iostat,
      },
    });
  }

  /**
   * Record CLOSE statement
   */
  recordFileClose(
    sessionId: string,
    unit: number,
    location: TraceLocation,
    iostat?: number
  ): void {
    if (!this.fortranConfig.traceIO) return;
    
    this.recordEvent(sessionId, {
      type: 'file_close',
      location,
      data: { name: `unit ${unit}` },
      metadata: { fortranType: 'CLOSE', unit, iostat },
    });
  }

  /**
   * Record READ statement
   */
  recordRead(
    sessionId: string,
    unit: number,
    format: string | undefined,
    variables: Record<string, unknown>,
    location: TraceLocation,
    iostat?: number
  ): void {
    if (!this.fortranConfig.traceIO) return;
    
    this.recordEvent(sessionId, {
      type: 'file_read',
      location,
      data: {
        name: `unit ${unit}`,
        value: variables,
      },
      metadata: { 
        fortranType: 'READ',
        unit,
        format,
        iostat,
      },
    });
    
    this.updateFileIO(sessionId, `unit_${unit}`, 'READ', variables);
  }

  /**
   * Record WRITE statement
   */
  recordWrite(
    sessionId: string,
    unit: number,
    format: string | undefined,
    values: unknown[],
    location: TraceLocation,
    iostat?: number
  ): void {
    if (!this.fortranConfig.traceIO) return;
    
    this.recordEvent(sessionId, {
      type: 'file_write',
      location,
      data: {
        name: `unit ${unit}`,
        value: values,
      },
      metadata: { 
        fortranType: 'WRITE',
        unit,
        format,
        iostat,
      },
    });
    
    this.updateFileIO(sessionId, `unit_${unit}`, 'WRITE', values);
  }

  /**
   * Record external procedure call
   */
  recordExternalCall(
    sessionId: string,
    procedureName: string,
    parameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.config.captureExternalCalls) return;
    
    this.recordEvent(sessionId, {
      type: 'external_call',
      location,
      data: { name: procedureName, parameters },
      metadata: { fortranType: 'EXTERNAL CALL' },
    });
  }

  /**
   * Record external procedure return
   */
  recordExternalReturn(
    sessionId: string,
    procedureName: string,
    returnValue: unknown,
    modifiedParameters: Record<string, unknown>,
    location: TraceLocation
  ): void {
    if (!this.config.captureExternalCalls) return;
    
    this.recordEvent(sessionId, {
      type: 'external_return',
      location,
      data: {
        name: procedureName,
        returnValue,
        parameters: modifiedParameters,
      },
      metadata: { fortranType: 'EXTERNAL RETURN' },
    });
  }

  private updateFileIO(
    sessionId: string,
    filename: string,
    operation: string,
    recordData?: unknown
  ): void {
    const state = this['activeTraces'].get(sessionId);
    if (!state) return;
    
    const opType = operation === 'READ' ? 'read' : 'write';
    
    let fileIO = state.inputs.files.find(f => f.filename === filename);
    if (!fileIO) {
      fileIO = {
        filename,
        operation: opType as FileIO['operation'],
        records: [],
        recordCount: 0,
      };
      state.inputs.files.push(fileIO);
    }
    
    if (recordData !== undefined) {
      fileIO.records.push(recordData);
      fileIO.recordCount++;
    }
  }
}
