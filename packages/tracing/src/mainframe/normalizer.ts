/**
 * Trace Normalizer
 * 
 * Normalizes mainframe execution traces into a standardized format
 * for behavioral analysis and rule inference.
 */

import { generateId } from '@migrationpilot/core';
import type { TraceEvent, TraceLocation } from '../types.js';
import type {
  MainframeExecutionTrace,
  NormalizedTrace,
  NormalizedProgram,
  NormalizedProcedure,
  NormalizedDataStructure,
  ExecutionPathNode,
  NormalizedDataSet,
  NormalizedVariable,
  InferredRule,
} from './types.js';

export class TraceNormalizer {
  private projectId: string;

  constructor(projectId: string) {
    this.projectId = projectId;
  }

  /**
   * Normalize a mainframe execution trace
   */
  normalize(trace: MainframeExecutionTrace): NormalizedTrace {
    const program = this.normalizeProgram(trace);
    const executionPath = this.buildExecutionPath(trace.events);
    const inputData = this.extractInputData(trace);
    const outputData = this.extractOutputData(trace);
    const intermediateStates = this.captureIntermediateStates(trace.events);
    const inferredRules = this.inferRules(trace, executionPath);

    return {
      id: generateId(),
      projectId: this.projectId,
      sourceTraceId: trace.id,
      sourceType: 'mainframe',
      program,
      executionPath,
      inputData,
      outputData,
      intermediateStates,
      inferredRules,
      normalizedAt: new Date(),
    };
  }

  /**
   * Normalize program structure from trace
   */
  private normalizeProgram(trace: MainframeExecutionTrace): NormalizedProgram {
    const procedures = this.extractProcedures(trace.events);
    const dataStructures = this.extractDataStructures(trace);

    return {
      name: trace.programName,
      language: trace.language,
      entryPoint: this.findEntryPoint(trace.events),
      procedures,
      dataStructures,
    };
  }

  /**
   * Extract procedures from trace events
   */
  private extractProcedures(events: TraceEvent[]): NormalizedProcedure[] {
    const procedureMap = new Map<string, ProcedureStats>();

    // Build call graph
    const callStack: string[] = [];
    
    for (const event of events) {
      if (event.type === 'procedure_entry' && event.data.name) {
        const name = event.data.name as string;
        const stats = procedureMap.get(name) || {
          name,
          type: this.inferProcedureType(event),
          callCount: 0,
          totalDuration: 0,
          calledBy: new Set<string>(),
          calls: new Set<string>(),
          entryTime: 0,
        };
        
        stats.callCount++;
        stats.entryTime = event.timestamp;
        
        if (callStack.length > 0) {
          const caller = callStack[callStack.length - 1]!;
          stats.calledBy.add(caller);
          const callerStats = procedureMap.get(caller);
          if (callerStats) {
            callerStats.calls.add(name);
          }
        }
        
        callStack.push(name);
        procedureMap.set(name, stats);
      } else if (event.type === 'procedure_exit') {
        const name = callStack.pop();
        if (name) {
          const stats = procedureMap.get(name);
          if (stats && stats.entryTime) {
            stats.totalDuration += event.timestamp - stats.entryTime;
          }
        }
      }
    }

    return Array.from(procedureMap.values()).map(stats => ({
      name: stats.name,
      type: stats.type,
      callCount: stats.callCount,
      averageDurationMs: stats.callCount > 0 ? stats.totalDuration / stats.callCount : 0,
      calledBy: Array.from(stats.calledBy),
      calls: Array.from(stats.calls),
    }));
  }

  private inferProcedureType(event: TraceEvent): NormalizedProcedure['type'] {
    const location = event.location;
    if (location.procedure?.includes('SECTION')) {
      return 'section';
    }
    return 'paragraph';
  }

  /**
   * Extract data structures from trace
   */
  private extractDataStructures(trace: MainframeExecutionTrace): NormalizedDataStructure[] {
    const structures: NormalizedDataStructure[] = [];

    // Extract from working storage if available
    if (trace.inputs.workingStorage) {
      for (const [name, value] of Object.entries(trace.inputs.workingStorage)) {
        structures.push(this.inferDataStructure(name, value));
      }
    }

    // Extract from linkage section
    if (trace.inputs.linkageSection) {
      for (const [name, value] of Object.entries(trace.inputs.linkageSection)) {
        structures.push(this.inferDataStructure(name, value));
      }
    }

    // Infer from variable events
    const variableEvents = trace.events.filter(e => 
      e.type === 'variable_read' || e.type === 'variable_write'
    );

    const seenVariables = new Set<string>(structures.map(s => s.name));
    for (const event of variableEvents) {
      const name = event.data.name as string;
      if (name && !seenVariables.has(name)) {
        structures.push(this.inferDataStructure(name, event.data.value));
        seenVariables.add(name);
      }
    }

    return structures;
  }

  private inferDataStructure(name: string, value: unknown): NormalizedDataStructure {
    let type = 'string';
    let length = 0;

    if (typeof value === 'number') {
      type = Number.isInteger(value) ? 'integer' : 'decimal';
      length = String(value).length;
    } else if (typeof value === 'string') {
      type = 'alphanumeric';
      length = value.length;
    } else if (typeof value === 'object' && value !== null) {
      type = 'group';
    }

    return {
      name,
      level: 1, // Would be inferred from COBOL copybook
      type,
      length,
    };
  }

  /**
   * Find the entry point of the program
   */
  private findEntryPoint(events: TraceEvent[]): string {
    const firstEntry = events.find(e => e.type === 'procedure_entry');
    return (firstEntry?.data.name as string) || 'MAIN';
  }

  /**
   * Build execution path from events
   */
  private buildExecutionPath(events: TraceEvent[]): ExecutionPathNode[] {
    const path: ExecutionPathNode[] = [];
    let sequence = 0;

    for (const event of events) {
      const node = this.convertEventToPathNode(event, sequence++);
      if (node) {
        path.push(node);
      }
    }

    // Aggregate consecutive loop iterations
    return this.aggregateLoops(path);
  }

  private convertEventToPathNode(event: TraceEvent, sequence: number): ExecutionPathNode | null {
    switch (event.type) {
      case 'procedure_entry':
        return {
          sequenceNumber: sequence,
          nodeType: 'call',
          location: event.location,
          calledProcedure: event.data.name as string,
          parameters: event.data.parameters as Record<string, unknown>,
        };
      
      case 'procedure_exit':
        return {
          sequenceNumber: sequence,
          nodeType: 'return',
          location: event.location,
          relevantVariables: event.data.returnValue 
            ? { returnValue: event.data.returnValue }
            : undefined,
        };
      
      case 'branch_taken':
      case 'branch_not_taken':
        return {
          sequenceNumber: sequence,
          nodeType: 'branch',
          location: event.location,
          condition: event.data.condition as string,
          conditionResult: event.type === 'branch_taken',
        };
      
      case 'loop_iteration':
        return {
          sequenceNumber: sequence,
          nodeType: 'loop',
          location: event.location,
          iterationCount: 1,
        };
      
      case 'variable_write':
        return {
          sequenceNumber: sequence,
          nodeType: 'statement',
          location: event.location,
          relevantVariables: {
            [event.data.name as string]: event.data.value,
          },
        };
      
      default:
        return null;
    }
  }

  private aggregateLoops(path: ExecutionPathNode[]): ExecutionPathNode[] {
    const aggregated: ExecutionPathNode[] = [];
    let i = 0;

    while (i < path.length) {
      const current = path[i]!;
      
      if (current.nodeType === 'loop') {
        // Count consecutive loop iterations at same location
        let count = 1;
        while (
          i + count < path.length &&
          path[i + count]!.nodeType === 'loop' &&
          this.sameLocation(current.location, path[i + count]!.location)
        ) {
          count++;
        }
        
        aggregated.push({
          ...current,
          iterationCount: count,
        });
        i += count;
      } else {
        aggregated.push(current);
        i++;
      }
    }

    return aggregated;
  }

  private sameLocation(a: TraceLocation, b: TraceLocation): boolean {
    return a.file === b.file && a.line === b.line;
  }

  /**
   * Extract input data from trace
   */
  private extractInputData(trace: MainframeExecutionTrace): NormalizedDataSet {
    const variables: NormalizedVariable[] = [];

    // From parameters
    for (const [name, value] of Object.entries(trace.inputs.parameters)) {
      variables.push({
        name,
        qualifiedName: name,
        type: typeof value,
        value,
      });
    }

    // From commarea
    if (trace.inputs.commarea?.parsedFields) {
      for (const [name, value] of Object.entries(trace.inputs.commarea.parsedFields)) {
        variables.push({
          name,
          qualifiedName: `COMMAREA.${name}`,
          type: typeof value,
          value,
        });
      }
    }

    return {
      timestamp: trace.startTime,
      variables,
      files: trace.inputs.files.map(f => ({
        name: f.filename,
        records: f.records,
      })),
      databases: trace.inputs.database.map(d => ({
        table: d.table || '',
        operation: d.operation,
        affectedRows: d.results,
      })),
    };
  }

  /**
   * Extract output data from trace
   */
  private extractOutputData(trace: MainframeExecutionTrace): NormalizedDataSet {
    const variables: NormalizedVariable[] = [];

    // From output parameters
    for (const [name, value] of Object.entries(trace.outputs.parameters)) {
      variables.push({
        name,
        qualifiedName: name,
        type: typeof value,
        value,
      });
    }

    return {
      timestamp: trace.endTime,
      variables,
      files: trace.outputs.files.map(f => ({
        name: f.filename,
        records: f.records,
      })),
      databases: trace.outputs.database.map(d => ({
        table: d.table || '',
        operation: d.operation,
        affectedRows: d.results,
      })),
    };
  }

  /**
   * Capture intermediate state changes
   */
  private captureIntermediateStates(events: TraceEvent[]): NormalizedDataSet[] {
    const states: NormalizedDataSet[] = [];
    const currentState = new Map<string, unknown>();
    let lastCaptureTime = 0;
    const captureInterval = 100; // Capture state every 100ms

    for (const event of events) {
      if (event.type === 'variable_write' && event.data.name) {
        currentState.set(event.data.name as string, event.data.value);
        
        if (event.timestamp - lastCaptureTime > captureInterval) {
          states.push({
            timestamp: event.timestamp,
            variables: Array.from(currentState.entries()).map(([name, value]) => ({
              name,
              qualifiedName: name,
              type: typeof value,
              value,
            })),
            files: [],
            databases: [],
          });
          lastCaptureTime = event.timestamp;
        }
      }
    }

    return states;
  }

  /**
   * Infer business rules from trace
   */
  private inferRules(
    trace: MainframeExecutionTrace,
    executionPath: ExecutionPathNode[]
  ): InferredRule[] {
    const rules: InferredRule[] = [];

    // Infer calculation rules
    rules.push(...this.inferCalculationRules(trace.events));

    // Infer validation rules
    rules.push(...this.inferValidationRules(executionPath));

    // Infer decision rules
    rules.push(...this.inferDecisionRules(executionPath));

    // Infer transformation rules
    rules.push(...this.inferTransformationRules(trace));

    return rules;
  }

  private inferCalculationRules(events: TraceEvent[]): InferredRule[] {
    const rules: InferredRule[] = [];
    const calculations = new Map<string, CalculationObservation[]>();

    // Group variable writes by output variable
    for (let i = 0; i < events.length; i++) {
      const event = events[i]!;
      if (event.type !== 'variable_write' || !event.data.name) continue;

      const outputVar = event.data.name as string;
      const observations = calculations.get(outputVar) || [];

      // Look for recent variable reads that might be inputs
      const recentReads: Record<string, unknown> = {};
      for (let j = Math.max(0, i - 10); j < i; j++) {
        const readEvent = events[j]!;
        if (readEvent.type === 'variable_read' && readEvent.data.name) {
          recentReads[readEvent.data.name as string] = readEvent.data.value;
        }
      }

      observations.push({
        output: event.data.value,
        inputs: recentReads,
        location: event.location,
      });

      calculations.set(outputVar, observations);
    }

    // Analyze observations to infer formulas
    for (const [outputVar, observations] of calculations) {
      if (observations.length < 2) continue;

      const formula = this.inferFormula(observations);
      if (!formula) continue;

      rules.push({
        id: generateId(),
        type: 'calculation',
        description: `Calculate ${outputVar}`,
        confidence: Math.min(observations.length / 10, 0.9),
        sourceLocations: observations.map(o => o.location),
        inputs: Object.keys(observations[0]?.inputs || {}),
        outputs: [outputVar],
        logic: formula.description,
        formula: formula.expression,
        observationCount: observations.length,
        exampleTraces: [],
      });
    }

    return rules;
  }

  private inferFormula(observations: CalculationObservation[]): FormulaInference | null {
    // Check for constant output
    const uniqueOutputs = new Set(observations.map(o => JSON.stringify(o.output)));
    if (uniqueOutputs.size === 1) {
      return {
        expression: `${observations[0]!.output}`,
        description: `Always returns ${observations[0]!.output}`,
      };
    }

    // Check for simple arithmetic (addition)
    const firstInputKeys = Object.keys(observations[0]?.inputs || {});
    if (firstInputKeys.length === 2) {
      const [a, b] = firstInputKeys;
      const isAddition = observations.every(o => {
        const inputA = o.inputs[a!] as number;
        const inputB = o.inputs[b!] as number;
        const output = o.output as number;
        return typeof inputA === 'number' && 
               typeof inputB === 'number' && 
               Math.abs(inputA + inputB - output) < 0.01;
      });

      if (isAddition) {
        return {
          expression: `${a} + ${b}`,
          description: `Sum of ${a} and ${b}`,
        };
      }
    }

    return null;
  }

  private inferValidationRules(executionPath: ExecutionPathNode[]): InferredRule[] {
    const rules: InferredRule[] = [];
    const validations = new Map<string, ValidationObservation[]>();

    for (const node of executionPath) {
      if (node.nodeType !== 'branch' || !node.condition) continue;

      const observations = validations.get(node.condition) || [];
      observations.push({
        condition: node.condition,
        result: node.conditionResult || false,
        location: node.location,
      });
      validations.set(node.condition, observations);
    }

    for (const [condition, observations] of validations) {
      if (observations.length < 2) continue;

      const trueCount = observations.filter(o => o.result).length;
      const falseCount = observations.length - trueCount;

      rules.push({
        id: generateId(),
        type: 'validation',
        description: `Validate: ${condition}`,
        confidence: 0.75,
        sourceLocations: observations.map(o => o.location),
        inputs: this.extractConditionVariables(condition),
        outputs: [],
        logic: `When ${condition} is true (${trueCount} times), proceed. When false (${falseCount} times), take alternative path.`,
        observationCount: observations.length,
        exampleTraces: [],
      });
    }

    return rules;
  }

  private extractConditionVariables(condition: string): string[] {
    // Simple variable extraction from condition string
    const varPattern = /[A-Z][A-Z0-9-]*/g;
    const matches = condition.match(varPattern) || [];
    return [...new Set(matches)];
  }

  private inferDecisionRules(executionPath: ExecutionPathNode[]): InferredRule[] {
    const rules: InferredRule[] = [];
    
    // Find decision points (multiple branches from same location)
    const branchesByLocation = new Map<string, ExecutionPathNode[]>();
    
    for (const node of executionPath) {
      if (node.nodeType !== 'branch') continue;
      const key = `${node.location.file}:${node.location.line}`;
      const branches = branchesByLocation.get(key) || [];
      branches.push(node);
      branchesByLocation.set(key, branches);
    }

    for (const [location, branches] of branchesByLocation) {
      if (branches.length < 2) continue;

      const conditions = [...new Set(branches.map(b => b.condition).filter(Boolean))];
      if (conditions.length === 0) continue;

      rules.push({
        id: generateId(),
        type: 'decision',
        description: `Decision at ${location}`,
        confidence: 0.7,
        sourceLocations: branches.map(b => b.location),
        inputs: conditions.flatMap(c => this.extractConditionVariables(c || '')),
        outputs: [],
        logic: `Multi-way decision based on: ${conditions.join(', ')}`,
        observationCount: branches.length,
        exampleTraces: [],
      });
    }

    return rules;
  }

  private inferTransformationRules(trace: MainframeExecutionTrace): InferredRule[] {
    const rules: InferredRule[] = [];

    // Compare input and output parameters for transformations
    for (const [inputName, inputValue] of Object.entries(trace.inputs.parameters)) {
      for (const [outputName, outputValue] of Object.entries(trace.outputs.parameters)) {
        const transformation = this.detectTransformation(inputValue, outputValue);
        if (transformation) {
          rules.push({
            id: generateId(),
            type: 'transformation',
            description: `Transform ${inputName} to ${outputName}`,
            confidence: 0.8,
            sourceLocations: [],
            inputs: [inputName],
            outputs: [outputName],
            logic: transformation.description,
            formula: transformation.formula,
            observationCount: 1,
            exampleTraces: [trace.id],
          });
        }
      }
    }

    return rules;
  }

  private detectTransformation(
    input: unknown, 
    output: unknown
  ): { description: string; formula?: string } | null {
    if (typeof input !== typeof output) return null;

    if (typeof input === 'string' && typeof output === 'string') {
      if (input.toUpperCase() === output) {
        return { description: 'Convert to uppercase', formula: 'UPPER(input)' };
      }
      if (input.toLowerCase() === output) {
        return { description: 'Convert to lowercase', formula: 'LOWER(input)' };
      }
      if (input.trim() === output) {
        return { description: 'Trim whitespace', formula: 'TRIM(input)' };
      }
      if (input.length !== output.length) {
        return { description: 'String transformation', formula: 'TRANSFORM(input)' };
      }
    }

    if (typeof input === 'number' && typeof output === 'number') {
      if (Math.abs(input * 100 - output) < 0.01) {
        return { description: 'Multiply by 100 (percentage conversion)', formula: 'input * 100' };
      }
      if (Math.abs(input / 100 - output) < 0.0001) {
        return { description: 'Divide by 100 (percentage conversion)', formula: 'input / 100' };
      }
    }

    return null;
  }
}

// Supporting interfaces
interface ProcedureStats {
  name: string;
  type: NormalizedProcedure['type'];
  callCount: number;
  totalDuration: number;
  calledBy: Set<string>;
  calls: Set<string>;
  entryTime: number;
}

interface CalculationObservation {
  output: unknown;
  inputs: Record<string, unknown>;
  location: TraceLocation;
}

interface ValidationObservation {
  condition: string;
  result: boolean;
  location: TraceLocation;
}

interface FormulaInference {
  expression: string;
  description: string;
}
