/**
 * Legacy Runtime Emulator
 * Sandboxed execution environment for legacy COBOL/Fortran/PL/I code
 */

import type {
  LegacyProgram,
  ExecutionContext,
  ExecutionResult,
  ExecutionOutput,
  ExecutionError,
  ExecutionTrace,
  TraceStep,
  MemoryLayout,
  MemorySegment,
  MemorySnapshot,
  MemoryVariable,
  VariableSnapshot,
  VariableValue,
  VariableType,
  VariableDefinition,
  Instruction,
  RegisterSet,
  ExecutionFlags,
  IOStreams,
  FileHandle,
  FileRecord,
  FileStatus,
  PerformanceMetrics,
  ValidationResult,
  BehaviorDifference,
  CoverageMetrics,
  EmulatorConfig,
  CallFrame,
  PerformFrame,
  ILegacyRuntimeEmulator,
} from './types.js';

const DEFAULT_CONFIG: EmulatorConfig = {
  maxInstructions: 1000000,
  maxMemoryBytes: 16 * 1024 * 1024, // 16MB
  maxExecutionTimeMs: 30000, // 30 seconds
  enableTrace: false,
  traceLevel: 'minimal',
  strictMode: false,
  charset: 'ebcdic',
  decimalPrecision: 18,
  dateFormat: 'yyyymmdd',
};

export class LegacyRuntimeEmulator implements ILegacyRuntimeEmulator {
  private program: LegacyProgram | null = null;
  private context: ExecutionContext | null = null;
  private config: EmulatorConfig;
  private breakpoints: Set<string | number> = new Set();
  private trace: ExecutionTrace | null = null;
  private isRunning = false;
  private isPaused = false;

  constructor(config: Partial<EmulatorConfig> = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  // ============================================================================
  // PROGRAM LOADING
  // ============================================================================

  async loadProgram(program: LegacyProgram): Promise<void> {
    this.program = program;
    this.context = this.initializeContext(program);

    if (this.config.enableTrace) {
      this.trace = { steps: [], branchHistory: [], callHistory: [] };
    }
  }

  private initializeContext(program: LegacyProgram): ExecutionContext {
    const memory = this.initializeMemory(program);

    return {
      programId: program.id,
      memory,
      callStack: [],
      performStack: [],
      fileHandles: this.initializeFileHandles(program),
      registers: this.initializeRegisters(),
      flags: this.initializeFlags(),
      ioStreams: { sysout: [], sysin: [], display: [], accept: [] },
      startTime: new Date(),
      instructionCount: 0,
    };
  }

  private initializeMemory(program: LegacyProgram): MemoryLayout {
    let baseAddress = 0;

    const workingStorage = this.createMemorySegment(
      'WORKING-STORAGE',
      program.dataDivision.workingStorage,
      baseAddress
    );
    baseAddress += workingStorage.totalSize;

    const localStorage = this.createMemorySegment(
      'LOCAL-STORAGE',
      program.dataDivision.localStorage,
      baseAddress
    );
    baseAddress += localStorage.totalSize;

    const linkageSection = this.createMemorySegment(
      'LINKAGE',
      program.dataDivision.linkageSection,
      baseAddress
    );
    baseAddress += linkageSection.totalSize;

    const fileSection = this.createMemorySegment(
      'FILE',
      program.dataDivision.fileSection.flatMap(f => f.recordDescriptions),
      baseAddress
    );

    return { workingStorage, localStorage, linkageSection, fileSection };
  }

  private createMemorySegment(
    name: string,
    definitions: VariableDefinition[],
    baseAddress: number
  ): MemorySegment {
    const variables = new Map<string, MemoryVariable>();
    let offset = 0;

    for (const def of definitions) {
      const variable = this.createMemoryVariable(def, offset);
      variables.set(def.name, variable);
      offset += variable.size;
    }

    return {
      name,
      variables,
      totalSize: offset,
      baseAddress,
    };
  }

  private createMemoryVariable(def: VariableDefinition, offset: number): MemoryVariable {
    const type = this.inferVariableType(def);
    const size = this.calculateVariableSize(def);
    const value = this.parseInitialValue(def.value, type);

    const variable: MemoryVariable = {
      name: def.name,
      type,
      value,
      size,
      offset,
      picture: def.picture,
      level: def.level,
      occurs: def.occurs,
      redefines: def.redefines,
      isGroup: def.level < 77 && def.children && def.children.length > 0,
    };

    if (def.children && def.children.length > 0) {
      variable.children = [];
      let childOffset = 0;
      for (const child of def.children) {
        const childVar = this.createMemoryVariable(child, childOffset);
        variable.children.push(childVar);
        childOffset += childVar.size;
      }
    }

    return variable;
  }

  private inferVariableType(def: VariableDefinition): VariableType {
    if (!def.picture) {
      if (def.children && def.children.length > 0) {
        return 'group';
      }
      return 'alphanumeric';
    }

    const pic = def.picture.toUpperCase();

    if (pic.includes('COMP-3') || def.usage === 'PACKED-DECIMAL') {
      return 'packed-decimal';
    }
    if (pic.includes('COMP') || def.usage === 'BINARY') {
      return 'binary';
    }
    if (pic.includes('9') && !pic.includes('X') && !pic.includes('A')) {
      return 'numeric';
    }
    if (pic.includes('X') || pic.includes('A')) {
      return 'alphanumeric';
    }

    return 'alphanumeric';
  }

  private calculateVariableSize(def: VariableDefinition): number {
    if (!def.picture) {
      if (def.children && def.children.length > 0) {
        return def.children.reduce((sum, child) => sum + this.calculateVariableSize(child), 0);
      }
      return 1;
    }

    const pic = def.picture.toUpperCase().replace(/\((\d+)\)/g, (_, n) => 'X'.repeat(parseInt(n)));
    let size = pic.replace(/[^X9ABS]/g, '').length;

    // Packed decimal uses half the bytes
    if (def.usage === 'PACKED-DECIMAL' || pic.includes('COMP-3')) {
      size = Math.ceil((size + 1) / 2);
    }

    // Apply OCCURS multiplier
    if (def.occurs) {
      size *= def.occurs.times;
    }

    return Math.max(1, size);
  }

  private parseInitialValue(value: string | undefined, type: VariableType): VariableValue {
    if (!value) {
      switch (type) {
        case 'numeric':
        case 'packed-decimal':
        case 'binary':
        case 'float':
          return 0;
        case 'alphanumeric':
          return '';
        default:
          return null;
      }
    }

    // Handle figurative constants
    const upperValue = value.toUpperCase();
    if (upperValue === 'SPACES' || upperValue === 'SPACE') {
      return ' ';
    }
    if (upperValue === 'ZEROS' || upperValue === 'ZEROES' || upperValue === 'ZERO') {
      return type === 'alphanumeric' ? '0' : 0;
    }
    if (upperValue === 'LOW-VALUES' || upperValue === 'LOW-VALUE') {
      return '\x00';
    }
    if (upperValue === 'HIGH-VALUES' || upperValue === 'HIGH-VALUE') {
      return '\xFF';
    }

    // Handle quoted strings
    if (value.startsWith('"') || value.startsWith("'")) {
      return value.slice(1, -1);
    }

    // Handle numeric values
    if (type === 'numeric' || type === 'packed-decimal' || type === 'binary') {
      return parseFloat(value) || 0;
    }

    return value;
  }

  private initializeFileHandles(program: LegacyProgram): Map<string, FileHandle> {
    const handles = new Map<string, FileHandle>();

    for (const fileDef of program.dataDivision.fileSection) {
      handles.set(fileDef.fdName, {
        name: fileDef.fdName,
        ddname: fileDef.fileName,
        status: '42', // File not open
        organization: 'sequential',
        accessMode: 'sequential',
        recordLength: fileDef.recordContains?.max || 80,
        blockSize: fileDef.blockContains || 0,
        currentPosition: 0,
        records: [],
        isOpen: false,
      });
    }

    return handles;
  }

  private initializeRegisters(): RegisterSet {
    return {
      programCounter: 0,
      returnCode: 0,
      addressRegisters: new Array(16).fill(0),
      generalRegisters: new Array(16).fill(0),
    };
  }

  private initializeFlags(): ExecutionFlags {
    return {
      conditionCode: 0,
      overflow: false,
      underflow: false,
      divideByZero: false,
      programCheck: false,
    };
  }

  // ============================================================================
  // EXECUTION
  // ============================================================================

  async execute(inputs: Map<string, VariableValue>): Promise<ExecutionResult> {
    if (!this.program || !this.context) {
      throw new Error('No program loaded');
    }

    // Apply inputs to linkage section
    this.applyInputs(inputs);

    const errors: ExecutionError[] = [];
    const startTime = Date.now();
    this.isRunning = true;
    this.isPaused = false;

    try {
      while (this.isRunning && !this.isPaused) {
        // Check limits
        if (this.context.instructionCount >= this.config.maxInstructions) {
          errors.push({
            code: 'MAX_INSTRUCTIONS',
            message: `Exceeded maximum instruction count: ${this.config.maxInstructions}`,
            severity: 'abend',
          });
          break;
        }

        if (Date.now() - startTime >= this.config.maxExecutionTimeMs) {
          errors.push({
            code: 'TIMEOUT',
            message: `Exceeded maximum execution time: ${this.config.maxExecutionTimeMs}ms`,
            severity: 'abend',
          });
          break;
        }

        // Execute next instruction
        const step = await this.executeStep();

        // Check for breakpoint
        if (this.breakpoints.has(step.instruction.lineNumber) ||
            (step.instruction.label && this.breakpoints.has(step.instruction.label))) {
          this.isPaused = true;
          break;
        }

        // Check for program end
        if (step.instruction.opcode === 'STOP' || step.instruction.opcode === 'GOBACK') {
          this.isRunning = false;
        }
      }
    } catch (error) {
      errors.push({
        code: 'RUNTIME_ERROR',
        message: error instanceof Error ? error.message : 'Unknown error',
        severity: 'abend',
        lineNumber: this.getCurrentInstruction()?.lineNumber,
      });
    }

    const executionTime = Date.now() - startTime;

    return {
      success: errors.filter(e => e.severity === 'abend').length === 0,
      returnCode: this.context.registers.returnCode,
      output: this.collectOutput(),
      memory: this.getMemory(),
      trace: this.trace || undefined,
      errors,
      performance: this.collectPerformanceMetrics(executionTime),
    };
  }

  async executeStep(): Promise<TraceStep> {
    if (!this.program || !this.context) {
      throw new Error('No program loaded');
    }

    const instruction = this.getCurrentInstruction();
    if (!instruction) {
      throw new Error('No instruction at current program counter');
    }

    const stepNumber = this.context.instructionCount++;
    const beforeState = this.config.enableTrace ? this.captureRelevantState(instruction) : {};

    // Execute the instruction
    await this.executeInstruction(instruction);

    const afterState = this.config.enableTrace ? this.captureRelevantState(instruction) : {};

    const step: TraceStep = {
      stepNumber,
      instruction,
      beforeState,
      afterState,
      timestamp: Date.now(),
    };

    if (this.trace) {
      this.trace.steps.push(step);
    }

    return step;
  }

  private getCurrentInstruction(): Instruction | null {
    if (!this.program || !this.context) return null;

    const pc = this.context.registers.programCounter;
    return this.program.compiledInstructions[pc] || null;
  }

  private async executeInstruction(instruction: Instruction): Promise<void> {
    if (!this.context) return;

    switch (instruction.opcode) {
      case 'MOVE':
        this.executeMOVE(instruction);
        break;
      case 'ADD':
        this.executeADD(instruction);
        break;
      case 'SUBTRACT':
        this.executeSUBTRACT(instruction);
        break;
      case 'MULTIPLY':
        this.executeMULTIPLY(instruction);
        break;
      case 'DIVIDE':
        this.executeDIVIDE(instruction);
        break;
      case 'COMPUTE':
        this.executeCOMPUTE(instruction);
        break;
      case 'IF':
        this.executeIF(instruction);
        break;
      case 'PERFORM':
        this.executePERFORM(instruction);
        break;
      case 'GO':
        this.executeGO(instruction);
        break;
      case 'CALL':
        await this.executeCALL(instruction);
        break;
      case 'DISPLAY':
        this.executeDISPLAY(instruction);
        break;
      case 'ACCEPT':
        this.executeACCEPT(instruction);
        break;
      case 'READ':
        this.executeREAD(instruction);
        break;
      case 'WRITE':
        this.executeWRITE(instruction);
        break;
      case 'OPEN':
        this.executeOPEN(instruction);
        break;
      case 'CLOSE':
        this.executeCLOSE(instruction);
        break;
      case 'STRING':
        this.executeSTRING(instruction);
        break;
      case 'UNSTRING':
        this.executeUNSTRING(instruction);
        break;
      case 'INSPECT':
        this.executeINSPECT(instruction);
        break;
      case 'EVALUATE':
        this.executeEVALUATE(instruction);
        break;
      case 'SEARCH':
        this.executeSEARCH(instruction);
        break;
      case 'SET':
        this.executeSET(instruction);
        break;
      case 'INITIALIZE':
        this.executeINITIALIZE(instruction);
        break;
      case 'STOP':
      case 'GOBACK':
        // Handled by execute loop
        break;
      default:
        // Unknown instruction - advance program counter
        break;
    }

    // Advance program counter (unless modified by control flow)
    if (!['GO', 'PERFORM', 'IF', 'EVALUATE'].includes(instruction.opcode)) {
      this.context.registers.programCounter++;
    }
  }

  // ============================================================================
  // INSTRUCTION IMPLEMENTATIONS
  // ============================================================================

  private executeMOVE(instruction: Instruction): void {
    const source = this.resolveOperandValue(instruction.operands[0]!);
    const target = instruction.operands[1]!;

    this.setVariableValue(target.value as string, source);
  }

  private executeADD(instruction: Instruction): void {
    const operands = instruction.operands;
    let sum = 0;

    // ADD a b c TO d GIVING e
    // or ADD a TO b
    for (let i = 0; i < operands.length - 1; i++) {
      const value = this.resolveOperandValue(operands[i]!);
      sum += typeof value === 'number' ? value : parseFloat(String(value)) || 0;
    }

    const target = operands[operands.length - 1]!;
    const currentValue = this.resolveOperandValue(target);
    const finalValue = sum + (typeof currentValue === 'number' ? currentValue : 0);

    this.setVariableValue(target.value as string, finalValue);
  }

  private executeSUBTRACT(instruction: Instruction): void {
    const operands = instruction.operands;
    let subtrahend = 0;

    for (let i = 0; i < operands.length - 1; i++) {
      const value = this.resolveOperandValue(operands[i]!);
      subtrahend += typeof value === 'number' ? value : parseFloat(String(value)) || 0;
    }

    const target = operands[operands.length - 1]!;
    const minuend = this.resolveOperandValue(target);
    const result = (typeof minuend === 'number' ? minuend : 0) - subtrahend;

    this.setVariableValue(target.value as string, result);
  }

  private executeMULTIPLY(instruction: Instruction): void {
    const multiplicand = this.resolveOperandValue(instruction.operands[0]!);
    const multiplier = this.resolveOperandValue(instruction.operands[1]!);
    const target = instruction.operands[2] || instruction.operands[1]!;

    const result =
      (typeof multiplicand === 'number' ? multiplicand : 0) *
      (typeof multiplier === 'number' ? multiplier : 0);

    this.setVariableValue(target.value as string, result);
  }

  private executeDIVIDE(instruction: Instruction): void {
    const dividend = this.resolveOperandValue(instruction.operands[0]!);
    const divisor = this.resolveOperandValue(instruction.operands[1]!);
    const target = instruction.operands[2] || instruction.operands[0]!;

    if (divisor === 0) {
      if (this.context) {
        this.context.flags.divideByZero = true;
      }
      return;
    }

    const result =
      (typeof dividend === 'number' ? dividend : 0) /
      (typeof divisor === 'number' ? divisor : 1);

    this.setVariableValue(target.value as string, result);
  }

  private executeCOMPUTE(instruction: Instruction): void {
    const target = instruction.operands[0]!;
    const expression = instruction.operands
      .slice(1)
      .map(op => this.resolveOperandValue(op))
      .join(' ');

    // Simple expression evaluation
    try {
      // Replace variable references with values
      const evaluated = this.evaluateExpression(expression);
      this.setVariableValue(target.value as string, evaluated);
    } catch {
      // Expression evaluation failed
    }
  }

  private executeIF(instruction: Instruction): void {
    if (!this.context) return;

    const condition = this.evaluateCondition(instruction);

    if (this.trace) {
      this.trace.branchHistory.push({
        stepNumber: this.context.instructionCount,
        condition: instruction.sourceText,
        taken: condition,
        targetLabel: condition ? 'THEN' : 'ELSE',
      });
    }

    // Simplified: just advance to next instruction
    // In full implementation, would jump to appropriate branch
    this.context.registers.programCounter++;
  }

  private executePERFORM(instruction: Instruction): void {
    if (!this.context) return;

    const paragraphName = instruction.operands[0]?.value as string;

    // Find paragraph
    const paragraph = this.program?.procedureDivision.paragraphs.find(
      p => p.name === paragraphName
    );

    if (!paragraph) {
      return;
    }

    // Push perform frame
    this.context.performStack.push({
      paragraphName,
      returnAddress: this.context.registers.programCounter + 1,
    });

    // Jump to paragraph
    const startInstruction = this.program?.compiledInstructions.findIndex(
      i => i.label === paragraphName
    );
    if (startInstruction !== undefined && startInstruction >= 0) {
      this.context.registers.programCounter = startInstruction;
    }
  }

  private executeGO(instruction: Instruction): void {
    if (!this.context) return;

    const targetLabel = instruction.operands[0]?.value as string;

    const targetInstruction = this.program?.compiledInstructions.findIndex(
      i => i.label === targetLabel
    );

    if (targetInstruction !== undefined && targetInstruction >= 0) {
      this.context.registers.programCounter = targetInstruction;
    } else {
      this.context.registers.programCounter++;
    }
  }

  private async executeCALL(instruction: Instruction): Promise<void> {
    if (!this.context) return;

    const programId = instruction.operands[0]?.value as string;

    // Record call
    if (this.trace) {
      this.trace.callHistory.push({
        stepNumber: this.context.instructionCount,
        programId,
        type: 'call',
      });
    }

    // Push call frame
    this.context.callStack.push({
      programId: this.context.programId,
      returnAddress: this.context.registers.programCounter + 1,
      linkageData: new Map(),
      savedRegisters: { ...this.context.registers },
    });

    // In a full implementation, would load and execute the called program
    // For now, simulate return
    this.context.registers.returnCode = 0;
    this.context.registers.programCounter++;

    if (this.trace) {
      this.trace.callHistory.push({
        stepNumber: this.context.instructionCount,
        programId,
        type: 'return',
        returnCode: 0,
      });
    }
  }

  private executeDISPLAY(instruction: Instruction): void {
    if (!this.context) return;

    const values = instruction.operands.map(op => {
      const value = this.resolveOperandValue(op);
      return String(value);
    });

    this.context.ioStreams.display.push(values.join(''));
  }

  private executeACCEPT(instruction: Instruction): void {
    if (!this.context) return;

    const target = instruction.operands[0]!;
    const input = this.context.ioStreams.sysin.shift() || '';

    this.setVariableValue(target.value as string, input);
  }

  private executeREAD(instruction: Instruction): void {
    if (!this.context) return;

    const fileName = instruction.operands[0]?.value as string;
    const handle = this.context.fileHandles.get(fileName);

    if (!handle || !handle.isOpen) {
      return;
    }

    if (handle.currentPosition >= handle.records.length) {
      handle.status = '10'; // End of file
      return;
    }

    const record = handle.records[handle.currentPosition];
    if (record) {
      // Move record data to record area
      const recordVar = instruction.operands[1]?.value as string;
      if (recordVar) {
        this.setVariableValue(recordVar, record.data);
      }
      handle.currentPosition++;
      handle.status = '00';
    }
  }

  private executeWRITE(instruction: Instruction): void {
    if (!this.context) return;

    const recordName = instruction.operands[0]?.value as string;
    const recordData = this.getVariableValue(recordName);

    // Find associated file
    for (const handle of this.context.fileHandles.values()) {
      if (handle.isOpen) {
        handle.records.push({
          data: String(recordData),
        });
        handle.status = '00';
        break;
      }
    }
  }

  private executeOPEN(instruction: Instruction): void {
    if (!this.context) return;

    const mode = instruction.operands[0]?.value as string;
    const fileName = instruction.operands[1]?.value as string;
    const handle = this.context.fileHandles.get(fileName);

    if (handle) {
      handle.isOpen = true;
      handle.status = '00';
      handle.currentPosition = 0;
      // Store mode for access validation
      if (mode === 'INPUT') {
        handle.accessMode = 'sequential';
      } else if (mode === 'OUTPUT') {
        handle.records = [];
      }
    }
  }

  private executeCLOSE(instruction: Instruction): void {
    if (!this.context) return;

    const fileName = instruction.operands[0]?.value as string;
    const handle = this.context.fileHandles.get(fileName);

    if (handle) {
      handle.isOpen = false;
      handle.status = '00';
    }
  }

  private executeSTRING(instruction: Instruction): void {
    if (!this.context) return;

    // STRING a DELIMITED BY SIZE b DELIMITED BY ',' INTO c
    const targetIndex = instruction.operands.findIndex(op => op.type === 'identifier' && op.qualifier === 'INTO');
    if (targetIndex < 0) return;

    const sources = instruction.operands.slice(0, targetIndex);
    const target = instruction.operands[targetIndex]!;

    let result = '';
    for (const source of sources) {
      if (source.qualifier === 'DELIMITED') continue;
      result += String(this.resolveOperandValue(source));
    }

    this.setVariableValue(target.value as string, result);
  }

  private executeUNSTRING(instruction: Instruction): void {
    if (!this.context) return;

    const source = this.resolveOperandValue(instruction.operands[0]!);
    const delimiter = instruction.operands[1]?.value || ' ';
    const targets = instruction.operands.slice(2);

    const parts = String(source).split(String(delimiter));

    for (let i = 0; i < targets.length && i < parts.length; i++) {
      this.setVariableValue(targets[i]!.value as string, parts[i] || '');
    }
  }

  private executeINSPECT(instruction: Instruction): void {
    if (!this.context) return;

    const targetName = instruction.operands[0]?.value as string;
    const value = String(this.getVariableValue(targetName));

    const action = instruction.operands[1]?.value as string;

    if (action === 'TALLYING') {
      const countVar = instruction.operands[2]?.value as string;
      const searchFor = String(instruction.operands[3]?.value || '');
      const count = (value.match(new RegExp(searchFor, 'g')) || []).length;
      this.setVariableValue(countVar, count);
    } else if (action === 'REPLACING') {
      const searchFor = String(instruction.operands[2]?.value || '');
      const replaceWith = String(instruction.operands[3]?.value || '');
      const newValue = value.replace(new RegExp(searchFor, 'g'), replaceWith);
      this.setVariableValue(targetName, newValue);
    }
  }

  private executeEVALUATE(instruction: Instruction): void {
    if (!this.context) return;

    // Simplified EVALUATE - just advance PC
    this.context.registers.programCounter++;
  }

  private executeSEARCH(instruction: Instruction): void {
    if (!this.context) return;

    // Simplified SEARCH - just advance PC
    this.context.registers.programCounter++;
  }

  private executeSET(instruction: Instruction): void {
    const target = instruction.operands[0]!;
    const source = instruction.operands[1]!;

    this.setVariableValue(target.value as string, this.resolveOperandValue(source));
  }

  private executeINITIALIZE(instruction: Instruction): void {
    const target = instruction.operands[0]!;
    const variable = this.findVariable(target.value as string);

    if (variable) {
      const defaultValue = this.getDefaultValue(variable.type);
      this.setVariableValue(target.value as string, defaultValue);
    }
  }

  // ============================================================================
  // HELPER METHODS
  // ============================================================================

  private resolveOperandValue(operand: { type: string; value: string | number; subscript?: (string | number)[] }): VariableValue {
    if (operand.type === 'literal') {
      return operand.value;
    }

    if (operand.type === 'figurative') {
      const fig = String(operand.value).toUpperCase();
      if (fig === 'SPACES' || fig === 'SPACE') return ' ';
      if (fig === 'ZEROS' || fig === 'ZEROES' || fig === 'ZERO') return 0;
      if (fig === 'LOW-VALUES' || fig === 'LOW-VALUE') return '\x00';
      if (fig === 'HIGH-VALUES' || fig === 'HIGH-VALUE') return '\xFF';
      return operand.value;
    }

    return this.getVariableValue(operand.value as string, operand.subscript);
  }

  private getVariableValue(name: string, subscript?: (string | number)[]): VariableValue {
    const variable = this.findVariable(name);
    if (!variable) return null;

    let value = variable.value;

    // Handle array access
    if (subscript && Array.isArray(value)) {
      for (const idx of subscript) {
        const numIdx = typeof idx === 'number' ? idx : parseInt(String(idx), 10);
        if (Array.isArray(value)) {
          value = value[numIdx - 1]; // COBOL arrays are 1-indexed
        }
      }
    }

    return value;
  }

  private setVariableValue(name: string, value: VariableValue): void {
    const variable = this.findVariable(name);
    if (!variable) return;

    // Type coercion
    switch (variable.type) {
      case 'numeric':
      case 'packed-decimal':
      case 'binary':
      case 'float':
        variable.value = typeof value === 'number' ? value : parseFloat(String(value)) || 0;
        break;
      case 'alphanumeric':
        variable.value = String(value);
        // Truncate or pad to size
        if (variable.picture) {
          const size = this.getPictureSize(variable.picture);
          variable.value = String(variable.value).padEnd(size).substring(0, size);
        }
        break;
      default:
        variable.value = value;
    }
  }

  private findVariable(name: string): MemoryVariable | null {
    if (!this.context) return null;

    for (const segment of [
      this.context.memory.workingStorage,
      this.context.memory.localStorage,
      this.context.memory.linkageSection,
      this.context.memory.fileSection,
    ]) {
      const variable = segment.variables.get(name);
      if (variable) return variable;

      // Search in children
      for (const v of segment.variables.values()) {
        const found = this.findInChildren(v, name);
        if (found) return found;
      }
    }

    return null;
  }

  private findInChildren(variable: MemoryVariable, name: string): MemoryVariable | null {
    if (!variable.children) return null;

    for (const child of variable.children) {
      if (child.name === name) return child;
      const found = this.findInChildren(child, name);
      if (found) return found;
    }

    return null;
  }

  private getPictureSize(picture: string): number {
    const expanded = picture.toUpperCase().replace(/\((\d+)\)/g, (_, n) => 'X'.repeat(parseInt(n)));
    return expanded.replace(/[^X9ABS]/g, '').length;
  }

  private getDefaultValue(type: VariableType): VariableValue {
    switch (type) {
      case 'numeric':
      case 'packed-decimal':
      case 'binary':
      case 'float':
        return 0;
      case 'alphanumeric':
        return ' ';
      default:
        return null;
    }
  }

  private evaluateExpression(expression: string): number {
    // Simple arithmetic expression evaluator
    // In production, would use proper parser
    const sanitized = expression.replace(/[^0-9+\-*/().]/g, '');
    try {
      // Use Function instead of eval for slightly better safety
      return new Function(`return ${sanitized}`)();
    } catch {
      return 0;
    }
  }

  private evaluateCondition(instruction: Instruction): boolean {
    const operands = instruction.operands;
    if (operands.length < 3) return false;

    const left = this.resolveOperandValue(operands[0]!);
    const operator = operands[1]?.value as string;
    const right = this.resolveOperandValue(operands[2]!);

    switch (operator?.toUpperCase()) {
      case '=':
      case 'EQUAL':
      case 'EQUALS':
        return left === right;
      case '>':
      case 'GREATER':
        return left! > right!;
      case '<':
      case 'LESS':
        return left! < right!;
      case '>=':
      case 'NOT LESS':
        return left! >= right!;
      case '<=':
      case 'NOT GREATER':
        return left! <= right!;
      case '<>':
      case 'NOT EQUAL':
        return left !== right;
      default:
        return false;
    }
  }

  private applyInputs(inputs: Map<string, VariableValue>): void {
    for (const [name, value] of inputs) {
      this.setVariableValue(name, value);
    }
  }

  private captureRelevantState(instruction: Instruction): Partial<MemorySnapshot> {
    const snapshot: Partial<MemorySnapshot> = {};

    // Capture variables referenced in instruction
    for (const operand of instruction.operands) {
      if (operand.type === 'identifier') {
        const variable = this.findVariable(operand.value as string);
        if (variable) {
          if (!snapshot.workingStorage) snapshot.workingStorage = [];
          snapshot.workingStorage.push({
            name: variable.name,
            value: variable.value,
            type: variable.type,
          });
        }
      }
    }

    return snapshot;
  }

  private collectOutput(): ExecutionOutput {
    if (!this.context) {
      return { display: [], files: [] };
    }

    const files = Array.from(this.context.fileHandles.entries()).map(([name, handle]) => ({
      name,
      records: handle.records.map(r => r.data),
      status: handle.status,
    }));

    return {
      display: this.context.ioStreams.display,
      files,
    };
  }

  private collectPerformanceMetrics(executionTimeMs: number): PerformanceMetrics {
    return {
      executionTimeMs,
      instructionsExecuted: this.context?.instructionCount || 0,
      memoryUsedBytes: this.calculateMemoryUsed(),
      ioOperations: this.countIOOperations(),
    };
  }

  private calculateMemoryUsed(): number {
    if (!this.context) return 0;

    let total = 0;
    for (const segment of [
      this.context.memory.workingStorage,
      this.context.memory.localStorage,
      this.context.memory.linkageSection,
      this.context.memory.fileSection,
    ]) {
      total += segment.totalSize;
    }
    return total;
  }

  private countIOOperations(): number {
    if (!this.context) return 0;

    return (
      this.context.ioStreams.display.length +
      this.context.ioStreams.accept.length +
      Array.from(this.context.fileHandles.values()).reduce(
        (sum, h) => sum + h.records.length,
        0
      )
    );
  }

  // ============================================================================
  // PUBLIC API
  // ============================================================================

  getMemory(): MemorySnapshot {
    if (!this.context) {
      return { workingStorage: [], localStorage: [], linkageSection: [] };
    }

    const snapshotSegment = (segment: MemorySegment): VariableSnapshot[] => {
      return Array.from(segment.variables.values()).map(v => ({
        name: v.name,
        value: v.value,
        type: v.type,
      }));
    };

    return {
      workingStorage: snapshotSegment(this.context.memory.workingStorage),
      localStorage: snapshotSegment(this.context.memory.localStorage),
      linkageSection: snapshotSegment(this.context.memory.linkageSection),
    };
  }

  setBreakpoint(location: string | number): void {
    this.breakpoints.add(location);
  }

  clearBreakpoint(location: string | number): void {
    this.breakpoints.delete(location);
  }

  reset(): void {
    if (this.program) {
      this.context = this.initializeContext(this.program);
    }
    if (this.config.enableTrace) {
      this.trace = { steps: [], branchHistory: [], callHistory: [] };
    }
    this.isRunning = false;
    this.isPaused = false;
  }

  validate(legacyResult: ExecutionResult, modernResult: unknown): ValidationResult {
    const differences: BehaviorDifference[] = [];
    const modernObj = modernResult as Record<string, unknown>;

    // Compare outputs
    if (modernObj && typeof modernObj === 'object') {
      // Compare display output
      if ('display' in modernObj && Array.isArray(modernObj.display)) {
        for (let i = 0; i < Math.max(legacyResult.output.display.length, modernObj.display.length); i++) {
          const legacyLine = legacyResult.output.display[i] || '';
          const modernLine = String(modernObj.display[i] || '');

          if (legacyLine !== modernLine) {
            differences.push({
              type: 'output-mismatch',
              description: `Display line ${i + 1} differs`,
              legacyValue: legacyLine,
              modernValue: modernLine,
              path: `output.display[${i}]`,
              severity: this.classifyDifferenceSeverity(legacyLine, modernLine),
            });
          }
        }
      }

      // Compare memory snapshots
      if ('memory' in modernObj && typeof modernObj.memory === 'object') {
        const modernMemory = modernObj.memory as Record<string, VariableValue>;
        for (const varSnapshot of legacyResult.memory.workingStorage) {
          const modernValue = modernMemory[varSnapshot.name];
          if (modernValue !== undefined && modernValue !== varSnapshot.value) {
            differences.push({
              type: this.classifyDifferenceType(varSnapshot.value, modernValue),
              description: `Variable ${varSnapshot.name} differs`,
              legacyValue: varSnapshot.value,
              modernValue: modernValue,
              path: `memory.${varSnapshot.name}`,
              severity: 'major',
            });
          }
        }
      }
    }

    // Calculate coverage (simplified)
    const coverage: CoverageMetrics = {
      statementCoverage: this.trace ? this.trace.steps.length / (this.program?.compiledInstructions.length || 1) : 0,
      branchCoverage: this.trace ? this.trace.branchHistory.length / 10 : 0, // Simplified
      pathCoverage: 0.5, // Placeholder
      dataFlowCoverage: 0.5, // Placeholder
      coveredParagraphs: this.trace
        ? [...new Set(this.trace.steps.filter(s => s.instruction.label).map(s => s.instruction.label!))]
        : [],
      uncoveredParagraphs: [],
    };

    return {
      isEquivalent: differences.length === 0,
      differences,
      coverage,
      confidence: differences.length === 0 ? 1.0 : Math.max(0, 1 - differences.length * 0.1),
    };
  }

  private classifyDifferenceType(legacy: VariableValue, modern: VariableValue): BehaviorDifference['type'] {
    if (typeof legacy === 'number' && typeof modern === 'number') {
      if (Math.abs(legacy - modern) < 0.0001) {
        return 'precision-loss';
      }
      return 'rounding-difference';
    }

    if (typeof legacy === 'string' && typeof modern === 'string') {
      if (legacy.trim() === modern.trim()) {
        return 'truncation';
      }
      return 'character-encoding';
    }

    return 'output-mismatch';
  }

  private classifyDifferenceSeverity(legacy: VariableValue, modern: VariableValue): BehaviorDifference['severity'] {
    // Numbers with small differences are minor
    if (typeof legacy === 'number' && typeof modern === 'number') {
      const diff = Math.abs(legacy - modern);
      if (diff < 0.01) return 'cosmetic';
      if (diff < 1) return 'minor';
      return 'major';
    }

    // Whitespace-only differences are cosmetic
    if (typeof legacy === 'string' && typeof modern === 'string') {
      if (legacy.trim() === modern.trim()) return 'cosmetic';
    }

    return 'major';
  }

  // ============================================================================
  // INPUT SIMULATION
  // ============================================================================

  provideInput(input: string): void {
    if (this.context) {
      this.context.ioStreams.sysin.push(input);
    }
  }

  loadFileData(fileName: string, records: string[]): void {
    if (!this.context) return;

    const handle = this.context.fileHandles.get(fileName);
    if (handle) {
      handle.records = records.map(data => ({ data }));
    }
  }
}
