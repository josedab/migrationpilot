/**
 * Legacy Runtime Emulator Types
 * Types for sandboxed execution and validation of legacy code behavior
 */

// ============================================================================
// MEMORY TYPES
// ============================================================================

export interface MemoryLayout {
  workingStorage: MemorySegment;
  localStorage: MemorySegment;
  linkageSection: MemorySegment;
  fileSection: MemorySegment;
}

export interface MemorySegment {
  name: string;
  variables: Map<string, MemoryVariable>;
  totalSize: number;
  baseAddress: number;
}

export interface MemoryVariable {
  name: string;
  type: VariableType;
  value: VariableValue;
  size: number;
  offset: number;
  picture?: string;
  level?: number;
  occurs?: OccursClause;
  redefines?: string;
  isGroup?: boolean;
  children?: MemoryVariable[];
}

export type VariableType =
  | 'alphanumeric'
  | 'numeric'
  | 'packed-decimal'
  | 'binary'
  | 'float'
  | 'group'
  | 'index'
  | 'pointer';

export type VariableValue = string | number | boolean | null | VariableValue[];

export interface OccursClause {
  times: number;
  dependingOn?: string;
  indexedBy?: string[];
  key?: { name: string; ascending: boolean }[];
}

// ============================================================================
// EXECUTION TYPES
// ============================================================================

export interface ExecutionContext {
  programId: string;
  memory: MemoryLayout;
  callStack: CallFrame[];
  performStack: PerformFrame[];
  fileHandles: Map<string, FileHandle>;
  registers: RegisterSet;
  flags: ExecutionFlags;
  ioStreams: IOStreams;
  startTime: Date;
  instructionCount: number;
}

export interface CallFrame {
  programId: string;
  returnAddress: number;
  linkageData: Map<string, VariableValue>;
  savedRegisters: RegisterSet;
}

export interface PerformFrame {
  paragraphName: string;
  returnAddress: number;
  timesRemaining?: number;
  untilCondition?: string;
  varyingIndex?: VaryingClause;
}

export interface VaryingClause {
  indexName: string;
  from: number;
  by: number;
  until: string;
}

export interface RegisterSet {
  programCounter: number;
  returnCode: number;
  addressRegisters: number[];
  generalRegisters: number[];
}

export interface ExecutionFlags {
  conditionCode: number;
  overflow: boolean;
  underflow: boolean;
  divideByZero: boolean;
  programCheck: boolean;
}

// ============================================================================
// I/O TYPES
// ============================================================================

export interface IOStreams {
  sysout: string[];
  sysin: string[];
  display: string[];
  accept: string[];
}

export interface FileHandle {
  name: string;
  ddname: string;
  status: FileStatus;
  organization: FileOrganization;
  accessMode: FileAccessMode;
  recordLength: number;
  blockSize: number;
  currentPosition: number;
  records: FileRecord[];
  isOpen: boolean;
}

export type FileStatus =
  | '00' // Successful
  | '02' // Duplicate key
  | '10' // End of file
  | '21' // Sequence error
  | '22' // Duplicate key on write
  | '23' // Record not found
  | '30' // Permanent error
  | '35' // File not found
  | '39' // Conflict in file attributes
  | '41' // File already open
  | '42' // File not open
  | '43' // No previous read
  | '44' // Record overflow
  | '46' // No valid next record
  | '47' // Input denied
  | '48' // Output denied
  | '49' // Rewrite/delete denied;

export type FileOrganization = 'sequential' | 'indexed' | 'relative' | 'line-sequential';
export type FileAccessMode = 'sequential' | 'random' | 'dynamic';

export interface FileRecord {
  key?: string;
  relativeKey?: number;
  data: string;
  deleted?: boolean;
}

// ============================================================================
// INSTRUCTION TYPES
// ============================================================================

export interface Instruction {
  type: InstructionType;
  opcode: string;
  operands: Operand[];
  label?: string;
  lineNumber: number;
  sourceText: string;
}

export type InstructionType =
  | 'arithmetic'
  | 'data-movement'
  | 'control-flow'
  | 'io'
  | 'string'
  | 'table'
  | 'program-control'
  | 'condition'
  | 'file';

export interface Operand {
  type: OperandType;
  value: string | number;
  qualifier?: string;
  subscript?: (string | number)[];
  refMod?: { start: number; length: number };
}

export type OperandType =
  | 'identifier'
  | 'literal'
  | 'figurative'
  | 'special-register'
  | 'address';

// ============================================================================
// PROGRAM TYPES
// ============================================================================

export interface LegacyProgram {
  id: string;
  name: string;
  language: LegacyLanguage;
  source: string;
  compiledInstructions: Instruction[];
  dataDivision: DataDivision;
  procedureDivision: ProcedureDivision;
  copybooks: Copybook[];
  metadata: ProgramMetadata;
}

export type LegacyLanguage = 'cobol' | 'fortran' | 'pli' | 'rpg' | 'natural' | 'assembly';

export interface DataDivision {
  fileSection: FileDefinition[];
  workingStorage: VariableDefinition[];
  localStorage: VariableDefinition[];
  linkageSection: VariableDefinition[];
}

export interface FileDefinition {
  fdName: string;
  fileName: string;
  recordDescriptions: VariableDefinition[];
  blockContains?: number;
  recordContains?: { min: number; max: number };
  labelRecords?: 'standard' | 'omitted';
}

export interface VariableDefinition {
  level: number;
  name: string;
  picture?: string;
  usage?: string;
  value?: string;
  occurs?: OccursClause;
  redefines?: string;
  children?: VariableDefinition[];
}

export interface ProcedureDivision {
  sections: Section[];
  paragraphs: Paragraph[];
  usingParameters?: string[];
  returningParameter?: string;
}

export interface Section {
  name: string;
  paragraphs: Paragraph[];
}

export interface Paragraph {
  name: string;
  instructions: Instruction[];
  startLine: number;
  endLine: number;
}

export interface Copybook {
  name: string;
  content: string;
  replacements?: { from: string; to: string }[];
}

export interface ProgramMetadata {
  author?: string;
  dateWritten?: string;
  dateCompiled?: string;
  security?: string;
  remarks?: string[];
}

// ============================================================================
// EXECUTION RESULT TYPES
// ============================================================================

export interface ExecutionResult {
  success: boolean;
  returnCode: number;
  output: ExecutionOutput;
  memory: MemorySnapshot;
  trace?: ExecutionTrace;
  errors: ExecutionError[];
  performance: PerformanceMetrics;
}

export interface ExecutionOutput {
  display: string[];
  files: FileOutput[];
  returnData?: Map<string, VariableValue>;
}

export interface FileOutput {
  name: string;
  records: string[];
  status: FileStatus;
}

export interface MemorySnapshot {
  workingStorage: VariableSnapshot[];
  localStorage: VariableSnapshot[];
  linkageSection: VariableSnapshot[];
}

export interface VariableSnapshot {
  name: string;
  value: VariableValue;
  type: VariableType;
  hexValue?: string;
}

export interface ExecutionTrace {
  steps: TraceStep[];
  branchHistory: BranchRecord[];
  callHistory: CallRecord[];
}

export interface TraceStep {
  stepNumber: number;
  instruction: Instruction;
  beforeState: Partial<MemorySnapshot>;
  afterState: Partial<MemorySnapshot>;
  timestamp: number;
}

export interface BranchRecord {
  stepNumber: number;
  condition: string;
  taken: boolean;
  targetLabel: string;
}

export interface CallRecord {
  stepNumber: number;
  programId: string;
  type: 'call' | 'return';
  returnCode?: number;
}

export interface ExecutionError {
  code: string;
  message: string;
  severity: 'warning' | 'error' | 'abend';
  lineNumber?: number;
  instruction?: Instruction;
  memoryDump?: VariableSnapshot[];
}

export interface PerformanceMetrics {
  executionTimeMs: number;
  instructionsExecuted: number;
  memoryUsedBytes: number;
  ioOperations: number;
  cpuCycles?: number;
}

// ============================================================================
// EMULATOR CONFIGURATION
// ============================================================================

export interface EmulatorConfig {
  maxInstructions: number;
  maxMemoryBytes: number;
  maxExecutionTimeMs: number;
  enableTrace: boolean;
  traceLevel: TraceLevel;
  strictMode: boolean;
  charset: CharacterSet;
  decimalPrecision: number;
  dateFormat: DateFormat;
}

export type TraceLevel = 'none' | 'minimal' | 'standard' | 'verbose' | 'debug';
export type CharacterSet = 'ebcdic' | 'ascii' | 'utf8';
export type DateFormat = 'yyyymmdd' | 'mmddyyyy' | 'ddmmyyyy' | 'julian';

// ============================================================================
// VALIDATION TYPES
// ============================================================================

export interface ValidationResult {
  isEquivalent: boolean;
  differences: BehaviorDifference[];
  coverage: CoverageMetrics;
  confidence: number;
}

export interface BehaviorDifference {
  type: DifferenceType;
  description: string;
  legacyValue: VariableValue;
  modernValue: VariableValue;
  path: string;
  severity: 'critical' | 'major' | 'minor' | 'cosmetic';
}

export type DifferenceType =
  | 'output-mismatch'
  | 'precision-loss'
  | 'rounding-difference'
  | 'truncation'
  | 'character-encoding'
  | 'date-format'
  | 'null-handling'
  | 'overflow-behavior';

export interface CoverageMetrics {
  statementCoverage: number;
  branchCoverage: number;
  pathCoverage: number;
  dataFlowCoverage: number;
  coveredParagraphs: string[];
  uncoveredParagraphs: string[];
}

// ============================================================================
// SERVICE INTERFACE
// ============================================================================

export interface ILegacyRuntimeEmulator {
  loadProgram(program: LegacyProgram): Promise<void>;
  execute(inputs: Map<string, VariableValue>): Promise<ExecutionResult>;
  executeStep(): Promise<TraceStep>;
  getMemory(): MemorySnapshot;
  setBreakpoint(location: string | number): void;
  clearBreakpoint(location: string | number): void;
  reset(): void;
  validate(legacyResult: ExecutionResult, modernResult: unknown): ValidationResult;
}
