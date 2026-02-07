/**
 * Legacy Runtime Emulator Package
 * Sandboxed execution environment for legacy code validation
 */

// Main export
export { LegacyRuntimeEmulator } from './emulator.js';

// Type exports
export type {
  // Memory types
  MemoryLayout,
  MemorySegment,
  MemoryVariable,
  MemorySnapshot,
  VariableSnapshot,
  VariableType,
  VariableValue,
  VariableDefinition,
  OccursClause,
  // Execution types
  ExecutionContext,
  ExecutionResult,
  ExecutionOutput,
  ExecutionError,
  ExecutionTrace,
  ExecutionFlags,
  TraceStep,
  BranchRecord,
  CallRecord,
  CallFrame,
  PerformFrame,
  VaryingClause,
  RegisterSet,
  PerformanceMetrics,
  // I/O types
  IOStreams,
  FileHandle,
  FileRecord,
  FileOutput,
  FileStatus,
  FileOrganization,
  FileAccessMode,
  // Instruction types
  Instruction,
  InstructionType,
  Operand,
  OperandType,
  // Program types
  LegacyProgram,
  LegacyLanguage,
  DataDivision,
  ProcedureDivision,
  FileDefinition,
  Section,
  Paragraph,
  Copybook,
  ProgramMetadata,
  // Validation types
  ValidationResult,
  BehaviorDifference,
  DifferenceType,
  CoverageMetrics,
  // Configuration
  EmulatorConfig,
  TraceLevel,
  CharacterSet,
  DateFormat,
  // Service interface
  ILegacyRuntimeEmulator,
} from './types.js';
