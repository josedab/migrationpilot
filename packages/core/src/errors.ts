/**
 * MigrationPilot Error Types Hierarchy
 * 
 * Provides a structured error hierarchy for consistent error handling across the platform.
 * All custom errors extend from MigrationError which includes error codes, context, and recoverability info.
 */

/**
 * Error codes for categorizing migration errors
 */
export enum MigrationErrorCode {
  // Analysis errors (1xxx)
  ANALYSIS_FAILED = 'E1001',
  PARSE_ERROR = 'E1002',
  UNSUPPORTED_LANGUAGE = 'E1003',
  INVALID_SOURCE_CODE = 'E1004',
  
  // Architecture errors (2xxx)
  ARCHITECTURE_DESIGN_FAILED = 'E2001',
  INVALID_PATTERN = 'E2002',
  INCOMPATIBLE_TARGET = 'E2003',
  
  // Generation errors (3xxx)
  CODE_GENERATION_FAILED = 'E3001',
  TEMPLATE_ERROR = 'E3002',
  INVALID_OUTPUT = 'E3003',
  
  // Validation errors (4xxx)
  VALIDATION_FAILED = 'E4001',
  TEST_GENERATION_FAILED = 'E4002',
  EQUIVALENCE_MISMATCH = 'E4003',
  CONFIDENCE_BELOW_THRESHOLD = 'E4004',
  
  // AI/Copilot errors (5xxx)
  AI_SERVICE_ERROR = 'E5001',
  AI_TIMEOUT = 'E5002',
  AI_RATE_LIMIT = 'E5003',
  AI_RESPONSE_INVALID = 'E5004',
  
  // Configuration errors (6xxx)
  INVALID_CONFIG = 'E6001',
  MISSING_REQUIRED_CONFIG = 'E6002',
  
  // Resource errors (7xxx)
  PROJECT_NOT_FOUND = 'E7001',
  FILE_NOT_FOUND = 'E7002',
  RESOURCE_ACCESS_DENIED = 'E7003',
  
  // General errors (9xxx)
  UNKNOWN_ERROR = 'E9001',
  INTERNAL_ERROR = 'E9002',
  NOT_IMPLEMENTED = 'E9003',
}

/**
 * Context information for errors
 */
export interface ErrorContext {
  projectId?: string;
  sessionId?: string;
  userId?: string;
  agentType?: string;
  phase?: string;
  sourceFile?: string;
  lineNumber?: number;
  additionalInfo?: Record<string, unknown>;
}

/**
 * Base error class for all MigrationPilot errors
 */
export class MigrationError extends Error {
  readonly code: MigrationErrorCode;
  readonly context: ErrorContext;
  readonly recoverable: boolean;
  readonly timestamp: Date;
  readonly originalError?: Error;

  constructor(
    message: string,
    code: MigrationErrorCode = MigrationErrorCode.UNKNOWN_ERROR,
    context: ErrorContext = {},
    recoverable: boolean = false,
    originalError?: Error
  ) {
    super(message);
    this.name = 'MigrationError';
    this.code = code;
    this.context = context;
    this.recoverable = recoverable;
    this.timestamp = new Date();
    this.originalError = originalError;

    // Maintain proper stack trace
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, MigrationError);
    }
  }

  /**
   * Convert to a plain object for logging/serialization
   */
  toJSON(): Record<string, unknown> {
    return {
      name: this.name,
      message: this.message,
      code: this.code,
      context: this.context,
      recoverable: this.recoverable,
      timestamp: this.timestamp.toISOString(),
      stack: this.stack,
      originalError: this.originalError?.message,
    };
  }
}

/**
 * Error thrown during code analysis phase
 */
export class AnalysisError extends MigrationError {
  constructor(
    message: string,
    code: MigrationErrorCode = MigrationErrorCode.ANALYSIS_FAILED,
    context: ErrorContext = {},
    originalError?: Error
  ) {
    super(message, code, { ...context, phase: 'analysis' }, true, originalError);
    this.name = 'AnalysisError';
  }
}

/**
 * Error thrown during parsing
 */
export class ParseError extends AnalysisError {
  readonly sourceLocation?: { line: number; column: number };

  constructor(
    message: string,
    sourceFile: string,
    sourceLocation?: { line: number; column: number },
    context: ErrorContext = {},
    originalError?: Error
  ) {
    super(
      message,
      MigrationErrorCode.PARSE_ERROR,
      { ...context, sourceFile, lineNumber: sourceLocation?.line },
      originalError
    );
    this.name = 'ParseError';
    this.sourceLocation = sourceLocation;
  }
}

/**
 * Error thrown during architecture design phase
 */
export class ArchitectureError extends MigrationError {
  constructor(
    message: string,
    code: MigrationErrorCode = MigrationErrorCode.ARCHITECTURE_DESIGN_FAILED,
    context: ErrorContext = {},
    originalError?: Error
  ) {
    super(message, code, { ...context, phase: 'architecture' }, true, originalError);
    this.name = 'ArchitectureError';
  }
}

/**
 * Error thrown during code generation phase
 */
export class GenerationError extends MigrationError {
  constructor(
    message: string,
    code: MigrationErrorCode = MigrationErrorCode.CODE_GENERATION_FAILED,
    context: ErrorContext = {},
    originalError?: Error
  ) {
    super(message, code, { ...context, phase: 'generation' }, true, originalError);
    this.name = 'GenerationError';
  }
}

/**
 * Error thrown during validation phase
 */
export class ValidationError extends MigrationError {
  readonly testCaseId?: string;
  readonly actualValue?: unknown;
  readonly expectedValue?: unknown;

  constructor(
    message: string,
    code: MigrationErrorCode = MigrationErrorCode.VALIDATION_FAILED,
    context: ErrorContext = {},
    details?: { testCaseId?: string; actualValue?: unknown; expectedValue?: unknown },
    originalError?: Error
  ) {
    super(message, code, { ...context, phase: 'validation' }, true, originalError);
    this.name = 'ValidationError';
    this.testCaseId = details?.testCaseId;
    this.actualValue = details?.actualValue;
    this.expectedValue = details?.expectedValue;
  }
}

/**
 * Error thrown when interacting with AI services
 */
export class AIServiceError extends MigrationError {
  readonly statusCode?: number;
  readonly retryAfter?: number;

  constructor(
    message: string,
    code: MigrationErrorCode = MigrationErrorCode.AI_SERVICE_ERROR,
    context: ErrorContext = {},
    details?: { statusCode?: number; retryAfter?: number },
    originalError?: Error
  ) {
    // AI errors are typically recoverable with retry
    const recoverable = code !== MigrationErrorCode.AI_RATE_LIMIT || (details?.retryAfter ?? 0) > 0;
    super(message, code, context, recoverable, originalError);
    this.name = 'AIServiceError';
    this.statusCode = details?.statusCode;
    this.retryAfter = details?.retryAfter;
  }
}

/**
 * Error thrown for configuration issues
 */
export class ConfigurationError extends MigrationError {
  readonly configKey?: string;

  constructor(
    message: string,
    configKey?: string,
    context: ErrorContext = {},
    originalError?: Error
  ) {
    super(
      message,
      configKey ? MigrationErrorCode.MISSING_REQUIRED_CONFIG : MigrationErrorCode.INVALID_CONFIG,
      context,
      false, // Configuration errors are not recoverable without user intervention
      originalError
    );
    this.name = 'ConfigurationError';
    this.configKey = configKey;
  }
}

/**
 * Error thrown when a resource is not found
 */
export class ResourceNotFoundError extends MigrationError {
  readonly resourceType: string;
  readonly resourceId: string;

  constructor(
    resourceType: string,
    resourceId: string,
    context: ErrorContext = {},
    originalError?: Error
  ) {
    super(
      `${resourceType} not found: ${resourceId}`,
      resourceType === 'Project' ? MigrationErrorCode.PROJECT_NOT_FOUND : MigrationErrorCode.FILE_NOT_FOUND,
      context,
      false,
      originalError
    );
    this.name = 'ResourceNotFoundError';
    this.resourceType = resourceType;
    this.resourceId = resourceId;
  }
}

/**
 * Helper to wrap unknown errors as MigrationError
 */
export function wrapError(
  error: unknown,
  context: ErrorContext = {},
  defaultMessage: string = 'An unexpected error occurred'
): MigrationError {
  if (error instanceof MigrationError) {
    // Add context if not present
    return new MigrationError(
      error.message,
      error.code,
      { ...context, ...error.context },
      error.recoverable,
      error.originalError || error
    );
  }

  if (error instanceof Error) {
    return new MigrationError(
      error.message,
      MigrationErrorCode.UNKNOWN_ERROR,
      context,
      false,
      error
    );
  }

  return new MigrationError(
    typeof error === 'string' ? error : defaultMessage,
    MigrationErrorCode.UNKNOWN_ERROR,
    context,
    false
  );
}

/**
 * Type guard to check if an error is a MigrationError
 */
export function isMigrationError(error: unknown): error is MigrationError {
  return error instanceof MigrationError;
}

/**
 * Type guard to check if an error is recoverable
 */
export function isRecoverableError(error: unknown): boolean {
  if (error instanceof MigrationError) {
    return error.recoverable;
  }
  return false;
}
