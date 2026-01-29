/**
 * MigrationPilot Constants
 */

import type { SourceLanguage, TargetLanguage } from './types/index.js';

// ============================================================================
// LANGUAGE CONFIGURATIONS
// ============================================================================

export const SOURCE_LANGUAGES: Record<SourceLanguage, {
  name: string;
  extensions: string[];
  dialects: string[];
}> = {
  cobol: {
    name: 'COBOL',
    extensions: ['.cbl', '.cob', '.cpy', '.ccp'],
    dialects: ['COBOL-85', 'COBOL-2002', 'IBM Enterprise COBOL', 'Micro Focus COBOL', 'GnuCOBOL'],
  },
  fortran: {
    name: 'Fortran',
    extensions: ['.f', '.f77', '.f90', '.f95', '.for'],
    dialects: ['Fortran 77', 'Fortran 90', 'Fortran 95'],
  },
  vb6: {
    name: 'Visual Basic 6',
    extensions: ['.bas', '.cls', '.frm', '.vbp'],
    dialects: ['VB6'],
  },
  vba: {
    name: 'VBA',
    extensions: ['.bas', '.cls'],
    dialects: ['Excel VBA', 'Access VBA', 'Word VBA'],
  },
  'java-legacy': {
    name: 'Legacy Java',
    extensions: ['.java'],
    dialects: ['J2EE', 'EJB 2.x', 'Struts 1.x'],
  },
};

export const TARGET_LANGUAGES: Record<TargetLanguage, {
  name: string;
  extension: string;
  frameworks: string[];
}> = {
  java: {
    name: 'Java',
    extension: '.java',
    frameworks: ['Spring Boot', 'Quarkus', 'Micronaut'],
  },
  python: {
    name: 'Python',
    extension: '.py',
    frameworks: ['FastAPI', 'Django', 'Flask'],
  },
  typescript: {
    name: 'TypeScript',
    extension: '.ts',
    frameworks: ['Node.js', 'NestJS', 'Express'],
  },
  go: {
    name: 'Go',
    extension: '.go',
    frameworks: ['Standard Library', 'Gin', 'Echo'],
  },
  csharp: {
    name: 'C#',
    extension: '.cs',
    frameworks: ['.NET Core', 'ASP.NET Core'],
  },
};

// ============================================================================
// DEFAULT CONFIGURATIONS
// ============================================================================

export const DEFAULT_CONFIDENCE_THRESHOLD = 0.85;
export const DEFAULT_PAGE_SIZE = 20;
export const MAX_PAGE_SIZE = 100;

export const DEFAULT_PROJECT_SETTINGS = {
  enableStranglerFig: false,
  generateTests: true,
  generateDocumentation: true,
  humanReviewRequired: true,
  confidenceThreshold: DEFAULT_CONFIDENCE_THRESHOLD,
};

// ============================================================================
// AGENT CONFIGURATION CONSTANTS
// ============================================================================

/** Default model for AI agents */
export const AGENT_DEFAULT_MODEL = 'gpt-4';

/** AI model temperatures by task type */
export const AGENT_TEMPERATURE = {
  /** Low temperature for analytical/precise work */
  ANALYTICAL: 0.1,
  /** Moderate temperature for code generation */
  CODE_GENERATION: 0.2,
  /** Slightly creative for architecture design */
  CREATIVE: 0.3,
} as const;

/** Token limits by agent type */
export const AGENT_MAX_TOKENS = {
  /** Default token limit for most agents */
  DEFAULT: 8000,
  /** Higher limit for code generation */
  CODE_GENERATION: 16000,
  /** Lower limit for explanations */
  EXPLANATION: 4000,
} as const;

// ============================================================================
// TIMEOUT AND RETRY CONSTANTS
// ============================================================================

/** Default timeout for API calls in milliseconds */
export const DEFAULT_API_TIMEOUT_MS = 60000;

/** Default timeout for pipeline stages in milliseconds */
export const PIPELINE_STAGE_TIMEOUT_MS = 300000;

/** Default number of retry attempts */
export const DEFAULT_MAX_RETRIES = 3;

/** Initial delay for exponential backoff in milliseconds */
export const RETRY_INITIAL_DELAY_MS = 1000;

/** Maximum delay for exponential backoff in milliseconds */
export const RETRY_MAX_DELAY_MS = 30000;

/** Backoff multiplier for retry delays */
export const RETRY_BACKOFF_MULTIPLIER = 2;

// ============================================================================
// VALIDATION THRESHOLDS
// ============================================================================

/** Minimum equivalence score for production readiness */
export const PRODUCTION_EQUIVALENCE_THRESHOLD = 0.99;

/** Minimum business rules coverage for production readiness */
export const PRODUCTION_RULES_COVERAGE_THRESHOLD = 0.95;

/** Minimum code paths coverage for production readiness */
export const PRODUCTION_PATHS_COVERAGE_THRESHOLD = 0.85;

/** Default tolerance for numeric comparisons */
export const DEFAULT_NUMERIC_TOLERANCE = 0.01;

// ============================================================================
// COPILOT SDK CONSTANTS
// ============================================================================

/** Maximum iterations for tool-calling loops */
export const COPILOT_MAX_TOOL_ITERATIONS = 10;

// ============================================================================
// BUSINESS RULE PATTERNS
// ============================================================================

export const COBOL_RULE_PATTERNS = {
  COMPUTE: /COMPUTE\s+[\w-]+\s*=\s*[^.]+\./gi,
  EVALUATE: /EVALUATE\s+[\w-]+[\s\S]*?END-EVALUATE/gi,
  IF_THEN: /IF\s+[\w-]+[\s\S]*?END-IF/gi,
  PERFORM: /PERFORM\s+[\w-]+/gi,
  CALL: /CALL\s+['"][\w-]+['"]/gi,
};

export const FORTRAN_RULE_PATTERNS = {
  FUNCTION: /FUNCTION\s+\w+\s*\([^)]*\)/gi,
  SUBROUTINE: /SUBROUTINE\s+\w+\s*\([^)]*\)/gi,
  DO_LOOP: /DO\s+\d*\s*\w+\s*=/gi,
  IF_THEN: /IF\s*\([^)]+\)\s*THEN/gi,
};

// ============================================================================
// API ENDPOINTS
// ============================================================================

export const API_ENDPOINTS = {
  PROJECTS: '/api/projects',
  FILES: '/api/files',
  RULES: '/api/rules',
  TESTS: '/api/tests',
  AGENTS: '/api/agents',
  USERS: '/api/users',
  ORGANIZATIONS: '/api/organizations',
};

// ============================================================================
// ERROR CODES
// ============================================================================

export const ERROR_CODES = {
  // Authentication
  UNAUTHORIZED: 'UNAUTHORIZED',
  FORBIDDEN: 'FORBIDDEN',
  TOKEN_EXPIRED: 'TOKEN_EXPIRED',
  
  // Validation
  VALIDATION_ERROR: 'VALIDATION_ERROR',
  INVALID_INPUT: 'INVALID_INPUT',
  
  // Resources
  NOT_FOUND: 'NOT_FOUND',
  ALREADY_EXISTS: 'ALREADY_EXISTS',
  CONFLICT: 'CONFLICT',
  
  // Processing
  PARSE_ERROR: 'PARSE_ERROR',
  ANALYSIS_FAILED: 'ANALYSIS_FAILED',
  GENERATION_FAILED: 'GENERATION_FAILED',
  VALIDATION_FAILED: 'VALIDATION_FAILED',
  
  // External
  EXTERNAL_SERVICE_ERROR: 'EXTERNAL_SERVICE_ERROR',
  RATE_LIMITED: 'RATE_LIMITED',
  
  // Internal
  INTERNAL_ERROR: 'INTERNAL_ERROR',
};
