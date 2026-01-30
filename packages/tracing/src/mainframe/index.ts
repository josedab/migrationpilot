/**
 * Mainframe Tracing Module
 * 
 * Exports all mainframe-specific tracing capabilities including:
 * - Live execution trace capture from z/OS systems
 * - Trace normalization and behavioral analysis
 * - Business rule inference engine
 * - SME validation workflow
 */

// Types
export * from './types.js';

// Trace capture
export { MainframeTraceCapture } from './capture.js';
export type { 
  CaptureSession, 
  PendingTrace, 
  RawTraceData,
  CICSTraceInput,
  DB2TraceInput,
  VSAMTraceInput,
} from './capture.js';

// Trace normalization
export { TraceNormalizer } from './normalizer.js';

// Behavioral inference
export { BehavioralInferenceEngine } from './inference.js';
export type {
  BehavioralInferenceConfig,
  RuleEvidence,
  InferredBusinessRule,
  SuggestedTestCase,
  RuleValidationResult,
} from './inference.js';

// SME validation workflow
export { SMEValidationWorkflow } from './validation-workflow.js';
export type {
  SMEValidationSession,
  ValidationSessionSettings,
  RuleValidationItem,
  SMEReviewResult,
  RuleModification,
  EvidenceView,
  SuggestedAction,
  ExportedValidationResult,
  ValidationProgressReport,
} from './validation-workflow.js';
