/**
 * AI-Powered Test Oracle Module
 * 
 * Exports all test oracle capabilities including:
 * - Behavioral learning from historical executions
 * - Output prediction and validation
 * - Intelligent test case synthesis
 * - Continuous validation pipeline with drift detection
 */

// Types
export * from './types.js';

// Core components
export { BehavioralLearningEngine } from './learning-engine.js';
export { TestOracle } from './test-oracle.js';
export { TestCaseSynthesizer } from './synthesizer.js';
export { ContinuousValidationPipeline } from './pipeline.js';
export type { ValidationSummaryReport } from './pipeline.js';
