/**
 * @migrationpilot/cost-estimator
 *
 * Self-service migration cost estimation engine for instant complexity
 * scoring and cost estimates from legacy code samples.
 */

// Main service
export { CostEstimatorService } from './cost-estimator.js';

// Types
export * from './types/index.js';

// Analyzers
export { LanguageDetector, CodeAnalyzer } from './analyzers/index.js';
