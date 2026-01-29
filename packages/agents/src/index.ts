/**
 * MigrationPilot Agents Package
 * 
 * AI agents for legacy code modernization using GitHub Copilot SDK
 */

// Common infrastructure
export * from './common/index.js';

// Agents
export { ArcheologistAgent, type AnalysisResult } from './archeologist/index.js';
export { ArchitectAgent, type ArchitectureDesign, type ServiceDefinition } from './architect/index.js';
export { BuilderAgent, type GenerationResult, type CodeGenerationOptions } from './builder/index.js';
export { ValidatorAgent, type ValidationResult, type TestGenerationConfig } from './validator/index.js';
export { ExplainerAgent, type ExplanationResult, type KeyOperation, type DataFlowStep, type QASession } from './explainer/index.js';

// Agent orchestrator
export { MigrationOrchestrator, type MigrationConfig, type MigrationResult } from './orchestrator.js';
