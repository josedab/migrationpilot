/**
 * Federated Learning Package
 * Privacy-preserving collaborative learning for business rule extraction
 */

// Main exports
export { FederatedCoordinator } from './coordinator.js';
export { FederatedLearningClient, type TrainingData } from './client.js';

// Type exports
export type {
  // Model types
  ModelWeights,
  LayerWeights,
  ModelUpdate,
  LocalMetrics,
  // Client types
  FederatedClient,
  ClientCapabilities,
  ClientStatus,
  ClientRegistration,
  // Round types
  TrainingRound,
  RoundConfig,
  RoundStatus,
  RoundParticipant,
  ParticipantStatus,
  AggregationStrategy,
  DifferentialPrivacyConfig,
  GlobalMetrics,
  // Task types
  RuleLearningTask,
  ModelInfo,
  RuleType,
  TaskStatus,
  // Contribution types
  Contribution,
  ContributionType,
  ContributionSummary,
  // Privacy types
  PrivacyBudget,
  SecureAggregation,
  // Service interfaces
  IFederatedCoordinator,
  IFederatedClient,
} from './types.js';
