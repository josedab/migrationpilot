/**
 * Federated Learning Types
 * Types for privacy-preserving collaborative rule learning
 */

// ============================================================================
// MODEL TYPES
// ============================================================================

export interface ModelWeights {
  layers: LayerWeights[];
  version: number;
  timestamp: Date;
}

export interface LayerWeights {
  name: string;
  weights: number[][];
  biases: number[];
}

export interface ModelUpdate {
  clientId: string;
  roundId: string;
  weights: ModelWeights;
  metrics: LocalMetrics;
  sampleCount: number;
  computeTime: number;
  timestamp: Date;
}

export interface LocalMetrics {
  loss: number;
  accuracy: number;
  precision: number;
  recall: number;
  f1Score: number;
}

// ============================================================================
// CLIENT TYPES
// ============================================================================

export interface FederatedClient {
  id: string;
  name: string;
  organizationId: string;
  status: ClientStatus;
  capabilities: ClientCapabilities;
  lastSeen: Date;
  roundsParticipated: number;
  totalContributions: number;
  reputation: number;
  registeredAt: Date;
}

export interface ClientCapabilities {
  maxBatchSize: number;
  supportedLanguages: string[];
  hasGPU: boolean;
  memoryGB: number;
  averageComputeTime: number;
}

export type ClientStatus = 'active' | 'idle' | 'training' | 'offline' | 'suspended';

export interface ClientRegistration {
  name: string;
  organizationId: string;
  capabilities: ClientCapabilities;
  publicKey?: string;
}

// ============================================================================
// ROUND TYPES
// ============================================================================

export interface TrainingRound {
  id: string;
  modelId: string;
  roundNumber: number;
  status: RoundStatus;
  config: RoundConfig;
  participants: RoundParticipant[];
  aggregatedWeights?: ModelWeights;
  globalMetrics?: GlobalMetrics;
  startedAt: Date;
  completedAt?: Date;
}

export interface RoundConfig {
  minClients: number;
  maxClients: number;
  clientTimeout: number;
  localEpochs: number;
  batchSize: number;
  learningRate: number;
  aggregationStrategy: AggregationStrategy;
  differentialPrivacy?: DifferentialPrivacyConfig;
}

export interface RoundParticipant {
  clientId: string;
  status: ParticipantStatus;
  update?: ModelUpdate;
  joinedAt: Date;
  completedAt?: Date;
}

export type RoundStatus = 'waiting' | 'training' | 'aggregating' | 'completed' | 'failed';
export type ParticipantStatus = 'joined' | 'training' | 'submitted' | 'timeout' | 'failed';
export type AggregationStrategy = 'fedavg' | 'fedprox' | 'scaffold' | 'weighted';

export interface DifferentialPrivacyConfig {
  enabled: boolean;
  epsilon: number;
  delta: number;
  clipNorm: number;
}

export interface GlobalMetrics {
  averageLoss: number;
  averageAccuracy: number;
  participantCount: number;
  totalSamples: number;
  convergence: number;
}

// ============================================================================
// RULE LEARNING TYPES
// ============================================================================

export interface RuleLearningTask {
  id: string;
  name: string;
  description: string;
  targetRuleType: RuleType;
  sourceLanguages: string[];
  status: TaskStatus;
  model: ModelInfo;
  rounds: TrainingRound[];
  createdAt: Date;
  updatedAt: Date;
}

export interface ModelInfo {
  id: string;
  name: string;
  architecture: string;
  version: number;
  inputShape: number[];
  outputShape: number[];
  parameters: number;
}

export type RuleType = 'calculation' | 'validation' | 'transformation' | 'decision' | 'general';
export type TaskStatus = 'created' | 'active' | 'paused' | 'completed' | 'archived';

// ============================================================================
// CONTRIBUTION TYPES
// ============================================================================

export interface Contribution {
  id: string;
  clientId: string;
  taskId: string;
  roundId: string;
  type: ContributionType;
  impact: number;
  verified: boolean;
  rewardPoints: number;
  timestamp: Date;
}

export type ContributionType = 'model-update' | 'rule-validation' | 'data-labeling' | 'bug-report';

export interface ContributionSummary {
  clientId: string;
  totalContributions: number;
  totalRewardPoints: number;
  contributionsByType: Record<ContributionType, number>;
  averageImpact: number;
  rank: number;
}

// ============================================================================
// PRIVACY TYPES
// ============================================================================

export interface PrivacyBudget {
  clientId: string;
  totalEpsilon: number;
  usedEpsilon: number;
  remainingEpsilon: number;
  resetAt: Date;
}

export interface SecureAggregation {
  enabled: boolean;
  threshold: number;
  protocol: 'masking' | 'homomorphic' | 'mpc';
}

// ============================================================================
// SERVICE INTERFACE
// ============================================================================

export interface IFederatedCoordinator {
  // Client management
  registerClient(registration: ClientRegistration): Promise<FederatedClient>;
  getClient(clientId: string): Promise<FederatedClient | null>;
  updateClientStatus(clientId: string, status: ClientStatus): Promise<void>;

  // Task management
  createTask(task: Partial<RuleLearningTask>): Promise<RuleLearningTask>;
  getTask(taskId: string): Promise<RuleLearningTask | null>;
  startRound(taskId: string, config: RoundConfig): Promise<TrainingRound>;

  // Training coordination
  joinRound(roundId: string, clientId: string): Promise<void>;
  submitUpdate(roundId: string, update: ModelUpdate): Promise<void>;
  getGlobalModel(taskId: string): Promise<ModelWeights>;

  // Contributions
  getContributions(clientId: string): Promise<ContributionSummary>;
}

export interface IFederatedClient {
  connect(coordinatorUrl: string): Promise<void>;
  register(registration: ClientRegistration): Promise<string>;
  joinRound(roundId: string): Promise<RoundConfig>;
  trainLocal(data: unknown[], config: RoundConfig): Promise<ModelUpdate>;
  submitUpdate(roundId: string, update: ModelUpdate): Promise<void>;
  downloadModel(taskId: string): Promise<ModelWeights>;
}
