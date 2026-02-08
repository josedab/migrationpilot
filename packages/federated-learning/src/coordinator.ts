/**
 * Federated Learning Coordinator
 * Manages distributed training across multiple clients
 */

import type {
  FederatedClient,
  ClientRegistration,
  ClientStatus,
  TrainingRound,
  RoundConfig,
  ModelUpdate,
  ModelWeights,
  LayerWeights,
  GlobalMetrics,
  RuleLearningTask,
  TaskStatus,
  Contribution,
  ContributionSummary,
  ContributionType,
  AggregationStrategy,
} from './types.js';

export class FederatedCoordinator {
  private clients: Map<string, FederatedClient> = new Map();
  private tasks: Map<string, RuleLearningTask> = new Map();
  private rounds: Map<string, TrainingRound> = new Map();
  private contributions: Map<string, Contribution[]> = new Map();
  private globalModels: Map<string, ModelWeights> = new Map();

  // ============================================================================
  // CLIENT MANAGEMENT
  // ============================================================================

  async registerClient(registration: ClientRegistration): Promise<FederatedClient> {
    const id = `client_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    const now = new Date();

    const client: FederatedClient = {
      id,
      name: registration.name,
      organizationId: registration.organizationId,
      status: 'idle',
      capabilities: registration.capabilities,
      lastSeen: now,
      roundsParticipated: 0,
      totalContributions: 0,
      reputation: 100,
      registeredAt: now,
    };

    this.clients.set(id, client);
    return client;
  }

  async getClient(clientId: string): Promise<FederatedClient | null> {
    return this.clients.get(clientId) || null;
  }

  async updateClientStatus(clientId: string, status: ClientStatus): Promise<void> {
    const client = this.clients.get(clientId);
    if (!client) {
      throw new Error(`Client not found: ${clientId}`);
    }

    client.status = status;
    client.lastSeen = new Date();
    this.clients.set(clientId, client);
  }

  async listClients(status?: ClientStatus): Promise<FederatedClient[]> {
    let clients = Array.from(this.clients.values());
    if (status) {
      clients = clients.filter(c => c.status === status);
    }
    return clients;
  }

  // ============================================================================
  // TASK MANAGEMENT
  // ============================================================================

  async createTask(data: Partial<RuleLearningTask>): Promise<RuleLearningTask> {
    const id = `task_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    const now = new Date();

    const task: RuleLearningTask = {
      id,
      name: data.name || 'Untitled Task',
      description: data.description || '',
      targetRuleType: data.targetRuleType || 'general',
      sourceLanguages: data.sourceLanguages || [],
      status: 'created',
      model: data.model || {
        id: `model_${id}`,
        name: 'RuleExtractor',
        architecture: 'transformer',
        version: 1,
        inputShape: [512],
        outputShape: [128],
        parameters: 1000000,
      },
      rounds: [],
      createdAt: now,
      updatedAt: now,
    };

    this.tasks.set(id, task);

    // Initialize global model with random weights
    const initialWeights = this.initializeModel(task.model.inputShape, task.model.outputShape);
    this.globalModels.set(id, initialWeights);

    return task;
  }

  async getTask(taskId: string): Promise<RuleLearningTask | null> {
    return this.tasks.get(taskId) || null;
  }

  async updateTaskStatus(taskId: string, status: TaskStatus): Promise<void> {
    const task = this.tasks.get(taskId);
    if (!task) {
      throw new Error(`Task not found: ${taskId}`);
    }

    task.status = status;
    task.updatedAt = new Date();
    this.tasks.set(taskId, task);
  }

  async listTasks(status?: TaskStatus): Promise<RuleLearningTask[]> {
    let tasks = Array.from(this.tasks.values());
    if (status) {
      tasks = tasks.filter(t => t.status === status);
    }
    return tasks;
  }

  // ============================================================================
  // TRAINING COORDINATION
  // ============================================================================

  async startRound(taskId: string, config: RoundConfig): Promise<TrainingRound> {
    const task = this.tasks.get(taskId);
    if (!task) {
      throw new Error(`Task not found: ${taskId}`);
    }

    const roundNumber = task.rounds.length + 1;
    const id = `round_${taskId}_${roundNumber}`;
    const now = new Date();

    const round: TrainingRound = {
      id,
      modelId: task.model.id,
      roundNumber,
      status: 'waiting',
      config,
      participants: [],
      startedAt: now,
    };

    this.rounds.set(id, round);
    task.rounds.push(round);
    task.status = 'active';
    task.updatedAt = now;
    this.tasks.set(taskId, task);

    return round;
  }

  async joinRound(roundId: string, clientId: string): Promise<void> {
    const round = this.rounds.get(roundId);
    if (!round) {
      throw new Error(`Round not found: ${roundId}`);
    }

    const client = this.clients.get(clientId);
    if (!client) {
      throw new Error(`Client not found: ${clientId}`);
    }

    if (round.status !== 'waiting') {
      throw new Error('Round is not accepting participants');
    }

    if (round.participants.length >= round.config.maxClients) {
      throw new Error('Round is full');
    }

    // Check if client already joined
    if (round.participants.some(p => p.clientId === clientId)) {
      throw new Error('Client already joined this round');
    }

    round.participants.push({
      clientId,
      status: 'joined',
      joinedAt: new Date(),
    });

    client.status = 'training';
    this.clients.set(clientId, client);

    // Start training if minimum clients reached
    if (round.participants.length >= round.config.minClients) {
      round.status = 'training';
    }

    this.rounds.set(roundId, round);
  }

  async submitUpdate(roundId: string, update: ModelUpdate): Promise<void> {
    const round = this.rounds.get(roundId);
    if (!round) {
      throw new Error(`Round not found: ${roundId}`);
    }

    const participant = round.participants.find(p => p.clientId === update.clientId);
    if (!participant) {
      throw new Error('Client is not a participant in this round');
    }

    participant.status = 'submitted';
    participant.update = update;
    participant.completedAt = new Date();

    // Update client stats
    const client = this.clients.get(update.clientId);
    if (client) {
      client.status = 'idle';
      client.roundsParticipated++;
      client.totalContributions++;
      this.clients.set(update.clientId, client);
    }

    // Record contribution
    this.recordContribution(update.clientId, round.id, 'model-update', update.metrics.accuracy);

    this.rounds.set(roundId, round);

    // Check if all participants submitted
    const allSubmitted = round.participants.every(
      p => p.status === 'submitted' || p.status === 'timeout' || p.status === 'failed'
    );

    if (allSubmitted) {
      await this.aggregateRound(roundId);
    }
  }

  async getRound(roundId: string): Promise<TrainingRound | null> {
    return this.rounds.get(roundId) || null;
  }

  async getGlobalModel(taskId: string): Promise<ModelWeights> {
    const weights = this.globalModels.get(taskId);
    if (!weights) {
      throw new Error(`No global model found for task: ${taskId}`);
    }
    return weights;
  }

  // ============================================================================
  // AGGREGATION
  // ============================================================================

  private async aggregateRound(roundId: string): Promise<void> {
    const round = this.rounds.get(roundId);
    if (!round) return;

    round.status = 'aggregating';

    const submittedUpdates = round.participants
      .filter(p => p.status === 'submitted' && p.update)
      .map(p => p.update!);

    if (submittedUpdates.length === 0) {
      round.status = 'failed';
      this.rounds.set(roundId, round);
      return;
    }

    // Aggregate weights based on strategy
    const aggregatedWeights = this.aggregateWeights(
      submittedUpdates,
      round.config.aggregationStrategy
    );

    // Calculate global metrics
    const globalMetrics = this.calculateGlobalMetrics(submittedUpdates);

    round.aggregatedWeights = aggregatedWeights;
    round.globalMetrics = globalMetrics;
    round.status = 'completed';
    round.completedAt = new Date();

    this.rounds.set(roundId, round);

    // Update global model
    const taskId = this.findTaskIdForRound(roundId);
    if (taskId) {
      this.globalModels.set(taskId, aggregatedWeights);
    }
  }

  private aggregateWeights(
    updates: ModelUpdate[],
    strategy: AggregationStrategy
  ): ModelWeights {
    if (updates.length === 0) {
      throw new Error('No updates to aggregate');
    }

    const firstUpdate = updates[0]!;
    const layerCount = firstUpdate.weights.layers.length;

    // Calculate total samples for weighted averaging
    const totalSamples = updates.reduce((sum, u) => sum + u.sampleCount, 0);

    // Initialize aggregated layers
    const aggregatedLayers: LayerWeights[] = [];

    for (let l = 0; l < layerCount; l++) {
      const layerName = firstUpdate.weights.layers[l]!.name;
      const weightRows = firstUpdate.weights.layers[l]!.weights.length;
      const weightCols = firstUpdate.weights.layers[l]!.weights[0]?.length || 0;
      const biasLength = firstUpdate.weights.layers[l]!.biases.length;

      // Initialize with zeros
      const weights: number[][] = Array(weightRows)
        .fill(null)
        .map(() => Array(weightCols).fill(0));
      const biases: number[] = Array(biasLength).fill(0);

      // Aggregate based on strategy
      for (const update of updates) {
        const clientWeight = strategy === 'weighted'
          ? update.sampleCount / totalSamples
          : 1 / updates.length;

        const layer = update.weights.layers[l]!;

        for (let i = 0; i < weightRows; i++) {
          for (let j = 0; j < weightCols; j++) {
            weights[i]![j]! += (layer.weights[i]?.[j] || 0) * clientWeight;
          }
        }

        for (let i = 0; i < biasLength; i++) {
          biases[i]! += (layer.biases[i] || 0) * clientWeight;
        }
      }

      aggregatedLayers.push({ name: layerName, weights, biases });
    }

    return {
      layers: aggregatedLayers,
      version: firstUpdate.weights.version + 1,
      timestamp: new Date(),
    };
  }

  private calculateGlobalMetrics(updates: ModelUpdate[]): GlobalMetrics {
    const totalSamples = updates.reduce((sum, u) => sum + u.sampleCount, 0);

    // Weighted average of metrics
    let weightedLoss = 0;
    let weightedAccuracy = 0;

    for (const update of updates) {
      const weight = update.sampleCount / totalSamples;
      weightedLoss += update.metrics.loss * weight;
      weightedAccuracy += update.metrics.accuracy * weight;
    }

    // Calculate convergence (simple: based on accuracy improvement)
    const convergence = Math.min(1, weightedAccuracy);

    return {
      averageLoss: weightedLoss,
      averageAccuracy: weightedAccuracy,
      participantCount: updates.length,
      totalSamples,
      convergence,
    };
  }

  // ============================================================================
  // CONTRIBUTIONS
  // ============================================================================

  private recordContribution(
    clientId: string,
    roundId: string,
    type: ContributionType,
    impact: number
  ): void {
    const taskId = this.findTaskIdForRound(roundId);
    if (!taskId) return;

    const contribution: Contribution = {
      id: `contrib_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`,
      clientId,
      taskId,
      roundId,
      type,
      impact,
      verified: true,
      rewardPoints: Math.round(impact * 100),
      timestamp: new Date(),
    };

    const clientContribs = this.contributions.get(clientId) || [];
    clientContribs.push(contribution);
    this.contributions.set(clientId, clientContribs);
  }

  async getContributions(clientId: string): Promise<ContributionSummary> {
    const contribs = this.contributions.get(clientId) || [];

    const byType: Record<ContributionType, number> = {
      'model-update': 0,
      'rule-validation': 0,
      'data-labeling': 0,
      'bug-report': 0,
    };

    let totalRewardPoints = 0;
    let totalImpact = 0;

    for (const c of contribs) {
      byType[c.type]++;
      totalRewardPoints += c.rewardPoints;
      totalImpact += c.impact;
    }

    // Calculate rank based on total reward points
    const allClients = Array.from(this.contributions.entries())
      .map(([id, cs]) => ({
        id,
        points: cs.reduce((sum, c) => sum + c.rewardPoints, 0),
      }))
      .sort((a, b) => b.points - a.points);

    const rank = allClients.findIndex(c => c.id === clientId) + 1;

    return {
      clientId,
      totalContributions: contribs.length,
      totalRewardPoints,
      contributionsByType: byType,
      averageImpact: contribs.length > 0 ? totalImpact / contribs.length : 0,
      rank: rank || allClients.length + 1,
    };
  }

  // ============================================================================
  // UTILITIES
  // ============================================================================

  private initializeModel(inputShape: number[], outputShape: number[]): ModelWeights {
    // Create simple neural network weights
    const inputSize = inputShape[0] || 512;
    const hiddenSize = 256;
    const outputSize = outputShape[0] || 128;

    return {
      layers: [
        {
          name: 'encoder',
          weights: this.randomMatrix(inputSize, hiddenSize),
          biases: this.randomArray(hiddenSize),
        },
        {
          name: 'hidden',
          weights: this.randomMatrix(hiddenSize, hiddenSize),
          biases: this.randomArray(hiddenSize),
        },
        {
          name: 'decoder',
          weights: this.randomMatrix(hiddenSize, outputSize),
          biases: this.randomArray(outputSize),
        },
      ],
      version: 1,
      timestamp: new Date(),
    };
  }

  private randomMatrix(rows: number, cols: number): number[][] {
    return Array(rows)
      .fill(null)
      .map(() => Array(cols).fill(0).map(() => (Math.random() - 0.5) * 0.1));
  }

  private randomArray(size: number): number[] {
    return Array(size).fill(0).map(() => (Math.random() - 0.5) * 0.1);
  }

  private findTaskIdForRound(roundId: string): string | null {
    for (const [taskId, task] of this.tasks) {
      if (task.rounds.some(r => r.id === roundId)) {
        return taskId;
      }
    }
    return null;
  }
}
