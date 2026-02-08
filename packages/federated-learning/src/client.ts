/**
 * Federated Learning Client
 * Local training client that participates in federated learning
 */

import type {
  FederatedClient,
  ClientRegistration,
  RoundConfig,
  ModelUpdate,
  ModelWeights,
  LocalMetrics,
} from './types.js';

export class FederatedLearningClient {
  private clientId: string | null = null;
  private clientInfo: FederatedClient | null = null;
  private currentModel: ModelWeights | null = null;
  private coordinatorUrl: string | null = null;

  // ============================================================================
  // CONNECTION
  // ============================================================================

  async connect(coordinatorUrl: string): Promise<void> {
    this.coordinatorUrl = coordinatorUrl;
    // In production, would establish WebSocket or HTTP connection
    console.log(`Connected to coordinator at ${coordinatorUrl}`);
  }

  async disconnect(): Promise<void> {
    this.coordinatorUrl = null;
    this.clientId = null;
    this.clientInfo = null;
    console.log('Disconnected from coordinator');
  }

  isConnected(): boolean {
    return this.coordinatorUrl !== null;
  }

  // ============================================================================
  // REGISTRATION
  // ============================================================================

  async register(registration: ClientRegistration): Promise<string> {
    if (!this.isConnected()) {
      throw new Error('Not connected to coordinator');
    }

    // In production, would call coordinator API
    const clientId = `client_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
    this.clientId = clientId;

    this.clientInfo = {
      id: clientId,
      name: registration.name,
      organizationId: registration.organizationId,
      status: 'idle',
      capabilities: registration.capabilities,
      lastSeen: new Date(),
      roundsParticipated: 0,
      totalContributions: 0,
      reputation: 100,
      registeredAt: new Date(),
    };

    return clientId;
  }

  getClientId(): string | null {
    return this.clientId;
  }

  getClientInfo(): FederatedClient | null {
    return this.clientInfo;
  }

  // ============================================================================
  // TRAINING PARTICIPATION
  // ============================================================================

  async joinRound(_roundId: string): Promise<RoundConfig> {
    if (!this.clientId) {
      throw new Error('Client not registered');
    }

    // In production, would call coordinator API
    // For now, return default config
    return {
      minClients: 3,
      maxClients: 10,
      clientTimeout: 300000,
      localEpochs: 5,
      batchSize: 32,
      learningRate: 0.001,
      aggregationStrategy: 'fedavg',
    };
  }

  async downloadModel(_taskId: string): Promise<ModelWeights> {
    if (!this.isConnected()) {
      throw new Error('Not connected to coordinator');
    }

    // In production, would download from coordinator
    // For now, create initial weights
    this.currentModel = this.createInitialWeights();
    return this.currentModel;
  }

  async trainLocal(
    data: TrainingData[],
    config: RoundConfig
  ): Promise<ModelUpdate> {
    if (!this.clientId) {
      throw new Error('Client not registered');
    }

    if (!this.currentModel) {
      throw new Error('No model loaded. Download model first.');
    }

    const startTime = Date.now();

    // Simulate local training
    const updatedWeights = await this.performLocalTraining(
      this.currentModel,
      data,
      config
    );

    // Calculate local metrics
    const metrics = this.evaluateModel(updatedWeights, data);

    const computeTime = Date.now() - startTime;

    const update: ModelUpdate = {
      clientId: this.clientId,
      roundId: '', // Will be set by caller
      weights: updatedWeights,
      metrics,
      sampleCount: data.length,
      computeTime,
      timestamp: new Date(),
    };

    this.currentModel = updatedWeights;
    return update;
  }

  async submitUpdate(roundId: string, update: ModelUpdate): Promise<void> {
    if (!this.isConnected()) {
      throw new Error('Not connected to coordinator');
    }

    update.roundId = roundId;

    // In production, would send to coordinator via API
    console.log(`Submitted update for round ${roundId}`);

    if (this.clientInfo) {
      this.clientInfo.roundsParticipated++;
      this.clientInfo.totalContributions++;
    }
  }

  // ============================================================================
  // LOCAL TRAINING (Simulated)
  // ============================================================================

  private async performLocalTraining(
    weights: ModelWeights,
    data: TrainingData[],
    config: RoundConfig
  ): Promise<ModelWeights> {
    // Simulate gradient descent updates
    const updatedLayers = weights.layers.map(layer => {
      const newWeights = layer.weights.map(row =>
        row.map(w => w + (Math.random() - 0.5) * config.learningRate)
      );
      const newBiases = layer.biases.map(
        b => b + (Math.random() - 0.5) * config.learningRate
      );
      return {
        name: layer.name,
        weights: newWeights,
        biases: newBiases,
      };
    });

    // Simulate training time based on data size and epochs
    const trainingTime = Math.min(
      100 + data.length * config.localEpochs * 0.1,
      5000
    );
    await this.delay(trainingTime);

    return {
      layers: updatedLayers,
      version: weights.version,
      timestamp: new Date(),
    };
  }

  private evaluateModel(_weights: ModelWeights, _data: TrainingData[]): LocalMetrics {
    // Simulate evaluation metrics
    // In production, would actually evaluate on validation set
    const baseAccuracy = 0.6 + Math.random() * 0.3;
    const baseLoss = 0.5 - baseAccuracy * 0.4;

    return {
      loss: Math.max(0.01, baseLoss + (Math.random() - 0.5) * 0.1),
      accuracy: Math.min(0.99, baseAccuracy + (Math.random() - 0.5) * 0.1),
      precision: Math.min(0.99, baseAccuracy + (Math.random() - 0.5) * 0.05),
      recall: Math.min(0.99, baseAccuracy + (Math.random() - 0.5) * 0.05),
      f1Score: Math.min(0.99, baseAccuracy + (Math.random() - 0.5) * 0.05),
    };
  }

  // ============================================================================
  // UTILITIES
  // ============================================================================

  private createInitialWeights(): ModelWeights {
    return {
      layers: [
        {
          name: 'encoder',
          weights: this.randomMatrix(512, 256),
          biases: this.randomArray(256),
        },
        {
          name: 'hidden',
          weights: this.randomMatrix(256, 256),
          biases: this.randomArray(256),
        },
        {
          name: 'decoder',
          weights: this.randomMatrix(256, 128),
          biases: this.randomArray(128),
        },
      ],
      version: 1,
      timestamp: new Date(),
    };
  }

  private randomMatrix(rows: number, cols: number): number[][] {
    return Array(rows)
      .fill(null)
      .map(() =>
        Array(cols)
          .fill(0)
          .map(() => (Math.random() - 0.5) * 0.1)
      );
  }

  private randomArray(size: number): number[] {
    return Array(size)
      .fill(0)
      .map(() => (Math.random() - 0.5) * 0.1);
  }

  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// ============================================================================
// TRAINING DATA TYPE
// ============================================================================

export interface TrainingData {
  input: number[];
  label: number[];
  metadata?: Record<string, unknown>;
}
