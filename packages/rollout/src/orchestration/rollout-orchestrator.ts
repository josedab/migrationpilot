/**
 * Rollout Orchestrator
 * Main orchestration engine for progressive deployment rollouts
 */

import {
  RolloutConfig,
  RolloutStage,
  RolloutStatus,
  RolloutEvent,
  RolloutResult,
  RolloutMetrics,
  RolloutOrchestratorConfig,
  HealthCheckResult,
  ThresholdViolation,
  StageMetrics,
} from '../types';

export class RolloutOrchestrator {
  private readonly config: RolloutOrchestratorConfig;
  private activeRollouts: Map<string, RolloutState> = new Map();
  private healthCheckTimers: Map<string, NodeJS.Timeout> = new Map();

  constructor(config: RolloutOrchestratorConfig) {
    this.config = config;
  }

  async startRollout(rolloutConfig: RolloutConfig): Promise<RolloutResult> {
    const state = this.initializeRolloutState(rolloutConfig);
    this.activeRollouts.set(rolloutConfig.id, state);

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId: rolloutConfig.id,
      type: 'started',
      timestamp: new Date(),
      data: { config: rolloutConfig },
    });

    this.startHealthChecks(rolloutConfig.id);

    try {
      for (const stage of rolloutConfig.stages) {
        const stageResult = await this.executeStage(rolloutConfig.id, stage);
        
        if (stageResult.status === 'failed') {
          if (rolloutConfig.autoRollback.enabled) {
            await this.initiateRollback(rolloutConfig.id, `Stage ${stage.name} failed`);
          }
          return this.buildResult(rolloutConfig.id, 'failed');
        }

        if (state.status === 'rolled-back') {
          return this.buildResult(rolloutConfig.id, 'rolled-back');
        }
      }

      state.status = 'completed';
      state.completedAt = new Date();
      
      await this.emitEvent({
        id: `evt_${Date.now()}`,
        rolloutId: rolloutConfig.id,
        type: 'completed',
        timestamp: new Date(),
        data: { metrics: state.metrics },
      });

      return this.buildResult(rolloutConfig.id, 'completed');
    } finally {
      this.stopHealthChecks(rolloutConfig.id);
      this.activeRollouts.delete(rolloutConfig.id);
    }
  }

  async pauseRollout(rolloutId: string): Promise<void> {
    const state = this.activeRollouts.get(rolloutId);
    if (!state) throw new Error(`Rollout ${rolloutId} not found`);

    state.status = 'paused';
    this.stopHealthChecks(rolloutId);

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId,
      type: 'paused',
      timestamp: new Date(),
      data: {},
    });
  }

  async resumeRollout(rolloutId: string): Promise<void> {
    const state = this.activeRollouts.get(rolloutId);
    if (!state || state.status !== 'paused') {
      throw new Error(`Rollout ${rolloutId} is not paused`);
    }

    state.status = 'in-progress';
    this.startHealthChecks(rolloutId);

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId,
      type: 'resumed',
      timestamp: new Date(),
      data: {},
    });
  }

  async initiateRollback(rolloutId: string, reason: string): Promise<void> {
    const state = this.activeRollouts.get(rolloutId);
    if (!state) throw new Error(`Rollout ${rolloutId} not found`);

    state.status = 'rolled-back';
    state.rollbackReason = reason;
    state.metrics.rollbackCount++;

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId,
      type: 'rollback-initiated',
      timestamp: new Date(),
      data: { reason },
    });

    await this.executeRollback(state);

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId,
      type: 'rollback-completed',
      timestamp: new Date(),
      data: {},
    });
  }

  getRolloutStatus(rolloutId: string): RolloutState | undefined {
    return this.activeRollouts.get(rolloutId);
  }

  private initializeRolloutState(config: RolloutConfig): RolloutState {
    return {
      config,
      status: 'in-progress',
      currentStage: 0,
      startedAt: new Date(),
      events: [],
      metrics: {
        stagesCompleted: 0,
        totalStages: config.stages.length,
        peakTrafficPercentage: 0,
        totalRequests: 0,
        totalErrors: 0,
        overallErrorRate: 0,
        avgLatencyP99: 0,
        rollbackCount: 0,
      },
    };
  }

  private async executeStage(rolloutId: string, stage: RolloutStage): Promise<StageResult> {
    const state = this.activeRollouts.get(rolloutId);
    if (!state) throw new Error(`Rollout ${rolloutId} not found`);

    stage.status = 'active';
    stage.startedAt = new Date();

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId,
      type: 'stage-promoted',
      timestamp: new Date(),
      stage: stage.id,
      data: { percentage: stage.percentage },
    });

    // Update traffic split
    await this.updateTrafficSplit(state.config.targetComponent, stage.percentage);

    // Wait for stage duration while monitoring
    const stageEndTime = Date.now() + stage.duration * 60 * 1000;
    
    while (Date.now() < stageEndTime) {
      if (state.status === 'paused') {
        await this.waitForResume(rolloutId);
      }
      
      if (state.status === 'rolled-back') {
        stage.status = 'failed';
        return { status: 'failed', reason: state.rollbackReason };
      }

      const healthCheck = await this.performHealthCheck(state);
      
      if (!healthCheck.healthy) {
        const shouldRollback = this.shouldAutoRollback(healthCheck.violations, state.config);
        if (shouldRollback) {
          stage.status = 'failed';
          return { status: 'failed', reason: 'Health check failed', violations: healthCheck.violations };
        }
      }

      stage.metrics = healthCheck.metrics;
      await this.sleep(5000); // Check every 5 seconds
    }

    // Verify promotion criteria
    const metricsOk = await this.verifyPromotionCriteria(stage);
    if (!metricsOk) {
      stage.status = 'failed';
      return { status: 'failed', reason: 'Promotion criteria not met' };
    }

    stage.status = 'completed';
    stage.completedAt = new Date();
    state.metrics.stagesCompleted++;
    state.metrics.peakTrafficPercentage = Math.max(state.metrics.peakTrafficPercentage, stage.percentage);
    state.currentStage++;

    await this.emitEvent({
      id: `evt_${Date.now()}`,
      rolloutId,
      type: 'stage-completed',
      timestamp: new Date(),
      stage: stage.id,
      data: { metrics: stage.metrics },
    });

    return { status: 'completed' };
  }

  private async performHealthCheck(state: RolloutState): Promise<HealthCheckResult> {
    const { metricsProvider } = this.config;
    const labels = { component: state.config.targetComponent };
    const window = 60; // 1 minute window

    const [requestCount, errorCount, latencyP50, latencyP95, latencyP99] = await Promise.all([
      metricsProvider.query('request_count', labels, window),
      metricsProvider.query('error_count', labels, window),
      metricsProvider.query('latency_p50', labels, window),
      metricsProvider.query('latency_p95', labels, window),
      metricsProvider.query('latency_p99', labels, window),
    ]);

    const errorRate = requestCount > 0 ? errorCount / requestCount : 0;
    const successRate = 1 - errorRate;

    const metrics: StageMetrics = {
      requestCount,
      errorCount,
      errorRate,
      successRate,
      latencyP50,
      latencyP95,
      latencyP99,
      customMetrics: {},
    };

    // Update overall metrics
    state.metrics.totalRequests += requestCount;
    state.metrics.totalErrors += errorCount;
    state.metrics.overallErrorRate = state.metrics.totalRequests > 0 
      ? state.metrics.totalErrors / state.metrics.totalRequests 
      : 0;

    const violations = this.checkThresholds(metrics, state.config.healthThresholds);

    return {
      timestamp: new Date(),
      healthy: violations.length === 0,
      metrics,
      violations,
    };
  }

  private checkThresholds(
    metrics: StageMetrics,
    thresholds: RolloutConfig['healthThresholds']
  ): ThresholdViolation[] {
    const violations: ThresholdViolation[] = [];

    if (metrics.errorRate > thresholds.errorRateThreshold) {
      violations.push({
        metric: 'error_rate',
        threshold: thresholds.errorRateThreshold,
        actual: metrics.errorRate,
        severity: metrics.errorRate > thresholds.errorRateThreshold * 2 ? 'critical' : 'warning',
      });
    }

    if (metrics.latencyP99 > thresholds.latencyThreshold) {
      violations.push({
        metric: 'latency_p99',
        threshold: thresholds.latencyThreshold,
        actual: metrics.latencyP99,
        severity: metrics.latencyP99 > thresholds.latencyThreshold * 2 ? 'critical' : 'warning',
      });
    }

    const availability = metrics.successRate;
    if (availability < thresholds.availabilityThreshold) {
      violations.push({
        metric: 'availability',
        threshold: thresholds.availabilityThreshold,
        actual: availability,
        severity: availability < thresholds.availabilityThreshold * 0.9 ? 'critical' : 'warning',
      });
    }

    return violations;
  }

  private shouldAutoRollback(
    violations: ThresholdViolation[],
    config: RolloutConfig
  ): boolean {
    if (!config.autoRollback.enabled) return false;
    return violations.some(v => v.severity === 'critical');
  }

  private async verifyPromotionCriteria(stage: RolloutStage): Promise<boolean> {
    const { criteria } = stage;
    const metrics = stage.metrics;

    if (!metrics) return false;

    if (metrics.errorRate > criteria.maxErrorRate) return false;
    if (metrics.successRate < criteria.minSuccessRate) return false;
    if (metrics.latencyP99 > criteria.latencyP99Max) return false;

    // Verify custom checks if any
    if (criteria.customChecks) {
      for (const check of criteria.customChecks) {
        const value = await this.config.metricsProvider.query(
          check.query,
          {},
          60
        );
        if (!this.evaluateCheck(value, check.threshold, check.operator)) {
          return false;
        }
      }
    }

    return true;
  }

  private evaluateCheck(value: number, threshold: number, operator: string): boolean {
    switch (operator) {
      case 'gt': return value > threshold;
      case 'gte': return value >= threshold;
      case 'lt': return value < threshold;
      case 'lte': return value <= threshold;
      case 'eq': return value === threshold;
      default: return false;
    }
  }

  private async updateTrafficSplit(component: string, percentage: number): Promise<void> {
    console.log(`Updating traffic split for ${component}: ${percentage}% to new version`);
    // Implementation would integrate with service mesh / load balancer
  }

  private async executeRollback(state: RolloutState): Promise<void> {
    console.log(`Executing rollback for ${state.config.id}`);
    
    if (state.config.autoRollback.rollbackStrategy === 'immediate') {
      await this.updateTrafficSplit(state.config.targetComponent, 0);
    } else {
      // Gradual rollback
      const currentPercentage = state.metrics.peakTrafficPercentage;
      for (let pct = currentPercentage; pct >= 0; pct -= 10) {
        await this.updateTrafficSplit(state.config.targetComponent, pct);
        await this.sleep(5000);
      }
    }
  }

  private startHealthChecks(rolloutId: string): void {
    const timer = setInterval(async () => {
      const state = this.activeRollouts.get(rolloutId);
      if (!state || state.status !== 'in-progress') return;

      const healthCheck = await this.performHealthCheck(state);
      
      await this.emitEvent({
        id: `evt_${Date.now()}`,
        rolloutId,
        type: 'health-check',
        timestamp: new Date(),
        data: { healthy: healthCheck.healthy, metrics: healthCheck.metrics },
      });

      if (healthCheck.violations.length > 0) {
        await this.emitEvent({
          id: `evt_${Date.now()}`,
          rolloutId,
          type: 'threshold-breach',
          timestamp: new Date(),
          data: { violations: healthCheck.violations },
        });
      }
    }, this.config.healthCheckInterval * 1000);

    this.healthCheckTimers.set(rolloutId, timer);
  }

  private stopHealthChecks(rolloutId: string): void {
    const timer = this.healthCheckTimers.get(rolloutId);
    if (timer) {
      clearInterval(timer);
      this.healthCheckTimers.delete(rolloutId);
    }
  }

  private async waitForResume(rolloutId: string): Promise<void> {
    while (true) {
      const state = this.activeRollouts.get(rolloutId);
      if (!state || state.status !== 'paused') break;
      await this.sleep(1000);
    }
  }

  private async emitEvent(event: RolloutEvent): Promise<void> {
    const state = this.activeRollouts.get(event.rolloutId);
    if (state) {
      state.events.push(event);
    }

    if (this.config.eventStore) {
      await this.config.eventStore.save(event);
    }

    if (this.config.notificationChannels) {
      for (const channel of this.config.notificationChannels) {
        await channel.notify(event).catch(console.error);
      }
    }
  }

  private buildResult(rolloutId: string, status: RolloutStatus): RolloutResult {
    const state = this.activeRollouts.get(rolloutId);
    if (!state) {
      throw new Error(`Rollout ${rolloutId} not found`);
    }

    return {
      rolloutId,
      status,
      startedAt: state.startedAt,
      completedAt: state.completedAt,
      finalStage: state.config.stages[state.currentStage - 1]?.name,
      totalDuration: state.completedAt 
        ? state.completedAt.getTime() - state.startedAt.getTime() 
        : undefined,
      rollbackReason: state.rollbackReason,
      metrics: state.metrics,
      events: state.events,
    };
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

interface RolloutState {
  config: RolloutConfig;
  status: RolloutStatus;
  currentStage: number;
  startedAt: Date;
  completedAt?: Date;
  rollbackReason?: string;
  events: RolloutEvent[];
  metrics: RolloutMetrics;
}

interface StageResult {
  status: 'completed' | 'failed';
  reason?: string;
  violations?: ThresholdViolation[];
}

export function createRolloutOrchestrator(
  config: RolloutOrchestratorConfig
): RolloutOrchestrator {
  return new RolloutOrchestrator(config);
}
