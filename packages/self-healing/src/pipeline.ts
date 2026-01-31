/**
 * Self-Healing Pipeline - Main orchestrator
 */

import type {
  PipelineConfig,
  PipelineRun,
  PipelineEvent,
  PipelineMetrics,
  StageConfig,
  TriggerCondition,
  HealingEvent,
} from './types.js';

import { PIPELINE_STAGE_TIMEOUT_MS } from '@migrationpilot/core';
import { HealthMonitor, createHealthMonitor, createMetricsCollector } from './monitors/health-monitor.js';
import { AutoHealer, createAutoHealer, type HealingContext } from './healers/auto-healer.js';
import { createRetryHandler } from './policies/retry-policy.js';

// ============================================================================
// Self-Healing Pipeline
// ============================================================================

export class SelfHealingPipeline {
  private config: PipelineConfig;
  private healthMonitor: HealthMonitor;
  private autoHealer: AutoHealer;
  private eventHandlers: ((event: PipelineEvent) => void)[] = [];
  private currentRun: PipelineRun | null = null;

  constructor(config: PipelineConfig) {
    this.config = config;
    this.healthMonitor = createHealthMonitor(config.monitoringConfig);
    // Initialize collectors even if unused in demo (ready for metrics collection)
    createMetricsCollector(config.monitoringConfig);
    createRetryHandler();
    this.autoHealer = createAutoHealer(
      config.healingPolicy.healingStrategies,
      config.healingPolicy.maxHealingAttempts,
      config.healingPolicy.cooldownPeriod
    );

    // Wire up health monitor alerts
    this.healthMonitor.onAlert(alert => {
      this.emit({ type: 'alert:triggered', alert });
    });
  }

  /**
   * Subscribe to pipeline events
   */
  onEvent(handler: (event: PipelineEvent) => void): () => void {
    this.eventHandlers.push(handler);
    return () => {
      const idx = this.eventHandlers.indexOf(handler);
      if (idx > -1) this.eventHandlers.splice(idx, 1);
    };
  }

  /**
   * Execute the pipeline
   */
  async execute(
    stageHandlers: Map<string, StageHandler>
  ): Promise<PipelineRun> {
    const run = this.createRun();
    this.currentRun = run;
    
    this.emit({ type: 'pipeline:started', run });

    try {
      // Execute stages in order (respecting dependencies)
      const executionOrder = this.resolveExecutionOrder();

      for (const stageId of executionOrder) {
        const stage = this.config.stages.find(s => s.id === stageId);
        if (!stage) continue;

        const handler = stageHandlers.get(stageId);
        if (!handler) {
          throw new Error(`No handler registered for stage: ${stageId}`);
        }

        await this.executeStage(stage, handler, run);

        // Check if pipeline should stop
        const stageExec = run.stages.find(s => s.stageId === stageId);
        if (stageExec?.status === 'failed') {
          if (this.config.healingPolicy.autoRollback) {
            await this.rollbackPipeline(run, stageId);
            run.status = 'rolled-back';
          } else {
            run.status = 'failed';
          }
          break;
        }
      }

      // Calculate final status
      if (run.status === 'running') {
        const allCompleted = run.stages.every(
          s => s.status === 'completed' || s.status === 'skipped'
        );
        run.status = allCompleted ? 'completed' : 'failed';
      }

      run.completedAt = new Date();
      run.metrics = this.calculatePipelineMetrics(run);

      if (run.status === 'completed') {
        this.emit({ type: 'pipeline:completed', run });
      } else {
        this.emit({
          type: 'pipeline:failed',
          run,
          error: run.error || { code: 'UNKNOWN', message: 'Pipeline failed', recoverable: false },
        });
      }

    } catch (error) {
      run.status = 'failed';
      run.completedAt = new Date();
      run.error = {
        code: 'PIPELINE_ERROR',
        message: error instanceof Error ? error.message : 'Unknown error',
        recoverable: false,
        stack: error instanceof Error ? error.stack : undefined,
      };
      
      this.emit({ type: 'pipeline:failed', run, error: run.error });
    }

    this.currentRun = null;
    return run;
  }

  /**
   * Cancel the current pipeline run
   */
  cancel(): boolean {
    if (!this.currentRun) return false;
    this.currentRun.status = 'cancelled';
    this.currentRun.completedAt = new Date();
    return true;
  }

  /**
   * Get current run
   */
  getCurrentRun(): PipelineRun | null {
    return this.currentRun;
  }

  /**
   * Get pipeline configuration
   */
  getConfig(): PipelineConfig {
    return { ...this.config };
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private createRun(): PipelineRun {
    return {
      id: `run_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      pipelineId: this.config.name,
      status: 'running',
      stages: this.config.stages.map(stage => ({
        stageId: stage.id,
        status: 'pending',
        attempts: [],
        healthStatus: {
          healthy: true,
          lastCheck: new Date(),
          consecutiveFailures: 0,
          consecutiveSuccesses: 0,
          checks: [],
        },
        metrics: {
          duration: 0,
          attempts: 0,
          itemsProcessed: 0,
          itemsFailed: 0,
          errorRate: 0,
        },
      })),
      startedAt: new Date(),
      healingEvents: [],
      metrics: {
        duration: 0,
        stagesCompleted: 0,
        stagesFailed: 0,
        totalRetries: 0,
        healingEventsTriggered: 0,
        successfulHealings: 0,
        errorRate: 0,
        throughput: 0,
      },
    };
  }

  private async executeStage(
    stage: StageConfig,
    handler: StageHandler,
    run: PipelineRun
  ): Promise<void> {
    const stageExec = run.stages.find(s => s.stageId === stage.id);
    if (!stageExec) return;

    stageExec.status = 'running';
    stageExec.startedAt = new Date();
    this.emit({ type: 'stage:started', runId: run.id, stageId: stage.id });

    const startTime = Date.now();
    let attempt = 0;
    const maxAttempts = (stage.retryPolicy?.maxRetries ?? 0) + 1;

    while (attempt < maxAttempts) {
      attempt++;
      const attemptStart = Date.now();

      try {
        // Run health checks before execution
        if (stage.healthChecks && stage.healthChecks.length > 0) {
          const healthStatus = await this.healthMonitor.runAllHealthChecks(
            stage.id,
            stage.healthChecks
          );
          stageExec.healthStatus = healthStatus;

          if (!healthStatus.healthy) {
            this.emit({
              type: 'health:degraded',
              runId: run.id,
              stageId: stage.id,
              status: healthStatus,
            });

            // Trigger healing if enabled
            if (this.config.healingPolicy.enabled) {
              const healingEvent = await this.triggerHealing(
                run,
                stage.id,
                { type: 'health-check' }
              );
              if (healingEvent.outcome !== 'resolved') {
                throw new Error('Health check failed and healing unsuccessful');
              }
            }
          }
        }

        // Execute the stage
        const result = await Promise.race([
          handler(stage, run),
          this.createTimeout(stage.timeout || PIPELINE_STAGE_TIMEOUT_MS),
        ]);

        // Record successful attempt
        stageExec.attempts.push({
          attemptNumber: attempt,
          startedAt: new Date(attemptStart),
          completedAt: new Date(),
          success: true,
        });

        stageExec.status = 'completed';
        stageExec.completedAt = new Date();
        stageExec.metrics = {
          duration: Date.now() - startTime,
          attempts: attempt,
          itemsProcessed: typeof result === 'number' ? result : 0,
          itemsFailed: 0,
          errorRate: 0,
        };

        this.emit({ type: 'stage:completed', runId: run.id, stageId: stage.id });
        return;

      } catch (error) {
        const err = error instanceof Error ? error : new Error(String(error));
        
        // Record failed attempt
        stageExec.attempts.push({
          attemptNumber: attempt,
          startedAt: new Date(attemptStart),
          completedAt: new Date(),
          success: false,
          error: err.message,
        });

        // Determine if we should retry
        const isTimeout = err.message === 'Stage timeout';
        const shouldRetry = attempt < maxAttempts && this.config.healingPolicy.autoRetry;

        if (shouldRetry) {
          stageExec.status = 'retrying';
          this.emit({
            type: 'stage:retrying',
            runId: run.id,
            stageId: stage.id,
            attempt: attempt + 1,
          });

          // Try healing before retry
          if (this.config.healingPolicy.enabled) {
            const trigger: TriggerCondition = isTimeout
              ? { type: 'timeout' }
              : { type: 'error', pattern: err.message };

            await this.triggerHealing(run, stage.id, trigger);
          }

          // Wait before retry
          if (stage.retryPolicy) {
            const delay = stage.retryPolicy.initialDelay * 
              Math.pow(stage.retryPolicy.backoffMultiplier, attempt - 1);
            await this.delay(Math.min(delay, stage.retryPolicy.maxDelay));
          }

          continue;
        }

        // Stage failed
        stageExec.status = 'failed';
        stageExec.completedAt = new Date();
        stageExec.error = {
          code: isTimeout ? 'TIMEOUT' : 'EXECUTION_ERROR',
          message: err.message,
          recoverable: false,
          retryable: false,
        };
        stageExec.metrics = {
          duration: Date.now() - startTime,
          attempts: attempt,
          itemsProcessed: 0,
          itemsFailed: 1,
          errorRate: 1,
        };

        this.emit({
          type: 'stage:failed',
          runId: run.id,
          stageId: stage.id,
          error: stageExec.error,
        });

        return;
      }
    }
  }

  private async triggerHealing(
    run: PipelineRun,
    stageId: string,
    trigger: TriggerCondition
  ): Promise<HealingEvent> {
    const context: HealingContext = {
      runId: run.id,
      stageId,
    };

    const event = await this.autoHealer.heal(trigger, context);
    run.healingEvents.push(event);

    this.emit({ type: 'healing:triggered', runId: run.id, event });
    this.emit({ type: 'healing:completed', runId: run.id, event });

    return event;
  }

  private async rollbackPipeline(run: PipelineRun, _failedStageId: string): Promise<void> {
    // Find completed stages that need rollback (in reverse order)
    const completedStages = run.stages
      .filter(s => s.status === 'completed')
      .reverse();

    for (const stageExec of completedStages) {
      const stage = this.config.stages.find(s => s.id === stageExec.stageId);
      if (!stage?.rollbackHandler) continue;

      this.emit({ type: 'rollback:started', runId: run.id, stageId: stage.id });

      try {
        // In production, would call actual rollback handler
        await this.delay(100);
        stageExec.status = 'rolled-back';
      } catch (_error) {
        // Log but continue with other rollbacks
        console.error(`Rollback failed for stage: ${stage.id}`);
      }

      this.emit({ type: 'rollback:completed', runId: run.id, stageId: stage.id });
    }
  }

  private resolveExecutionOrder(): string[] {
    // Topological sort based on dependencies
    const stages = this.config.stages;
    const order: string[] = [];
    const visited = new Set<string>();
    const visiting = new Set<string>();

    const visit = (stageId: string) => {
      if (visited.has(stageId)) return;
      if (visiting.has(stageId)) {
        throw new Error(`Circular dependency detected involving stage: ${stageId}`);
      }

      visiting.add(stageId);

      const stage = stages.find(s => s.id === stageId);
      if (stage?.dependencies) {
        for (const dep of stage.dependencies) {
          visit(dep);
        }
      }

      visiting.delete(stageId);
      visited.add(stageId);
      order.push(stageId);
    };

    for (const stage of stages) {
      visit(stage.id);
    }

    return order;
  }

  private calculatePipelineMetrics(run: PipelineRun): PipelineMetrics {
    let totalRetries = 0;
    let stagesCompleted = 0;
    let stagesFailed = 0;
    let successfulHealings = 0;

    for (const stage of run.stages) {
      totalRetries += stage.attempts.length - 1;
      if (stage.status === 'completed') stagesCompleted++;
      if (stage.status === 'failed') stagesFailed++;
    }

    for (const event of run.healingEvents) {
      if (event.outcome === 'resolved') successfulHealings++;
    }

    const duration = run.completedAt 
      ? run.completedAt.getTime() - run.startedAt.getTime()
      : Date.now() - run.startedAt.getTime();

    return {
      duration,
      stagesCompleted,
      stagesFailed,
      totalRetries,
      healingEventsTriggered: run.healingEvents.length,
      successfulHealings,
      errorRate: run.stages.length > 0 ? stagesFailed / run.stages.length : 0,
      throughput: stagesCompleted / (duration / 1000),
    };
  }

  private createTimeout(ms: number): Promise<never> {
    return new Promise((_, reject) => {
      setTimeout(() => reject(new Error('Stage timeout')), ms);
    });
  }

  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  private emit(event: PipelineEvent): void {
    for (const handler of this.eventHandlers) {
      try {
        handler(event);
      } catch (e) {
        console.error('Event handler error:', e);
      }
    }
  }
}

// ============================================================================
// Stage Handler Type
// ============================================================================

export type StageHandler = (
  stage: StageConfig,
  run: PipelineRun
) => Promise<number | void>;

// ============================================================================
// Factory
// ============================================================================

export function createSelfHealingPipeline(config: PipelineConfig): SelfHealingPipeline {
  return new SelfHealingPipeline(config);
}
