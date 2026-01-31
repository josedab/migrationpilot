/**
 * Auto Healer - Automatically applies healing actions
 */

import type {
  HealingStrategy,
  HealingAction,
  HealingEvent,
  HealingActionResult,
  HealingOutcome,
  TriggerCondition,
} from '../types.js';

// ============================================================================
// Auto Healer
// ============================================================================

export class AutoHealer {
  private strategies: HealingStrategy[];
  private cooldowns: Map<string, number> = new Map();
  private _maxAttempts: number;
  private _globalCooldown: number;

  constructor(
    strategies: HealingStrategy[],
    maxAttempts: number = 3,
    globalCooldown: number = 30000
  ) {
    this.strategies = strategies.sort((a, b) => b.priority - a.priority);
    this._maxAttempts = maxAttempts;
    this._globalCooldown = globalCooldown;
  }

  /**
   * Attempt to heal based on a triggered condition
   */
  async heal(
    trigger: TriggerCondition,
    context: HealingContext
  ): Promise<HealingEvent> {
    const startTime = Date.now();
    const strategy = this.findMatchingStrategy(trigger);

    if (!strategy) {
      return this.createFailedEvent(trigger, startTime, 'No matching strategy found');
    }

    // Check cooldown
    if (this.isInCooldown(strategy.id)) {
      return this.createFailedEvent(trigger, startTime, 'Strategy in cooldown');
    }

    // Execute healing actions
    const actionResults: HealingActionResult[] = [];
    let outcome: HealingOutcome = 'resolved';

    for (const action of strategy.actions) {
      const result = await this.executeAction(action, context);
      actionResults.push(result);

      if (!result.success) {
        if (action.rollbackOnFailure && result.rollbackPerformed) {
          outcome = 'partially-resolved';
        } else {
          outcome = 'failed';
          break;
        }
      }
    }

    // Set cooldown
    this.setCooldown(strategy.id, strategy.cooldown);

    const event: HealingEvent = {
      id: `heal_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      timestamp: new Date(),
      trigger,
      strategyId: strategy.id,
      actions: actionResults,
      outcome,
      duration: Date.now() - startTime,
    };

    return event;
  }

  /**
   * Check if any strategy matches the trigger
   */
  hasMatchingStrategy(trigger: TriggerCondition): boolean {
    return this.findMatchingStrategy(trigger) !== null;
  }

  /**
   * Get all registered strategies
   */
  getStrategies(): HealingStrategy[] {
    return [...this.strategies];
  }

  /**
   * Add a new strategy
   */
  addStrategy(strategy: HealingStrategy): void {
    this.strategies.push(strategy);
    this.strategies.sort((a, b) => b.priority - a.priority);
  }

  /**
   * Remove a strategy
   */
  removeStrategy(strategyId: string): boolean {
    const idx = this.strategies.findIndex(s => s.id === strategyId);
    if (idx >= 0) {
      this.strategies.splice(idx, 1);
      return true;
    }
    return false;
  }

  /**
   * Get max healing attempts
   */
  getMaxAttempts(): number {
    return this._maxAttempts;
  }

  /**
   * Get global cooldown period
   */
  getGlobalCooldown(): number {
    return this._globalCooldown;
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private findMatchingStrategy(trigger: TriggerCondition): HealingStrategy | null {
    for (const strategy of this.strategies) {
      const matches = strategy.triggerConditions.some(tc =>
        this.conditionMatches(tc, trigger)
      );
      if (matches) return strategy;
    }
    return null;
  }

  private conditionMatches(
    strategyCondition: TriggerCondition,
    trigger: TriggerCondition
  ): boolean {
    if (strategyCondition.type !== trigger.type) return false;

    switch (trigger.type) {
      case 'error':
        if (strategyCondition.pattern && trigger.pattern) {
          const regex = new RegExp(strategyCondition.pattern, 'i');
          return regex.test(trigger.pattern);
        }
        return true;

      case 'metric':
        return strategyCondition.metric === trigger.metric;

      case 'health-check':
        return true;

      case 'timeout':
        return true;

      case 'anomaly':
        return true;

      default:
        return false;
    }
  }

  private async executeAction(
    action: HealingAction,
    context: HealingContext
  ): Promise<HealingActionResult> {
    const startTime = Date.now();
    let success = false;
    let error: string | undefined;
    let rollbackPerformed = false;

    try {
      switch (action.type) {
        case 'retry':
          success = await this.executeRetry(context);
          break;
        case 'rollback':
          success = await this.executeRollback(context);
          rollbackPerformed = success;
          break;
        case 'restart':
          success = await this.executeRestart(context);
          break;
        case 'scale':
          success = await this.executeScale(action.params, context);
          break;
        case 'failover':
          success = await this.executeFailover(context);
          break;
        case 'circuit-break':
          success = await this.executeCircuitBreak(context);
          break;
        case 'throttle':
          success = await this.executeThrottle(action.params, context);
          break;
        case 'notify':
          success = await this.executeNotify(action.params, context);
          break;
        case 'custom':
          success = await this.executeCustom(action.params, context);
          break;
      }
    } catch (e) {
      success = false;
      error = e instanceof Error ? e.message : 'Unknown error';
    }

    return {
      action,
      success,
      startedAt: new Date(startTime),
      completedAt: new Date(),
      error,
      rollbackPerformed,
    };
  }

  private async executeRetry(context: HealingContext): Promise<boolean> {
    // In production, would re-execute the failed stage
    console.log(`Retrying stage: ${context.stageId}`);
    await this.delay(100);
    return true;
  }

  private async executeRollback(context: HealingContext): Promise<boolean> {
    // In production, would invoke rollback handler
    console.log(`Rolling back stage: ${context.stageId}`);
    await this.delay(200);
    return true;
  }

  private async executeRestart(context: HealingContext): Promise<boolean> {
    console.log(`Restarting stage: ${context.stageId}`);
    await this.delay(150);
    return true;
  }

  private async executeScale(
    params: Record<string, unknown> | undefined,
    _context: HealingContext
  ): Promise<boolean> {
    const factor = params?.factor ?? 2;
    console.log(`Scaling by factor: ${factor}`);
    await this.delay(100);
    return true;
  }

  private async executeFailover(context: HealingContext): Promise<boolean> {
    console.log(`Failing over stage: ${context.stageId}`);
    await this.delay(300);
    return true;
  }

  private async executeCircuitBreak(context: HealingContext): Promise<boolean> {
    console.log(`Circuit breaking stage: ${context.stageId}`);
    await this.delay(50);
    return true;
  }

  private async executeThrottle(
    params: Record<string, unknown> | undefined,
    _context: HealingContext
  ): Promise<boolean> {
    const rate = params?.rate ?? 0.5;
    console.log(`Throttling to ${rate}x rate`);
    await this.delay(50);
    return true;
  }

  private async executeNotify(
    params: Record<string, unknown> | undefined,
    context: HealingContext
  ): Promise<boolean> {
    const channel = params?.channel ?? 'default';
    console.log(`Notifying channel ${channel} for stage: ${context.stageId}`);
    await this.delay(50);
    return true;
  }

  private async executeCustom(
    params: Record<string, unknown> | undefined,
    _context: HealingContext
  ): Promise<boolean> {
    // Custom actions would be implemented by consumers
    console.log('Executing custom healing action', params);
    await this.delay(100);
    return true;
  }

  private isInCooldown(strategyId: string): boolean {
    const cooldownEnd = this.cooldowns.get(strategyId);
    if (!cooldownEnd) return false;
    return Date.now() < cooldownEnd;
  }

  private setCooldown(strategyId: string, duration: number): void {
    this.cooldowns.set(strategyId, Date.now() + duration);
  }

  private createFailedEvent(
    trigger: TriggerCondition,
    startTime: number,
    reason: string
  ): HealingEvent {
    return {
      id: `heal_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      timestamp: new Date(),
      trigger,
      strategyId: 'none',
      actions: [{
        action: { type: 'notify' },
        success: false,
        startedAt: new Date(startTime),
        completedAt: new Date(),
        error: reason,
      }],
      outcome: 'failed',
      duration: Date.now() - startTime,
    };
  }

  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// ============================================================================
// Healing Context
// ============================================================================

export interface HealingContext {
  runId: string;
  stageId: string;
  error?: Error;
  metrics?: Record<string, number>;
  state?: Record<string, unknown>;
}

// ============================================================================
// Factory
// ============================================================================

export function createAutoHealer(
  strategies: HealingStrategy[],
  maxAttempts?: number,
  globalCooldown?: number
): AutoHealer {
  return new AutoHealer(strategies, maxAttempts, globalCooldown);
}
