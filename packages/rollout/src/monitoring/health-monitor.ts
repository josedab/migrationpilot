/**
 * Rollout Health Monitor
 * Monitors deployment health and triggers alerts/rollbacks
 */

import {
  HealthThresholds,
  MetricsProvider,
  StageMetrics,
  ThresholdViolation,
  HealthCheckResult,
} from '../types';

export class HealthMonitor {
  private readonly metricsProvider: MetricsProvider;
  private readonly thresholds: HealthThresholds;
  private activeMonitors: Map<string, MonitorState> = new Map();
  private listeners: HealthEventListener[] = [];

  constructor(metricsProvider: MetricsProvider, thresholds: HealthThresholds) {
    this.metricsProvider = metricsProvider;
    this.thresholds = thresholds;
  }

  startMonitoring(config: MonitorConfig): string {
    const monitorId = `monitor_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
    
    const state: MonitorState = {
      id: monitorId,
      config,
      status: 'active',
      startedAt: new Date(),
      lastCheck: null,
      checkCount: 0,
      violationHistory: [],
      consecutiveViolations: 0,
    };

    this.activeMonitors.set(monitorId, state);

    // Start periodic checks
    const intervalId = setInterval(async () => {
      await this.performCheck(monitorId);
    }, config.checkInterval * 1000);

    state.intervalId = intervalId;

    return monitorId;
  }

  stopMonitoring(monitorId: string): void {
    const state = this.activeMonitors.get(monitorId);
    if (!state) return;

    if (state.intervalId) {
      clearInterval(state.intervalId);
    }

    state.status = 'stopped';
    this.activeMonitors.delete(monitorId);
  }

  async checkHealth(component: string): Promise<HealthCheckResult> {
    const labels = { component };
    const window = 60;

    try {
      const [requestCount, errorCount, latencyP50, latencyP95, latencyP99] = await Promise.all([
        this.metricsProvider.query('request_count', labels, window),
        this.metricsProvider.query('error_count', labels, window),
        this.metricsProvider.query('latency_p50', labels, window),
        this.metricsProvider.query('latency_p95', labels, window),
        this.metricsProvider.query('latency_p99', labels, window),
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

      const violations = this.evaluateThresholds(metrics);

      return {
        timestamp: new Date(),
        healthy: violations.length === 0,
        metrics,
        violations,
      };
    } catch (error) {
      return {
        timestamp: new Date(),
        healthy: false,
        metrics: this.emptyMetrics(),
        violations: [{
          metric: 'metrics_collection',
          threshold: 0,
          actual: -1,
          severity: 'critical',
        }],
      };
    }
  }

  onHealthEvent(listener: HealthEventListener): void {
    this.listeners.push(listener);
  }

  removeListener(listener: HealthEventListener): void {
    const index = this.listeners.indexOf(listener);
    if (index !== -1) {
      this.listeners.splice(index, 1);
    }
  }

  getMonitorState(monitorId: string): MonitorState | undefined {
    return this.activeMonitors.get(monitorId);
  }

  listActiveMonitors(): MonitorState[] {
    return Array.from(this.activeMonitors.values()).filter(m => m.status === 'active');
  }

  private async performCheck(monitorId: string): Promise<void> {
    const state = this.activeMonitors.get(monitorId);
    if (!state || state.status !== 'active') return;

    const result = await this.checkHealth(state.config.component);
    state.lastCheck = result;
    state.checkCount++;

    if (!result.healthy) {
      state.consecutiveViolations++;
      state.violationHistory.push({
        timestamp: result.timestamp,
        violations: result.violations,
      });

      // Keep only last 100 violations
      if (state.violationHistory.length > 100) {
        state.violationHistory = state.violationHistory.slice(-100);
      }

      // Check for alert threshold
      if (state.consecutiveViolations >= state.config.alertAfterFailures) {
        await this.emitEvent({
          type: 'alert',
          monitorId,
          component: state.config.component,
          timestamp: new Date(),
          violations: result.violations,
          consecutiveFailures: state.consecutiveViolations,
        });
      }

      // Check for rollback threshold
      if (state.config.rollbackAfterFailures && 
          state.consecutiveViolations >= state.config.rollbackAfterFailures) {
        await this.emitEvent({
          type: 'rollback-recommended',
          monitorId,
          component: state.config.component,
          timestamp: new Date(),
          violations: result.violations,
          consecutiveFailures: state.consecutiveViolations,
        });
      }
    } else {
      // Reset consecutive violations on healthy check
      if (state.consecutiveViolations > 0) {
        await this.emitEvent({
          type: 'recovered',
          monitorId,
          component: state.config.component,
          timestamp: new Date(),
          violations: [],
          consecutiveFailures: 0,
        });
      }
      state.consecutiveViolations = 0;
    }

    await this.emitEvent({
      type: 'health-check',
      monitorId,
      component: state.config.component,
      timestamp: new Date(),
      violations: result.violations,
      consecutiveFailures: state.consecutiveViolations,
      metrics: result.metrics,
    });
  }

  private evaluateThresholds(metrics: StageMetrics): ThresholdViolation[] {
    const violations: ThresholdViolation[] = [];

    if (metrics.errorRate > this.thresholds.errorRateThreshold) {
      violations.push({
        metric: 'error_rate',
        threshold: this.thresholds.errorRateThreshold,
        actual: metrics.errorRate,
        severity: metrics.errorRate > this.thresholds.errorRateThreshold * 2 ? 'critical' : 'warning',
      });
    }

    if (metrics.latencyP99 > this.thresholds.latencyThreshold) {
      violations.push({
        metric: 'latency_p99',
        threshold: this.thresholds.latencyThreshold,
        actual: metrics.latencyP99,
        severity: metrics.latencyP99 > this.thresholds.latencyThreshold * 2 ? 'critical' : 'warning',
      });
    }

    const availability = metrics.successRate;
    if (availability < this.thresholds.availabilityThreshold) {
      violations.push({
        metric: 'availability',
        threshold: this.thresholds.availabilityThreshold,
        actual: availability,
        severity: availability < this.thresholds.availabilityThreshold * 0.9 ? 'critical' : 'warning',
      });
    }

    return violations;
  }

  private emptyMetrics(): StageMetrics {
    return {
      requestCount: 0,
      errorCount: 0,
      errorRate: 0,
      successRate: 1,
      latencyP50: 0,
      latencyP95: 0,
      latencyP99: 0,
      customMetrics: {},
    };
  }

  private async emitEvent(event: HealthEvent): Promise<void> {
    for (const listener of this.listeners) {
      try {
        await listener(event);
      } catch (error) {
        console.error('Health event listener error:', error);
      }
    }
  }
}

export interface MonitorConfig {
  component: string;
  checkInterval: number; // seconds
  alertAfterFailures: number;
  rollbackAfterFailures?: number;
  customThresholds?: Partial<HealthThresholds>;
}

export interface MonitorState {
  id: string;
  config: MonitorConfig;
  status: 'active' | 'paused' | 'stopped';
  startedAt: Date;
  lastCheck: HealthCheckResult | null;
  checkCount: number;
  violationHistory: ViolationRecord[];
  consecutiveViolations: number;
  intervalId?: NodeJS.Timeout;
}

interface ViolationRecord {
  timestamp: Date;
  violations: ThresholdViolation[];
}

export interface HealthEvent {
  type: 'health-check' | 'alert' | 'rollback-recommended' | 'recovered';
  monitorId: string;
  component: string;
  timestamp: Date;
  violations: ThresholdViolation[];
  consecutiveFailures: number;
  metrics?: StageMetrics;
}

export type HealthEventListener = (event: HealthEvent) => Promise<void> | void;

export function createHealthMonitor(
  metricsProvider: MetricsProvider,
  thresholds: HealthThresholds
): HealthMonitor {
  return new HealthMonitor(metricsProvider, thresholds);
}

// Default threshold presets
export const ThresholdPresets = {
  strict: {
    errorRateThreshold: 0.001, // 0.1%
    latencyThreshold: 100, // 100ms
    availabilityThreshold: 0.999, // 99.9%
    anomalyScoreThreshold: 0.5,
  },
  standard: {
    errorRateThreshold: 0.01, // 1%
    latencyThreshold: 500, // 500ms
    availabilityThreshold: 0.99, // 99%
    anomalyScoreThreshold: 0.7,
  },
  relaxed: {
    errorRateThreshold: 0.05, // 5%
    latencyThreshold: 2000, // 2s
    availabilityThreshold: 0.95, // 95%
    anomalyScoreThreshold: 0.9,
  },
};
