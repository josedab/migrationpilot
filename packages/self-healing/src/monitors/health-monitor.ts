/**
 * Health Monitor - Monitors pipeline and stage health
 */

import type {
  HealthCheck,
  HealthStatus,
  HealthCheckResult,
  Alert,
  AlertSeverity,
  MonitoringConfig,
} from '../types.js';

// ============================================================================
// Health Monitor
// ============================================================================

export class HealthMonitor {
  private config: MonitoringConfig;
  private healthStatuses: Map<string, HealthStatus> = new Map();
  private alertHistory: Alert[] = [];
  private eventHandlers: ((alert: Alert) => void)[] = [];

  constructor(config: MonitoringConfig) {
    this.config = config;
  }

  /**
   * Subscribe to alerts
   */
  onAlert(handler: (alert: Alert) => void): () => void {
    this.eventHandlers.push(handler);
    return () => {
      const idx = this.eventHandlers.indexOf(handler);
      if (idx > -1) this.eventHandlers.splice(idx, 1);
    };
  }

  /**
   * Run a health check
   */
  async runHealthCheck(
    stageId: string,
    check: HealthCheck
  ): Promise<HealthCheckResult> {
    const startTime = Date.now();
    let passed = false;
    let message: string | undefined;

    try {
      switch (check.type) {
        case 'http':
          passed = await this.checkHttp(check);
          break;
        case 'tcp':
          passed = await this.checkTcp(check);
          break;
        case 'database':
          passed = await this.checkDatabase(check);
          break;
        case 'metric':
          passed = await this.checkMetric(check);
          break;
        case 'custom':
          passed = true; // Custom checks would be implemented by consumers
          break;
      }
    } catch (error) {
      passed = false;
      message = error instanceof Error ? error.message : 'Unknown error';
    }

    const result: HealthCheckResult = {
      checkId: check.id,
      passed,
      timestamp: new Date(),
      duration: Date.now() - startTime,
      message,
    };

    this.updateHealthStatus(stageId, check, result);
    return result;
  }

  /**
   * Run all health checks for a stage
   */
  async runAllHealthChecks(
    stageId: string,
    checks: HealthCheck[]
  ): Promise<HealthStatus> {
    const results: HealthCheckResult[] = [];

    for (const check of checks) {
      const result = await this.runHealthCheck(stageId, check);
      results.push(result);
    }

    return this.getHealthStatus(stageId);
  }

  /**
   * Get current health status for a stage
   */
  getHealthStatus(stageId: string): HealthStatus {
    return this.healthStatuses.get(stageId) || {
      healthy: true,
      lastCheck: new Date(),
      consecutiveFailures: 0,
      consecutiveSuccesses: 0,
      checks: [],
    };
  }

  /**
   * Check if a stage is healthy
   */
  isHealthy(stageId: string): boolean {
    const status = this.healthStatuses.get(stageId);
    return status?.healthy ?? true;
  }

  /**
   * Evaluate alert thresholds for a metric
   */
  evaluateAlertThresholds(
    metric: string,
    value: number,
    runId?: string,
    stageId?: string
  ): Alert[] {
    const triggeredAlerts: Alert[] = [];

    for (const threshold of this.config.alertThresholds) {
      if (threshold.metric !== metric) continue;

      let triggered = false;
      switch (threshold.operator) {
        case 'gt':
          triggered = value > threshold.value;
          break;
        case 'lt':
          triggered = value < threshold.value;
          break;
        case 'eq':
          triggered = value === threshold.value;
          break;
        case 'gte':
          triggered = value >= threshold.value;
          break;
        case 'lte':
          triggered = value <= threshold.value;
          break;
      }

      if (triggered) {
        const alert: Alert = {
          id: `alert_${Date.now()}_${Math.random().toString(36).slice(2)}`,
          timestamp: new Date(),
          severity: threshold.severity,
          metric,
          value,
          threshold: threshold.value,
          message: `${metric} ${threshold.operator} ${threshold.value}: actual value is ${value}`,
          runId,
          stageId,
        };

        triggeredAlerts.push(alert);
        this.alertHistory.push(alert);
        this.emitAlert(alert);
      }
    }

    return triggeredAlerts;
  }

  /**
   * Get alert history
   */
  getAlertHistory(
    filter?: {
      severity?: AlertSeverity;
      metric?: string;
      since?: Date;
    }
  ): Alert[] {
    let alerts = [...this.alertHistory];

    if (filter?.severity) {
      alerts = alerts.filter(a => a.severity === filter.severity);
    }
    if (filter?.metric) {
      alerts = alerts.filter(a => a.metric === filter.metric);
    }
    if (filter?.since) {
      alerts = alerts.filter(a => a.timestamp >= filter.since!);
    }

    return alerts;
  }

  /**
   * Clear alert history
   */
  clearAlertHistory(): void {
    this.alertHistory = [];
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private async checkHttp(check: HealthCheck): Promise<boolean> {
    // Simulated HTTP check - in production would use fetch
    if (!check.endpoint) return false;
    
    // Simulate network delay
    await new Promise(resolve => setTimeout(resolve, 50));
    
    // Simulate success (in production would actually call the endpoint)
    return true;
  }

  private async checkTcp(check: HealthCheck): Promise<boolean> {
    // Simulated TCP check - in production would use net module
    if (!check.endpoint) return false;
    await new Promise(resolve => setTimeout(resolve, 30));
    return true;
  }

  private async checkDatabase(check: HealthCheck): Promise<boolean> {
    // Simulated database check - in production would run query
    if (!check.query) return false;
    await new Promise(resolve => setTimeout(resolve, 100));
    return true;
  }

  private async checkMetric(check: HealthCheck): Promise<boolean> {
    // Simulated metric check
    const currentValue = Math.random() * 100;
    if (check.expectedResult !== undefined) {
      return currentValue <= (check.expectedResult as number);
    }
    return true;
  }

  private updateHealthStatus(
    stageId: string,
    check: HealthCheck,
    result: HealthCheckResult
  ): void {
    let status = this.healthStatuses.get(stageId);
    
    if (!status) {
      status = {
        healthy: true,
        lastCheck: new Date(),
        consecutiveFailures: 0,
        consecutiveSuccesses: 0,
        checks: [],
      };
    }

    // Update check results
    const existingIdx = status.checks.findIndex(c => c.checkId === check.id);
    if (existingIdx >= 0) {
      status.checks[existingIdx] = result;
    } else {
      status.checks.push(result);
    }

    // Update counters
    if (result.passed) {
      status.consecutiveSuccesses++;
      status.consecutiveFailures = 0;
      
      if (status.consecutiveSuccesses >= check.successThreshold) {
        status.healthy = true;
      }
    } else {
      status.consecutiveFailures++;
      status.consecutiveSuccesses = 0;
      
      if (status.consecutiveFailures >= check.failureThreshold) {
        status.healthy = false;
      }
    }

    status.lastCheck = new Date();
    this.healthStatuses.set(stageId, status);
  }

  private emitAlert(alert: Alert): void {
    for (const handler of this.eventHandlers) {
      try {
        handler(alert);
      } catch (e) {
        console.error('Alert handler error:', e);
      }
    }
  }
}

// ============================================================================
// Metrics Collector
// ============================================================================

export class MetricsCollector {
  private metrics: Map<string, MetricSeries> = new Map();
  private config: MonitoringConfig;

  constructor(config: MonitoringConfig) {
    this.config = config;
  }

  /**
   * Record a metric value
   */
  record(name: string, value: number, tags?: Record<string, string>): void {
    if (!this.config.metricsEnabled) return;

    // Apply sampling
    if (this.config.samplingRate && Math.random() > this.config.samplingRate) {
      return;
    }

    const key = this.getMetricKey(name, tags);
    let series = this.metrics.get(key);
    
    if (!series) {
      series = {
        name,
        tags: tags || {},
        values: [],
      };
      this.metrics.set(key, series);
    }

    series.values.push({
      timestamp: Date.now(),
      value,
    });

    // Keep only last 1000 values
    if (series.values.length > 1000) {
      series.values = series.values.slice(-1000);
    }
  }

  /**
   * Record a timer
   */
  recordTimer(name: string, startTime: number, tags?: Record<string, string>): void {
    const duration = Date.now() - startTime;
    this.record(name, duration, tags);
  }

  /**
   * Get metric statistics
   */
  getStats(name: string, tags?: Record<string, string>): MetricStats | null {
    const key = this.getMetricKey(name, tags);
    const series = this.metrics.get(key);
    
    if (!series || series.values.length === 0) return null;

    const values = series.values.map(v => v.value);
    const sorted = [...values].sort((a, b) => a - b);

    return {
      count: values.length,
      sum: values.reduce((a, b) => a + b, 0),
      min: sorted[0]!,
      max: sorted[sorted.length - 1]!,
      avg: values.reduce((a, b) => a + b, 0) / values.length,
      p50: sorted[Math.floor(sorted.length * 0.5)]!,
      p95: sorted[Math.floor(sorted.length * 0.95)]!,
      p99: sorted[Math.floor(sorted.length * 0.99)]!,
    };
  }

  /**
   * Get current metric value
   */
  getCurrent(name: string, tags?: Record<string, string>): number | null {
    const key = this.getMetricKey(name, tags);
    const series = this.metrics.get(key);
    
    if (!series || series.values.length === 0) return null;
    return series.values[series.values.length - 1]!.value;
  }

  /**
   * Get all metrics
   */
  getAllMetrics(): Map<string, MetricSeries> {
    return new Map(this.metrics);
  }

  /**
   * Clear metrics
   */
  clear(): void {
    this.metrics.clear();
  }

  private getMetricKey(name: string, tags?: Record<string, string>): string {
    if (!tags || Object.keys(tags).length === 0) return name;
    const tagStr = Object.entries(tags)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}=${v}`)
      .join(',');
    return `${name}{${tagStr}}`;
  }
}

// ============================================================================
// Supporting Types
// ============================================================================

export interface MetricSeries {
  name: string;
  tags: Record<string, string>;
  values: MetricValue[];
}

export interface MetricValue {
  timestamp: number;
  value: number;
}

export interface MetricStats {
  count: number;
  sum: number;
  min: number;
  max: number;
  avg: number;
  p50: number;
  p95: number;
  p99: number;
}

// ============================================================================
// Factory
// ============================================================================

export function createHealthMonitor(config: MonitoringConfig): HealthMonitor {
  return new HealthMonitor(config);
}

export function createMetricsCollector(config: MonitoringConfig): MetricsCollector {
  return new MetricsCollector(config);
}
