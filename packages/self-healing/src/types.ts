/**
 * Self-Healing Pipeline Types
 */

// ============================================================================
// Core Types
// ============================================================================

export interface PipelineConfig {
  name: string;
  projectId: string;
  stages: StageConfig[];
  healingPolicy: HealingPolicy;
  monitoringConfig: MonitoringConfig;
  notificationConfig?: NotificationConfig;
}

export interface StageConfig {
  id: string;
  name: string;
  type: StageType;
  retryPolicy?: RetryPolicy;
  timeout?: number;
  dependencies?: string[];
  healthChecks?: HealthCheck[];
  rollbackHandler?: string;
}

export type StageType = 
  | 'extract'
  | 'transform'
  | 'validate'
  | 'migrate'
  | 'verify'
  | 'deploy'
  | 'rollback'
  | 'custom';

export interface RetryPolicy {
  maxRetries: number;
  initialDelay: number;
  maxDelay: number;
  backoffMultiplier: number;
  retryableErrors?: string[];
}

export interface HealthCheck {
  id: string;
  name: string;
  type: HealthCheckType;
  endpoint?: string;
  query?: string;
  expectedResult?: unknown;
  interval: number;
  timeout: number;
  failureThreshold: number;
  successThreshold: number;
}

export type HealthCheckType = 
  | 'http'
  | 'tcp'
  | 'database'
  | 'metric'
  | 'custom';

// ============================================================================
// Monitoring Types
// ============================================================================

export interface MonitoringConfig {
  metricsEnabled: boolean;
  loggingLevel: 'debug' | 'info' | 'warn' | 'error';
  alertThresholds: AlertThreshold[];
  samplingRate?: number;
}

export interface AlertThreshold {
  metric: string;
  operator: 'gt' | 'lt' | 'eq' | 'gte' | 'lte';
  value: number;
  duration?: number;
  severity: AlertSeverity;
}

export type AlertSeverity = 'info' | 'warning' | 'error' | 'critical';

export interface NotificationConfig {
  channels: NotificationChannel[];
  escalationPolicy?: EscalationPolicy;
}

export interface NotificationChannel {
  type: 'email' | 'slack' | 'webhook' | 'pagerduty';
  config: Record<string, unknown>;
  severities: AlertSeverity[];
}

export interface EscalationPolicy {
  levels: EscalationLevel[];
}

export interface EscalationLevel {
  waitMinutes: number;
  notifyChannels: string[];
}

// ============================================================================
// Healing Types
// ============================================================================

export interface HealingPolicy {
  enabled: boolean;
  autoRollback: boolean;
  autoRetry: boolean;
  autoScale: boolean;
  healingStrategies: HealingStrategy[];
  maxHealingAttempts: number;
  cooldownPeriod: number;
}

export interface HealingStrategy {
  id: string;
  name: string;
  triggerConditions: TriggerCondition[];
  actions: HealingAction[];
  priority: number;
  cooldown: number;
}

export interface TriggerCondition {
  type: 'error' | 'metric' | 'health-check' | 'timeout' | 'anomaly';
  pattern?: string;
  metric?: string;
  threshold?: number;
  operator?: 'gt' | 'lt' | 'eq';
}

export interface HealingAction {
  type: HealingActionType;
  params?: Record<string, unknown>;
  timeout?: number;
  rollbackOnFailure?: boolean;
}

export type HealingActionType =
  | 'retry'
  | 'rollback'
  | 'restart'
  | 'scale'
  | 'failover'
  | 'circuit-break'
  | 'throttle'
  | 'notify'
  | 'custom';

// ============================================================================
// Pipeline Execution Types
// ============================================================================

export interface PipelineRun {
  id: string;
  pipelineId: string;
  status: PipelineStatus;
  stages: StageExecution[];
  startedAt: Date;
  completedAt?: Date;
  healingEvents: HealingEvent[];
  metrics: PipelineMetrics;
  error?: PipelineError;
}

export type PipelineStatus = 
  | 'pending'
  | 'running'
  | 'healing'
  | 'completed'
  | 'failed'
  | 'rolled-back'
  | 'cancelled';

export interface StageExecution {
  stageId: string;
  status: StageStatus;
  startedAt?: Date;
  completedAt?: Date;
  attempts: AttemptRecord[];
  healthStatus: HealthStatus;
  metrics: StageMetrics;
  error?: StageError;
}

export type StageStatus = 
  | 'pending'
  | 'running'
  | 'retrying'
  | 'healing'
  | 'completed'
  | 'failed'
  | 'skipped'
  | 'rolled-back';

export interface AttemptRecord {
  attemptNumber: number;
  startedAt: Date;
  completedAt: Date;
  success: boolean;
  error?: string;
  healingApplied?: string;
}

export interface HealthStatus {
  healthy: boolean;
  lastCheck: Date;
  consecutiveFailures: number;
  consecutiveSuccesses: number;
  checks: HealthCheckResult[];
}

export interface HealthCheckResult {
  checkId: string;
  passed: boolean;
  timestamp: Date;
  duration: number;
  message?: string;
}

// ============================================================================
// Metrics Types
// ============================================================================

export interface PipelineMetrics {
  duration: number;
  stagesCompleted: number;
  stagesFailed: number;
  totalRetries: number;
  healingEventsTriggered: number;
  successfulHealings: number;
  dataProcessed?: number;
  errorRate: number;
  throughput: number;
}

export interface StageMetrics {
  duration: number;
  attempts: number;
  itemsProcessed: number;
  itemsFailed: number;
  bytesProcessed?: number;
  errorRate: number;
}

// ============================================================================
// Error Types
// ============================================================================

export interface PipelineError {
  code: string;
  message: string;
  stageId?: string;
  recoverable: boolean;
  details?: Record<string, unknown>;
  stack?: string;
}

export interface StageError {
  code: string;
  message: string;
  recoverable: boolean;
  retryable: boolean;
  details?: Record<string, unknown>;
}

// ============================================================================
// Healing Event Types
// ============================================================================

export interface HealingEvent {
  id: string;
  timestamp: Date;
  trigger: TriggerCondition;
  strategyId: string;
  actions: HealingActionResult[];
  outcome: HealingOutcome;
  duration: number;
}

export interface HealingActionResult {
  action: HealingAction;
  success: boolean;
  startedAt: Date;
  completedAt: Date;
  error?: string;
  rollbackPerformed?: boolean;
}

export type HealingOutcome = 
  | 'resolved'
  | 'partially-resolved'
  | 'failed'
  | 'escalated';

// ============================================================================
// Event Types
// ============================================================================

export type PipelineEvent =
  | { type: 'pipeline:started'; run: PipelineRun }
  | { type: 'pipeline:completed'; run: PipelineRun }
  | { type: 'pipeline:failed'; run: PipelineRun; error: PipelineError }
  | { type: 'stage:started'; runId: string; stageId: string }
  | { type: 'stage:completed'; runId: string; stageId: string }
  | { type: 'stage:failed'; runId: string; stageId: string; error: StageError }
  | { type: 'stage:retrying'; runId: string; stageId: string; attempt: number }
  | { type: 'healing:triggered'; runId: string; event: HealingEvent }
  | { type: 'healing:completed'; runId: string; event: HealingEvent }
  | { type: 'health:degraded'; runId: string; stageId: string; status: HealthStatus }
  | { type: 'health:recovered'; runId: string; stageId: string; status: HealthStatus }
  | { type: 'alert:triggered'; alert: Alert }
  | { type: 'rollback:started'; runId: string; stageId: string }
  | { type: 'rollback:completed'; runId: string; stageId: string };

export interface Alert {
  id: string;
  timestamp: Date;
  severity: AlertSeverity;
  metric: string;
  value: number;
  threshold: number;
  message: string;
  runId?: string;
  stageId?: string;
}
