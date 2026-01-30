/**
 * Progressive Rollout Types
 * Types for canary deployments, feature flags, and traffic splitting
 */

export interface RolloutConfig {
  id: string;
  name: string;
  description: string;
  targetComponent: string;
  strategy: RolloutStrategy;
  stages: RolloutStage[];
  healthThresholds: HealthThresholds;
  autoRollback: AutoRollbackConfig;
  featureFlags: FeatureFlagConfig[];
  created: Date;
  status: RolloutStatus;
}

export type RolloutStrategy = 
  | 'canary'
  | 'blue-green'
  | 'rolling'
  | 'feature-flag'
  | 'shadow';

export type RolloutStatus = 
  | 'pending'
  | 'in-progress'
  | 'paused'
  | 'completed'
  | 'rolled-back'
  | 'failed';

export interface RolloutStage {
  id: string;
  name: string;
  percentage: number;
  duration: number; // minutes
  criteria: PromotionCriteria;
  status: StageStatus;
  startedAt?: Date;
  completedAt?: Date;
  metrics?: StageMetrics;
}

export type StageStatus = 
  | 'pending'
  | 'active'
  | 'completed'
  | 'failed'
  | 'skipped';

export interface PromotionCriteria {
  minDuration: number;
  maxErrorRate: number;
  minSuccessRate: number;
  latencyP99Max: number;
  customChecks?: CustomCheck[];
}

export interface CustomCheck {
  name: string;
  query: string;
  threshold: number;
  operator: 'gt' | 'gte' | 'lt' | 'lte' | 'eq';
}

export interface StageMetrics {
  requestCount: number;
  errorCount: number;
  errorRate: number;
  successRate: number;
  latencyP50: number;
  latencyP95: number;
  latencyP99: number;
  customMetrics: Record<string, number>;
}

export interface HealthThresholds {
  errorRateThreshold: number;
  latencyThreshold: number;
  availabilityThreshold: number;
  anomalyScoreThreshold: number;
}

export interface AutoRollbackConfig {
  enabled: boolean;
  triggerConditions: RollbackTrigger[];
  cooldownPeriod: number;
  notifyOnRollback: boolean;
  rollbackStrategy: 'immediate' | 'gradual';
}

export interface RollbackTrigger {
  type: 'error-rate' | 'latency' | 'availability' | 'custom-metric' | 'anomaly';
  threshold: number;
  window: number; // seconds
  operator: 'gt' | 'gte' | 'lt' | 'lte';
}

export interface FeatureFlagConfig {
  id: string;
  name: string;
  description: string;
  enabled: boolean;
  rules: FlagRule[];
  defaultValue: FlagValue;
  variants: FlagVariant[];
}

export type FlagValue = boolean | string | number | Record<string, unknown>;

export interface FlagRule {
  id: string;
  priority: number;
  conditions: FlagCondition[];
  variant: string;
}

export interface FlagCondition {
  attribute: string;
  operator: ConditionOperator;
  value: string | number | string[] | number[];
}

export type ConditionOperator = 
  | 'equals'
  | 'not-equals'
  | 'contains'
  | 'not-contains'
  | 'in'
  | 'not-in'
  | 'matches'
  | 'gt'
  | 'gte'
  | 'lt'
  | 'lte'
  | 'percentage';

export interface FlagVariant {
  id: string;
  name: string;
  value: FlagValue;
  weight: number;
}

export interface TrafficSplit {
  id: string;
  name: string;
  routes: TrafficRoute[];
  stickySession: boolean;
  hashKey?: string;
}

export interface TrafficRoute {
  id: string;
  target: string;
  weight: number;
  conditions?: RouteCondition[];
  metadata?: Record<string, string>;
}

export interface RouteCondition {
  type: 'header' | 'query' | 'cookie' | 'path' | 'user-agent';
  key: string;
  operator: 'equals' | 'contains' | 'matches';
  value: string;
}

export interface RolloutEvent {
  id: string;
  rolloutId: string;
  type: RolloutEventType;
  timestamp: Date;
  stage?: string;
  data: Record<string, unknown>;
  actor?: string;
}

export type RolloutEventType =
  | 'created'
  | 'started'
  | 'stage-promoted'
  | 'stage-completed'
  | 'paused'
  | 'resumed'
  | 'rollback-initiated'
  | 'rollback-completed'
  | 'completed'
  | 'failed'
  | 'health-check'
  | 'threshold-breach';

export interface RolloutResult {
  rolloutId: string;
  status: RolloutStatus;
  startedAt: Date;
  completedAt?: Date;
  finalStage?: string;
  totalDuration?: number;
  rollbackReason?: string;
  metrics: RolloutMetrics;
  events: RolloutEvent[];
}

export interface RolloutMetrics {
  stagesCompleted: number;
  totalStages: number;
  peakTrafficPercentage: number;
  totalRequests: number;
  totalErrors: number;
  overallErrorRate: number;
  avgLatencyP99: number;
  rollbackCount: number;
}

export interface HealthCheckResult {
  timestamp: Date;
  healthy: boolean;
  metrics: StageMetrics;
  violations: ThresholdViolation[];
}

export interface ThresholdViolation {
  metric: string;
  threshold: number;
  actual: number;
  severity: 'warning' | 'critical';
}

export interface DeploymentTarget {
  id: string;
  name: string;
  environment: string;
  version: string;
  instances: number;
  healthEndpoint: string;
  metricsEndpoint?: string;
}

export interface RolloutOrchestratorConfig {
  metricsProvider: MetricsProvider;
  healthCheckInterval: number;
  eventStore?: EventStore;
  notificationChannels?: NotificationChannel[];
}

export interface MetricsProvider {
  name: string;
  query(metric: string, labels: Record<string, string>, window: number): Promise<number>;
  queryRange(metric: string, labels: Record<string, string>, start: Date, end: Date): Promise<TimeSeriesData>;
}

export interface TimeSeriesData {
  metric: string;
  values: Array<{ timestamp: Date; value: number }>;
}

export interface EventStore {
  save(event: RolloutEvent): Promise<void>;
  getByRollout(rolloutId: string): Promise<RolloutEvent[]>;
  getByTimeRange(start: Date, end: Date): Promise<RolloutEvent[]>;
}

export interface NotificationChannel {
  type: 'slack' | 'email' | 'webhook' | 'pagerduty';
  config: Record<string, string>;
  notify(event: RolloutEvent): Promise<void>;
}
