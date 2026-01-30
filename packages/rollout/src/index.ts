/**
 * @migrationpilot/rollout
 * Progressive rollout orchestrator for safe migration deployments
 */

// Types
export * from './types';

// Orchestration
export { RolloutOrchestrator, createRolloutOrchestrator } from './orchestration';

// Feature Flags
export {
  FeatureFlagManager,
  EvaluationContext,
  createFeatureFlagManager,
  FlagTemplates,
} from './flags';

// Traffic Routing
export {
  TrafficRouter,
  RoutingRequest,
  RoutingResult,
  TrafficStats,
  createTrafficRouter,
  SplitTemplates,
} from './traffic';

// Health Monitoring
export {
  HealthMonitor,
  MonitorConfig,
  MonitorState,
  HealthEvent,
  HealthEventListener,
  createHealthMonitor,
  ThresholdPresets,
} from './monitoring';
