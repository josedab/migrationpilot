/**
 * Capture Module
 * 
 * Shadow traffic capture and replay functionality
 */

export { ShadowTrafficCapture, type ShadowCaptureStatistics } from './shadow-capture.js';
export { 
  TrafficReplay, 
  type ReplayConfig, 
  type ReplayResult, 
  type ReplayBatchResult,
  type ReplaySummary,
  type DifferencePattern,
} from './traffic-replay.js';
