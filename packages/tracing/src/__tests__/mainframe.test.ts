/**
 * Tests for Live Execution Tracing Module (Mainframe)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  MainframeTraceCapture,
  TraceNormalizer,
  BehavioralInferenceEngine,
  SMEValidationWorkflow,
} from '../mainframe/index.js';
import type {
  MainframeTraceConfig,
  NormalizedTrace,
  InferredRule,
} from '../mainframe/types.js';

// Helper to create test config
function createTestConfig(): MainframeTraceConfig {
  return {
    projectId: 'test-project',
    platform: 'zos',
    traceSource: 'custom_agent',
    connection: {
      host: 'localhost',
      port: 3270,
      protocol: 'agent',
      agentEndpoint: 'http://localhost:8080',
      connectionTimeoutMs: 30000,
      readTimeoutMs: 60000,
    },
    captureSettings: {
      captureCICS: true,
      captureDB2: true,
      captureIMS: false,
      captureBatch: true,
      captureVSAM: true,
      captureStatements: true,
      captureVariables: true,
      captureWorkingStorage: false,
      captureLinkageSection: false,
      samplingMode: 'all',
      samplingRate: 1.0,
    },
    security: {
      authType: 'api_key',
      encryptTraces: false,
      maskSensitiveData: true,
      auditLevel: 'standard',
      sensitivePatterns: [],
      retentionDays: 30,
    },
    performance: {
      maxCpuPercent: 5,
      maxMemoryMB: 256,
      maxTraceBufferMB: 100,
      batchSize: 100,
      flushIntervalMs: 1000,
      maxEventsPerSecond: 10000,
      backoffMultiplier: 2,
      maxBackoffMs: 30000,
      compressionEnabled: false,
      compressionLevel: 6,
    },
  };
}

describe('MainframeTraceCapture', () => {
  let capture: MainframeTraceCapture;

  beforeEach(() => {
    capture = new MainframeTraceCapture(createTestConfig());
  });

  describe('configuration', () => {
    it('should initialize with provided config', () => {
      expect(capture.config).toBeDefined();
      expect(capture.config.platform).toBe('zos');
    });

    it('should have capture settings', () => {
      expect(capture.config.captureSettings.captureCICS).toBe(true);
      expect(capture.config.captureSettings.captureDB2).toBe(true);
    });
  });

  describe('connection lifecycle', () => {
    it('should report disconnected state initially', () => {
      expect(capture.isConnected()).toBe(false);
    });
  });

  describe('listener management', () => {
    it('should allow registering trace listeners', () => {
      const listener = () => {};
      capture.onTrace(listener);
      // No error means success - listeners are internal
    });

    it('should allow unregistering trace listeners', () => {
      const listener = () => {};
      capture.onTrace(listener);
      capture.offTrace(listener);
      // No error means success
    });
  });
});

describe('TraceNormalizer', () => {
  let normalizer: TraceNormalizer;

  beforeEach(() => {
    normalizer = new TraceNormalizer();
  });

  describe('initialization', () => {
    it('should initialize without config', () => {
      expect(normalizer).toBeDefined();
    });
  });
});

describe('BehavioralInferenceEngine', () => {
  let engine: BehavioralInferenceEngine;

  beforeEach(() => {
    engine = new BehavioralInferenceEngine({
      minConfidenceThreshold: 0.6,
      maxRulesPerProgram: 100,
      enableStatisticalInference: true,
      enablePatternMatching: true,
      enableDataFlowAnalysis: true,
    });
  });

  describe('configuration', () => {
    it('should initialize with provided config', () => {
      expect(engine.config).toBeDefined();
      expect(engine.config.minConfidenceThreshold).toBe(0.6);
    });
  });

  describe('rule inference', () => {
    it('should infer rules from normalized traces', async () => {
      const traces: NormalizedTrace[] = Array.from({ length: 10 }, (_, i) => ({
        id: `trace-${i}`,
        sourceTraceId: `raw-${i}`,
        projectId: 'test-project',
        transactionId: `TXN00${i}`,
        programName: 'CALCINT',
        events: [
          {
            id: `e1-${i}`,
            type: 'db2',
            operation: 'SELECT',
            timestamp: new Date(),
            details: { table: 'ACCOUNTS', columns: ['BALANCE'] },
            duration: 10,
          },
          {
            id: `e2-${i}`,
            type: 'cics',
            operation: 'SEND',
            timestamp: new Date(),
            details: { map: 'RESULT' },
            duration: 5,
          },
        ],
        dataFlows: [],
        executionPath: ['DB2_SELECT', 'CICS_SEND'],
        branchingPoints: [],
        businessContext: { domain: 'banking', subdomain: 'interest' },
        normalizedAt: new Date(),
      }));

      const rules = await engine.inferRules(traces);

      expect(rules).toBeDefined();
      expect(Array.isArray(rules)).toBe(true);
    });
  });
});

describe('SMEValidationWorkflow', () => {
  let workflow: SMEValidationWorkflow;

  beforeEach(() => {
    workflow = new SMEValidationWorkflow({
      requiredApprovals: 2,
      validationTimeoutDays: 7,
      escalationThreshold: 3,
      notificationEnabled: true,
    });
  });

  describe('initialization', () => {
    it('should initialize with provided config', () => {
      expect(workflow).toBeDefined();
    });
  });

  // Note: createSession requires InferredBusinessRule with evidence, 
  // which requires running the full inference engine. Session management 
  // tests are deferred to integration tests.
});
