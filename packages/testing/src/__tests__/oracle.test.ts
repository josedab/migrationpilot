/**
 * Tests for AI-Powered Test Oracle Module
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { BehavioralLearningEngine } from '../oracle/index.js';
import type { HistoricalExecution } from '../oracle/types.js';

describe('BehavioralLearningEngine', () => {
  let engine: BehavioralLearningEngine;
  let sampleExecutions: HistoricalExecution[];

  beforeEach(() => {
    // Constructor requires projectId
    engine = new BehavioralLearningEngine({ projectId: 'test-project' });
    
    // Create sample historical execution data (needs at least 50 by default)
    sampleExecutions = Array.from({ length: 60 }, (_, i) => ({
      id: `exec-${i}`,
      ruleId: 'interest-calc',
      ruleName: 'Interest Calculator',
      timestamp: new Date(Date.now() - i * 60000),
      inputs: {
        principal: 1000 + i * 100,
        rate: 0.05,
        years: 1 + (i % 5),
      },
      outputs: {
        interest: (1000 + i * 100) * 0.05 * (1 + (i % 5)),
        total: (1000 + i * 100) * (1 + 0.05 * (1 + (i % 5))),
      },
      metadata: {
        environment: 'production',
        source: 'mainframe',
        success: true,
        executionTimeMs: 10 + Math.random() * 50,
      },
    }));
  });

  describe('model training', () => {
    it('should train a model from historical executions', async () => {
      // trainModel signature: (data, name, ruleId?)
      const model = await engine.trainModel(sampleExecutions, 'Interest Calc Model', 'interest-calc');

      expect(model).toHaveProperty('id');
      expect(model.ruleId).toBe('interest-calc');
      expect(model.version).toBeGreaterThan(0);
    });

    it('should extract input-output signatures', async () => {
      const model = await engine.trainModel(sampleExecutions, 'Interest Calc Model');

      expect(model.inputSignature).toBeDefined();
      expect(model.outputSignature).toBeDefined();
      expect(Object.keys(model.inputSignature).length).toBeGreaterThan(0);
    });

    it('should discover invariants', async () => {
      const model = await engine.trainModel(sampleExecutions, 'Interest Calc Model');

      expect(model.invariants).toBeDefined();
      expect(model.invariants.length).toBeGreaterThan(0);
    });

    it('should reject insufficient data', async () => {
      // Default minimum is 50 samples
      await expect(engine.trainModel(sampleExecutions.slice(0, 10), 'Small Model')).rejects.toThrow('Insufficient training data');
    });

    it('should compute quality metrics', async () => {
      const model = await engine.trainModel(sampleExecutions, 'Interest Calc Model');

      expect(model.quality).toBeDefined();
      // Quality structure may vary - just verify it exists
    });
  });

  describe('incremental learning', () => {
    it('should update model with new executions', async () => {
      // Create additional data for update
      const additionalData = Array.from({ length: 60 }, (_, i) => ({
        id: `exec-new-${i}`,
        ruleId: 'interest-calc',
        ruleName: 'Interest Calculator',
        timestamp: new Date(),
        inputs: { principal: 5000 + i * 100, rate: 0.06, years: 2 },
        outputs: { interest: (5000 + i * 100) * 0.06 * 2, total: (5000 + i * 100) * 1.12 },
        metadata: { success: true },
      }));

      const model = await engine.trainModel(sampleExecutions, 'Interest Calc Model');
      const version1 = model.version;

      const updatedModel = await engine.updateModel(model.id, additionalData);

      expect(updatedModel.version).toBeGreaterThan(version1);
    });
  });

  describe('model retrieval', () => {
    it('should retrieve model by ID', async () => {
      const model = await engine.trainModel(sampleExecutions, 'Interest Calc Model');
      const retrieved = engine.getModel(model.id);

      expect(retrieved).not.toBeNull();
      expect(retrieved?.id).toBe(model.id);
    });

    it('should return null for non-existent model', () => {
      const model = engine.getModel('non-existent-id');
      expect(model).toBeNull();
    });
  });
});
