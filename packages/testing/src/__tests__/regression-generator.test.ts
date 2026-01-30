/**
 * Tests for Regression Test Generator
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { 
  RegressionTestGenerator, 
  type ProductionLogEntry, 
  type GoldenDataset 
} from '../regression-generator.js';
import type { TestBusinessRule } from '../types.js';

describe('RegressionTestGenerator', () => {
  let generator: RegressionTestGenerator;

  beforeEach(() => {
    generator = new RegressionTestGenerator();
  });

  describe('generateFromLogs', () => {
    const sampleLogs: ProductionLogEntry[] = [
      {
        timestamp: '2024-01-15T10:00:00Z',
        transactionId: 'TXN001',
        programName: 'CALCINT',
        inputs: { principal: 10000, rate: 0.05 },
        outputs: { interest: 500 },
        executionTimeMs: 50,
      },
      {
        timestamp: '2024-01-15T10:01:00Z',
        transactionId: 'TXN002',
        programName: 'CALCINT',
        inputs: { principal: 20000, rate: 0.03 },
        outputs: { interest: 600 },
        executionTimeMs: 45,
      },
      {
        timestamp: '2024-01-15T10:02:00Z',
        transactionId: 'TXN003',
        programName: 'CALCINT',
        inputs: { principal: 0, rate: 0.05 },
        outputs: { interest: 0 },
        executionTimeMs: 30,
      },
    ];

    const sampleRules: TestBusinessRule[] = [
      {
        id: 'rule-1',
        name: 'Interest Calculation',
        description: 'Calculate simple interest',
        confidence: 0.95,
        inputs: [
          { name: 'principal', type: 'decimal', constraints: { min: 0, max: 1000000 } },
          { name: 'rate', type: 'decimal', constraints: { min: 0, max: 1 } },
        ],
        outputs: [{ name: 'interest', type: 'decimal' }],
        edgeCases: ['Zero principal', 'Zero rate'],
      },
    ];

    it('should generate test cases from production logs', () => {
      const dataset = generator.generateFromLogs(sampleLogs, sampleRules);

      expect(dataset).toHaveProperty('id');
      expect(dataset).toHaveProperty('testCases');
      expect(dataset.testCases.length).toBeGreaterThan(0);
      expect(dataset.sourceMetadata.logCount).toBe(3);
    });

    it('should include historical replay tests', () => {
      const dataset = generator.generateFromLogs(sampleLogs, sampleRules);
      
      const historicalTests = dataset.testCases.filter(
        tc => tc.generationStrategy === 'historical-replay'
      );
      
      expect(historicalTests.length).toBeGreaterThan(0);
      expect(historicalTests[0].inputs).toHaveProperty('principal');
    });

    it('should deduplicate tests when configured', () => {
      const duplicateLogs: ProductionLogEntry[] = [
        ...sampleLogs,
        { ...sampleLogs[0], transactionId: 'TXN004' }, // Duplicate inputs
      ];

      const dataset = generator.generateFromLogs(duplicateLogs, sampleRules, {
        deduplication: true,
      });

      // Should not have more historical tests than unique inputs
      const historicalTests = dataset.testCases.filter(
        tc => tc.generationStrategy === 'historical-replay'
      );
      expect(historicalTests.length).toBeLessThanOrEqual(3);
    });

    it('should generate boundary tests from observed data', () => {
      const dataset = generator.generateFromLogs(sampleLogs, sampleRules);
      
      const boundaryTests = dataset.testCases.filter(
        tc => tc.generationStrategy === 'boundary'
      );
      
      expect(boundaryTests.length).toBeGreaterThan(0);
    });

    it('should respect maxTestCases config', () => {
      const dataset = generator.generateFromLogs(sampleLogs, sampleRules, {
        maxTestCases: 5,
      });

      expect(dataset.testCases.length).toBeLessThanOrEqual(5);
    });

    it('should calculate statistics correctly', () => {
      const dataset = generator.generateFromLogs(sampleLogs, sampleRules);

      expect(dataset.statistics.totalCases).toBe(dataset.testCases.length);
      expect(dataset.statistics.byPriority).toBeDefined();
      expect(dataset.statistics.byStrategy).toBeDefined();
    });

    it('should anonymize data when configured', () => {
      const logsWithSensitive: ProductionLogEntry[] = [
        {
          timestamp: '2024-01-15T10:00:00Z',
          transactionId: 'TXN001',
          programName: 'PROCESS',
          inputs: { 
            customerId: 'CUST12345',
            ssn: '123-45-6789',
            amount: 1000 
          },
          outputs: { result: 'success' },
          executionTimeMs: 50,
        },
      ];

      const dataset = generator.generateFromLogs(logsWithSensitive, [], {
        anonymization: {
          fields: {
            'ssn': 'mask',
            'customerId': 'tokenize',
          },
          preserveFormat: true,
        },
      });

      // Check that sensitive data is anonymized
      const testCase = dataset.testCases.find(tc => tc.inputs.ssn);
      if (testCase) {
        expect(testCase.inputs.ssn).not.toBe('123-45-6789');
      }
      expect(dataset.sourceMetadata.anonymized).toBe(true);
    });
  });

  describe('generateFromBatchData', () => {
    it('should generate tests from batch data', () => {
      const batchData = [
        { inputs: { a: 1, b: 2 }, outputs: { sum: 3 } },
        { inputs: { a: 5, b: 10 }, outputs: { sum: 15 } },
      ];

      const tests = generator.generateFromBatchData(batchData, []);

      expect(tests.length).toBe(2);
      expect(tests[0].inputs).toEqual({ a: 1, b: 2 });
      expect(tests[0].expectedOutput).toEqual({ sum: 3 });
    });

    it('should respect maxTestCases for batch data', () => {
      const batchData = Array.from({ length: 100 }, (_, i) => ({
        inputs: { value: i },
        outputs: { result: i * 2 },
      }));

      const tests = generator.generateFromBatchData(batchData, [], {
        maxTestCases: 10,
      });

      expect(tests.length).toBe(10);
    });
  });

  describe('generateFromJCLLogs', () => {
    it('should parse JCL output and generate tests', () => {
      const jclOutput = `
//MYJOB   JOB (ACCT),'TEST',CLASS=A
//STEP1   EXEC PGM=CALCINT
//SYSIN   DD *
INPUT DATA HERE
/*
//SYSOUT  DD SYSOUT=*
OUTPUT DATA HERE
/*
      `;

      const tests = generator.generateFromJCLLogs(jclOutput, 'CALCINT', []);

      // Should parse at least the step
      expect(tests).toBeDefined();
    });
  });

  describe('detectDrift', () => {
    it('should detect new patterns in production data', () => {
      const currentDataset: GoldenDataset = {
        id: 'gd-1',
        name: 'Test Dataset',
        description: 'Test',
        projectId: 'proj-1',
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        version: 1,
        testCases: [
          {
            id: 'tc-1',
            name: 'Test 1',
            description: 'Test',
            inputs: { a: 1, b: 2 },
            expectedOutput: { sum: 3 },
            tolerance: { numeric: 0.001, string: 'exact' },
            tags: [],
            generationStrategy: 'historical-replay',
            priority: 'high',
          },
        ],
        sourceMetadata: {
          logCount: 1,
          dateRange: { start: '2024-01-01', end: '2024-01-15' },
          uniqueInputPatterns: 1,
          anonymized: false,
        },
        statistics: {
          totalCases: 1,
          byPriority: { high: 1 },
          byStrategy: { 'historical-replay': 1 },
          inputCoverage: 1,
          outputCoverage: 1,
        },
      };

      const newLogs: ProductionLogEntry[] = [
        {
          timestamp: '2024-01-20T10:00:00Z',
          transactionId: 'TXN100',
          programName: 'CALC',
          inputs: { a: 100, b: 200 }, // New pattern
          outputs: { sum: 300 },
          executionTimeMs: 50,
        },
      ];

      const driftResult = generator.detectDrift(currentDataset, newLogs, 0.1);

      expect(driftResult.newPatterns.length).toBeGreaterThan(0);
    });

    it('should detect changed behaviors', () => {
      const currentDataset: GoldenDataset = {
        id: 'gd-1',
        name: 'Test Dataset',
        description: 'Test',
        projectId: 'proj-1',
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        version: 1,
        testCases: [
          {
            id: 'tc-1',
            name: 'Test 1',
            description: 'Test',
            inputs: { a: 1, b: 2 },
            expectedOutput: { sum: 3 },
            tolerance: { numeric: 0.001, string: 'exact' },
            tags: [],
            generationStrategy: 'historical-replay',
            priority: 'high',
          },
        ],
        sourceMetadata: {
          logCount: 1,
          dateRange: { start: '2024-01-01', end: '2024-01-15' },
          uniqueInputPatterns: 1,
          anonymized: false,
        },
        statistics: {
          totalCases: 1,
          byPriority: { high: 1 },
          byStrategy: { 'historical-replay': 1 },
          inputCoverage: 1,
          outputCoverage: 1,
        },
      };

      const newLogs: ProductionLogEntry[] = [
        {
          timestamp: '2024-01-20T10:00:00Z',
          transactionId: 'TXN100',
          programName: 'CALC',
          inputs: { a: 1, b: 2 }, // Same inputs
          outputs: { sum: 4 }, // Different output - behavior changed!
          executionTimeMs: 50,
        },
      ];

      const driftResult = generator.detectDrift(currentDataset, newLogs);

      expect(driftResult.changedBehaviors.length).toBeGreaterThan(0);
    });

    it('should provide recommendation based on drift level', () => {
      const currentDataset: GoldenDataset = {
        id: 'gd-1',
        name: 'Test',
        description: 'Test',
        projectId: 'proj-1',
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        version: 1,
        testCases: [],
        sourceMetadata: {
          logCount: 0,
          dateRange: { start: '', end: '' },
          uniqueInputPatterns: 0,
          anonymized: false,
        },
        statistics: {
          totalCases: 0,
          byPriority: {},
          byStrategy: {},
          inputCoverage: 0,
          outputCoverage: 0,
        },
      };

      const driftResult = generator.detectDrift(currentDataset, [], 0.1);

      expect(driftResult.recommendation).toBe('keep'); // No drift = keep
    });
  });
});
