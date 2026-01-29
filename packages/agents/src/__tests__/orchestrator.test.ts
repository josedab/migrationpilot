/**
 * Migration Orchestrator Tests
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { MigrationOrchestrator, type MigrationConfig } from '../orchestrator.js';
import type { ICopilotClient } from '../common/copilot-client.js';

// Mock the agents
vi.mock('../archeologist/agent.js', () => ({
  ArcheologistAgent: vi.fn().mockImplementation(() => ({
    analyze: vi.fn().mockResolvedValue({
      success: true,
      businessRules: [{ id: 'BR-001', name: 'Test Rule', confidence: 0.95 }],
      dataStructures: [],
      procedures: [],
    }),
    analyzeFile: vi.fn().mockResolvedValue({
      success: true,
      data: {
        businessRules: [{ id: 'BR-001', name: 'Test Rule', confidence: 0.95 }],
        dataStructures: [],
        procedures: [],
      },
    }),
  })),
}));

vi.mock('../architect/agent.js', () => ({
  ArchitectAgent: vi.fn().mockImplementation(() => ({
    design: vi.fn().mockResolvedValue({
      success: true,
      architecture: {
        pattern: 'microservices',
        services: [{ name: 'test-service' }],
      },
    }),
    designArchitecture: vi.fn().mockResolvedValue({
      success: true,
      data: {
        services: [{ name: 'test-service', responsibilities: ['test'] }],
        apis: [],
      },
    }),
  })),
}));

vi.mock('../builder/agent.js', () => ({
  BuilderAgent: vi.fn().mockImplementation(() => ({
    generate: vi.fn().mockResolvedValue({
      success: true,
      files: [{ path: 'src/Service.java', content: 'public class Service {}' }],
    }),
    generateService: vi.fn().mockResolvedValue({
      success: true,
      data: {
        files: [{ path: 'src/Service.java', content: 'public class Service {}', type: 'source' }],
      },
    }),
  })),
}));

vi.mock('../validator/agent.js', () => ({
  ValidatorAgent: vi.fn().mockImplementation(() => ({
    validate: vi.fn().mockResolvedValue({
      success: true,
      testResults: [{ id: 'T001', status: 'passed' }],
      confidenceScore: 0.98,
    }),
    generateTestCases: vi.fn().mockResolvedValue({
      success: true,
      data: [{ id: 'TC-001', name: 'Test Case 1' }],
    }),
    validateEquivalence: vi.fn().mockResolvedValue({
      success: true,
      data: {
        report: { equivalenceScore: 95 },
      },
    }),
  })),
}));

// Create a mock Copilot client for DI testing
function createMockCopilotClient(): ICopilotClient {
  return {
    createCompletion: vi.fn().mockResolvedValue({
      content: '{"success": true}',
      model: 'gpt-4',
      usage: { promptTokens: 100, completionTokens: 50, totalTokens: 150 },
    }),
    createStreamingCompletion: vi.fn().mockImplementation(async function* () {
      yield { content: '{"success": true}', done: false };
      yield { content: '', done: true };
    }),
    executeWithTools: vi.fn().mockResolvedValue({
      content: '{"success": true}',
      toolCalls: [],
    }),
  };
}

describe('MigrationOrchestrator', () => {
  let orchestrator: MigrationOrchestrator;

  beforeEach(() => {
    orchestrator = new MigrationOrchestrator();
  });

  describe('constructor', () => {
    it('should create orchestrator with default factory', () => {
      const orch = new MigrationOrchestrator();
      expect(orch).toBeDefined();
    });

    it('should accept custom copilot client via factory config', () => {
      const mockClient = createMockCopilotClient();
      const orch = new MigrationOrchestrator({
        copilotClient: mockClient,
      });
      expect(orch).toBeDefined();
    });
  });

  describe('migrate', () => {
    it('should run full migration pipeline', async () => {
      const sourceCode = 'IDENTIFICATION DIVISION.';
      const filename = 'TEST.cbl';
      const config: MigrationConfig = {
        projectId: 'proj_test',
        userId: 'user_test',
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        targetFramework: 'spring-boot',
        options: {
          enableStranglerFig: false,
          generateTests: true,
          generateDocumentation: true,
          humanReviewRequired: false,
          confidenceThreshold: 0.85,
        },
        callbacks: {
          onProgress: vi.fn(),
        },
      };

      const result = await orchestrator.migrate(sourceCode, filename, config);

      expect(result.success).toBe(true);
      expect(result.projectId).toBe('proj_test');
    });

    it('should include timing information in result', async () => {
      const config: MigrationConfig = {
        projectId: 'proj_timing',
        userId: 'user_test',
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        targetFramework: 'spring-boot',
        options: {
          enableStranglerFig: false,
          generateTests: true,
          generateDocumentation: true,
          humanReviewRequired: false,
          confidenceThreshold: 0.85,
        },
      };

      const result = await orchestrator.migrate('CODE', 'test.cbl', config);

      expect(result.startedAt).toBeInstanceOf(Date);
      expect(result.completedAt).toBeInstanceOf(Date);
      expect(result.duration).toBeGreaterThanOrEqual(0);
    });

    it('should handle analysis failures', async () => {
      // Override mock for this test
      vi.doMock('../archeologist/agent.js', () => ({
        ArcheologistAgent: vi.fn().mockImplementation(() => ({
          analyzeFile: vi.fn().mockResolvedValue({
            success: false,
            error: 'Parse error',
          }),
        })),
      }));

      const sourceCode = 'INVALID';
      const filename = 'BAD.cbl';
      const config: MigrationConfig = {
        projectId: 'proj_fail',
        userId: 'user_test',
        sourceLanguage: 'cobol',
        targetLanguage: 'java',
        targetFramework: 'spring-boot',
        options: {
          enableStranglerFig: false,
          generateTests: true,
          generateDocumentation: true,
          humanReviewRequired: false,
          confidenceThreshold: 0.85,
        },
      };

      const result = await orchestrator.migrate(sourceCode, filename, config);

      // Should fail gracefully
      expect(result).toBeDefined();
      expect(result.projectId).toBe('proj_fail');
    });
  });

  describe('analyzeOnly', () => {
    it('should run only analysis phase', async () => {
      const sourceCode = 'PROGRAM CALC';
      const filename = 'calc.f90';

      const result = await orchestrator.analyzeOnly(
        sourceCode,
        filename,
        'fortran',
        'proj_analyze',
        'user_test'
      );

      expect(result).toBeDefined();
      expect(result?.businessRules).toBeDefined();
    });
  });
});
