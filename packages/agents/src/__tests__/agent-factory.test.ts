/**
 * Tests for Agent Factory
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { AgentFactory, getAgentFactory, resetAgentFactory } from '../common/agent-factory.js';
import { ArcheologistAgent } from '../archeologist/index.js';
import { ArchitectAgent } from '../architect/index.js';
import { BuilderAgent } from '../builder/index.js';
import { ValidatorAgent } from '../validator/index.js';
import { ExplainerAgent } from '../explainer/index.js';

describe('AgentFactory', () => {
  describe('constructor', () => {
    it('should create a factory with default configuration', () => {
      const factory = new AgentFactory();
      expect(factory).toBeInstanceOf(AgentFactory);
      expect(factory.getCopilotClient()).toBeDefined();
    });

    it('should accept custom copilot client', () => {
      const mockClient = {
        createCompletion: vi.fn(),
        executeWithTools: vi.fn(),
      };
      
      const factory = new AgentFactory({ copilotClient: mockClient as any });
      expect(factory.getCopilotClient()).toBe(mockClient);
    });
  });

  describe('agent creation', () => {
    let factory: AgentFactory;

    beforeEach(() => {
      factory = new AgentFactory();
    });

    it('should create ArcheologistAgent', () => {
      const agent = factory.createArcheologist();
      expect(agent).toBeInstanceOf(ArcheologistAgent);
    });

    it('should create ArchitectAgent', () => {
      const agent = factory.createArchitect();
      expect(agent).toBeInstanceOf(ArchitectAgent);
    });

    it('should create BuilderAgent', () => {
      const agent = factory.createBuilder();
      expect(agent).toBeInstanceOf(BuilderAgent);
    });

    it('should create ValidatorAgent', () => {
      const agent = factory.createValidator();
      expect(agent).toBeInstanceOf(ValidatorAgent);
    });

    it('should create ExplainerAgent', () => {
      const agent = factory.createExplainer();
      expect(agent).toBeInstanceOf(ExplainerAgent);
    });

    it('should create all agents at once', () => {
      const container = factory.createAll();
      
      expect(container.archeologist).toBeInstanceOf(ArcheologistAgent);
      expect(container.architect).toBeInstanceOf(ArchitectAgent);
      expect(container.builder).toBeInstanceOf(BuilderAgent);
      expect(container.validator).toBeInstanceOf(ValidatorAgent);
      expect(container.explainer).toBeInstanceOf(ExplainerAgent);
      expect(container.copilotClient).toBeDefined();
    });
  });

  describe('getAgentFactory', () => {
    beforeEach(() => {
      resetAgentFactory();
    });

    it('should return a singleton factory', () => {
      const factory1 = getAgentFactory();
      const factory2 = getAgentFactory();
      expect(factory1).toBe(factory2);
    });

    it('should create new factory when config is provided', () => {
      const factory1 = getAgentFactory();
      const factory2 = getAgentFactory({ modelOverrides: { archeologist: 'gpt-4o' } });
      expect(factory1).not.toBe(factory2);
    });
  });

  describe('resetAgentFactory', () => {
    it('should reset the singleton', () => {
      const factory1 = getAgentFactory();
      resetAgentFactory();
      const factory2 = getAgentFactory();
      expect(factory1).not.toBe(factory2);
    });
  });
});
