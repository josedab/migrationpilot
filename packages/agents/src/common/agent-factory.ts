/**
 * Agent Factory
 * 
 * Provides dependency injection for all agent types.
 * Enables easy testing by allowing mock implementations.
 */

import type { ICopilotClient } from './copilot-client.js';
import { CopilotClient } from './copilot-client.js';
import { ArcheologistAgent } from '../archeologist/index.js';
import { ArchitectAgent } from '../architect/index.js';
import { BuilderAgent } from '../builder/index.js';
import { ValidatorAgent } from '../validator/index.js';
import { ExplainerAgent } from '../explainer/index.js';

/**
 * Configuration for agent creation
 */
export interface AgentFactoryConfig {
  /** Optional Copilot client instance (for mocking in tests) */
  copilotClient?: ICopilotClient;
  /** Optional model overrides per agent type */
  modelOverrides?: {
    archeologist?: string;
    architect?: string;
    builder?: string;
    validator?: string;
    explainer?: string;
  };
}

/**
 * Container for all instantiated agents
 */
export interface AgentContainer {
  archeologist: ArcheologistAgent;
  architect: ArchitectAgent;
  builder: BuilderAgent;
  validator: ValidatorAgent;
  explainer: ExplainerAgent;
  copilotClient: ICopilotClient;
}

/**
 * Factory for creating agent instances with dependency injection
 */
export class AgentFactory {
  private copilotClient: ICopilotClient;

  constructor(config: AgentFactoryConfig = {}) {
    this.copilotClient = config.copilotClient || new CopilotClient();
  }

  /**
   * Get the Copilot client instance
   */
  getCopilotClient(): ICopilotClient {
    return this.copilotClient;
  }

  /**
   * Create an Archeologist agent for code analysis
   */
  createArcheologist(): ArcheologistAgent {
    return new ArcheologistAgent();
  }

  /**
   * Create an Architect agent for design generation
   */
  createArchitect(): ArchitectAgent {
    return new ArchitectAgent();
  }

  /**
   * Create a Builder agent for code generation
   */
  createBuilder(): BuilderAgent {
    return new BuilderAgent();
  }

  /**
   * Create a Validator agent for testing
   */
  createValidator(): ValidatorAgent {
    return new ValidatorAgent();
  }

  /**
   * Create an Explainer agent for Q&A
   */
  createExplainer(): ExplainerAgent {
    return new ExplainerAgent();
  }

  /**
   * Create all agents at once (convenience method)
   */
  createAll(): AgentContainer {
    return {
      archeologist: this.createArcheologist(),
      architect: this.createArchitect(),
      builder: this.createBuilder(),
      validator: this.createValidator(),
      explainer: this.createExplainer(),
      copilotClient: this.copilotClient,
    };
  }
}

// Default singleton factory for convenience
let defaultFactory: AgentFactory | null = null;

/**
 * Get or create the default agent factory
 */
export function getAgentFactory(config?: AgentFactoryConfig): AgentFactory {
  if (!defaultFactory || config) {
    defaultFactory = new AgentFactory(config);
  }
  return defaultFactory;
}

/**
 * Reset the default factory (useful for testing)
 */
export function resetAgentFactory(): void {
  defaultFactory = null;
}
