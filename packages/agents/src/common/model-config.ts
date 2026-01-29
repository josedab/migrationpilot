/**
 * Model Configuration Provider
 * 
 * Centralizes AI model configuration for all agents. Supports environment-based overrides.
 */

import {
  AGENT_DEFAULT_MODEL,
  AGENT_TEMPERATURE,
  AGENT_MAX_TOKENS,
} from '@migrationpilot/core';

export interface ModelConfig {
  model: string;
  temperature: number;
  maxTokens: number;
}

export interface AgentModelConfigs {
  archeologist: ModelConfig;
  architect: ModelConfig;
  builder: ModelConfig;
  validator: ModelConfig;
  explainer: ModelConfig;
}

/**
 * Default model configurations for each agent type.
 * These can be overridden via environment variables or the setConfig method.
 */
const DEFAULT_CONFIGS: AgentModelConfigs = {
  archeologist: {
    model: AGENT_DEFAULT_MODEL,
    temperature: AGENT_TEMPERATURE.ANALYTICAL,
    maxTokens: AGENT_MAX_TOKENS.DEFAULT,
  },
  architect: {
    model: 'claude-sonnet-4-20250514',
    temperature: AGENT_TEMPERATURE.CREATIVE,
    maxTokens: AGENT_MAX_TOKENS.DEFAULT,
  },
  builder: {
    model: AGENT_DEFAULT_MODEL,
    temperature: AGENT_TEMPERATURE.CODE_GENERATION,
    maxTokens: AGENT_MAX_TOKENS.CODE_GENERATION,
  },
  validator: {
    model: AGENT_DEFAULT_MODEL,
    temperature: AGENT_TEMPERATURE.ANALYTICAL,
    maxTokens: AGENT_MAX_TOKENS.DEFAULT,
  },
  explainer: {
    model: AGENT_DEFAULT_MODEL,
    temperature: AGENT_TEMPERATURE.CREATIVE,
    maxTokens: AGENT_MAX_TOKENS.EXPLANATION,
  },
};

class ModelConfigProvider {
  private configs: AgentModelConfigs;

  constructor() {
    this.configs = this.loadFromEnvironment();
  }

  /**
   * Load configurations from environment variables with fallback to defaults.
   * Environment variables follow the pattern:
   *   AGENT_<AGENT_TYPE>_MODEL (e.g., AGENT_ARCHEOLOGIST_MODEL)
   *   AGENT_<AGENT_TYPE>_TEMPERATURE (e.g., AGENT_ARCHEOLOGIST_TEMPERATURE)
   *   AGENT_<AGENT_TYPE>_MAX_TOKENS (e.g., AGENT_ARCHEOLOGIST_MAX_TOKENS)
   */
  private loadFromEnvironment(): AgentModelConfigs {
    const agentTypes: (keyof AgentModelConfigs)[] = [
      'archeologist',
      'architect',
      'builder',
      'validator',
      'explainer',
    ];

    const configs = { ...DEFAULT_CONFIGS };

    for (const agentType of agentTypes) {
      const envPrefix = `AGENT_${agentType.toUpperCase()}`;
      
      const modelEnv = process.env[`${envPrefix}_MODEL`];
      const temperatureEnv = process.env[`${envPrefix}_TEMPERATURE`];
      const maxTokensEnv = process.env[`${envPrefix}_MAX_TOKENS`];

      if (modelEnv) {
        configs[agentType] = {
          ...configs[agentType],
          model: modelEnv,
        };
      }
      if (temperatureEnv) {
        const temp = parseFloat(temperatureEnv);
        if (!isNaN(temp) && temp >= 0 && temp <= 2) {
          configs[agentType] = {
            ...configs[agentType],
            temperature: temp,
          };
        }
      }
      if (maxTokensEnv) {
        const tokens = parseInt(maxTokensEnv, 10);
        if (!isNaN(tokens) && tokens > 0) {
          configs[agentType] = {
            ...configs[agentType],
            maxTokens: tokens,
          };
        }
      }
    }

    // Global model override (applies to all agents if set)
    const globalModel = process.env['AGENT_DEFAULT_MODEL'];
    if (globalModel) {
      for (const agentType of agentTypes) {
        configs[agentType] = {
          ...configs[agentType],
          model: globalModel,
        };
      }
    }

    return configs;
  }

  /**
   * Get configuration for a specific agent type
   */
  getConfig(agentType: keyof AgentModelConfigs): ModelConfig {
    return { ...this.configs[agentType] };
  }

  /**
   * Update configuration for a specific agent type (runtime override)
   */
  setConfig(agentType: keyof AgentModelConfigs, config: Partial<ModelConfig>): void {
    this.configs[agentType] = {
      ...this.configs[agentType],
      ...config,
    };
  }

  /**
   * Get all configurations
   */
  getAllConfigs(): AgentModelConfigs {
    return { ...this.configs };
  }

  /**
   * Reset configurations to defaults
   */
  resetToDefaults(): void {
    this.configs = { ...DEFAULT_CONFIGS };
  }

  /**
   * Get default configurations (useful for documentation/display)
   */
  getDefaults(): AgentModelConfigs {
    return { ...DEFAULT_CONFIGS };
  }
}

// Export singleton instance
export const modelConfigProvider = new ModelConfigProvider();

// Export class for testing
export { ModelConfigProvider };
