/**
 * Tests for Multi-LLM Orchestration
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { MultiLLMOrchestrator, createOrchestrator, type LLMConfig, type OrchestratorConfig } from '../common/multi-llm.js';

// Mock fetch globally
const mockFetch = vi.fn();
global.fetch = mockFetch;

describe('MultiLLMOrchestrator', () => {
  let orchestrator: MultiLLMOrchestrator;
  
  const primaryConfig: LLMConfig = {
    provider: 'github-copilot',
    model: 'gpt-4',
    apiKey: 'test-key',
    maxTokens: 4000,
    temperature: 0.1,
    timeout: 30000,
    costPer1kTokens: 0.03,
  };

  const fallbackConfig: LLMConfig = {
    provider: 'openai',
    model: 'gpt-4-turbo',
    apiKey: 'test-openai-key',
    maxTokens: 4000,
    temperature: 0.1,
    timeout: 30000,
    costPer1kTokens: 0.01,
  };

  const orchestratorConfig: OrchestratorConfig = {
    primaryProvider: primaryConfig,
    fallbackProviders: [fallbackConfig],
    strategy: 'failover',
    maxRetries: 2,
    retryDelayMs: 100,
    enableCaching: true,
    cacheTTLSeconds: 60,
  };

  beforeEach(() => {
    mockFetch.mockReset();
    orchestrator = new MultiLLMOrchestrator(orchestratorConfig);
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('complete', () => {
    it('should successfully complete a request with primary provider', async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: () => Promise.resolve({
          choices: [{ message: { content: 'Test response' } }],
          usage: { prompt_tokens: 10, completion_tokens: 20, total_tokens: 30 }
        })
      });

      const messages = [
        { role: 'user' as const, content: 'Hello' }
      ];

      const response = await orchestrator.complete(messages);

      expect(response.content).toBe('Test response');
      expect(response.provider).toBe('github-copilot');
      expect(response.usage.totalTokens).toBe(30);
      expect(mockFetch).toHaveBeenCalledTimes(1);
    });

    it('should fallback to secondary provider on primary failure', async () => {
      // Primary fails
      mockFetch.mockRejectedValueOnce(new Error('Primary failed'));
      mockFetch.mockRejectedValueOnce(new Error('Primary retry failed'));
      mockFetch.mockRejectedValueOnce(new Error('Primary retry 2 failed'));
      
      // Fallback succeeds
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: () => Promise.resolve({
          choices: [{ message: { content: 'Fallback response' } }],
          usage: { prompt_tokens: 10, completion_tokens: 20, total_tokens: 30 }
        })
      });

      const messages = [{ role: 'user' as const, content: 'Hello' }];
      const response = await orchestrator.complete(messages);

      expect(response.content).toBe('Fallback response');
      expect(response.provider).toBe('openai');
    });

    it('should throw error when all providers fail', async () => {
      mockFetch.mockRejectedValue(new Error('All providers failed'));

      const messages = [{ role: 'user' as const, content: 'Hello' }];

      await expect(orchestrator.complete(messages)).rejects.toThrow('All providers failed');
    });

    it('should use cached response when available', async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: () => Promise.resolve({
          choices: [{ message: { content: 'Cached response' } }],
          usage: { prompt_tokens: 10, completion_tokens: 20, total_tokens: 30 }
        })
      });

      const messages = [{ role: 'user' as const, content: 'Hello' }];

      // First call
      const response1 = await orchestrator.complete(messages);
      expect(response1.content).toBe('Cached response');

      // Second call should use cache
      const response2 = await orchestrator.complete(messages);
      expect(response2.content).toBe('Cached response');

      // Only one fetch call due to caching
      expect(mockFetch).toHaveBeenCalledTimes(1);
    });
  });

  describe('getProviderHealth', () => {
    it('should return health status for all providers', () => {
      const health = orchestrator.getProviderHealth();

      expect(health).toHaveLength(2);
      expect(health[0]).toHaveProperty('provider');
      expect(health[0]).toHaveProperty('available');
      expect(health[0]).toHaveProperty('latencyMs');
      expect(health[0]).toHaveProperty('errorRate');
    });

    it('should update health after successful request', async () => {
      mockFetch.mockResolvedValueOnce({
        ok: true,
        json: () => Promise.resolve({
          choices: [{ message: { content: 'Response' } }],
          usage: { total_tokens: 30 }
        })
      });

      await orchestrator.complete([{ role: 'user', content: 'Test' }]);

      const health = orchestrator.getProviderHealth();
      const primaryHealth = health.find(h => h.provider === 'github-copilot');
      
      expect(primaryHealth?.errorRate).toBe(0);
    });
  });
});

describe('createOrchestrator', () => {
  beforeEach(() => {
    mockFetch.mockReset();
  });

  it('should create high-availability orchestrator', () => {
    const orchestrator = createOrchestrator('high-availability');
    const health = orchestrator.getProviderHealth();

    expect(health.length).toBeGreaterThan(1);
  });

  it('should create cost-effective orchestrator', () => {
    const orchestrator = createOrchestrator('cost-effective');
    const health = orchestrator.getProviderHealth();

    // Should have ollama as primary for cost savings
    expect(health.some(h => h.provider === 'ollama')).toBe(true);
  });

  it('should create air-gapped orchestrator with only local models', () => {
    const orchestrator = createOrchestrator('air-gapped');
    const health = orchestrator.getProviderHealth();

    // All providers should be ollama (local)
    expect(health.every(h => h.provider === 'ollama')).toBe(true);
  });

  it('should allow config overrides', () => {
    const orchestrator = createOrchestrator('high-availability', {
      maxRetries: 5,
      cacheTTLSeconds: 600,
    });

    // Internal config should be overridden (we can't directly test, but it shouldn't throw)
    expect(orchestrator).toBeDefined();
  });
});

describe('Provider-specific execution', () => {
  beforeEach(() => {
    mockFetch.mockReset();
  });

  it('should format request correctly for Anthropic', async () => {
    const anthropicConfig: OrchestratorConfig = {
      primaryProvider: {
        provider: 'anthropic',
        model: 'claude-3-opus',
        apiKey: 'test-anthropic-key',
      },
      fallbackProviders: [],
      strategy: 'failover',
      maxRetries: 0,
      retryDelayMs: 100,
      enableCaching: false,
      cacheTTLSeconds: 0,
    };

    const orchestrator = new MultiLLMOrchestrator(anthropicConfig);

    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: () => Promise.resolve({
        content: [{ text: 'Claude response' }],
        usage: { input_tokens: 10, output_tokens: 20 }
      })
    });

    const messages = [
      { role: 'system' as const, content: 'You are helpful' },
      { role: 'user' as const, content: 'Hello' }
    ];

    const response = await orchestrator.complete(messages);

    expect(response.provider).toBe('anthropic');
    expect(response.content).toBe('Claude response');
    
    // Verify Anthropic API was called with correct format
    expect(mockFetch).toHaveBeenCalledWith(
      'https://api.anthropic.com/v1/messages',
      expect.objectContaining({
        method: 'POST',
        headers: expect.objectContaining({
          'anthropic-version': '2024-01-01'
        })
      })
    );
  });

  it('should handle Ollama (local) requests', async () => {
    const ollamaConfig: OrchestratorConfig = {
      primaryProvider: {
        provider: 'ollama',
        model: 'llama2',
        baseUrl: 'http://localhost:11434',
      },
      fallbackProviders: [],
      strategy: 'failover',
      maxRetries: 0,
      retryDelayMs: 100,
      enableCaching: false,
      cacheTTLSeconds: 0,
    };

    const orchestrator = new MultiLLMOrchestrator(ollamaConfig);

    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: () => Promise.resolve({
        message: { content: 'Ollama response' },
        prompt_eval_count: 10,
        eval_count: 20
      })
    });

    const response = await orchestrator.complete([{ role: 'user', content: 'Hello' }]);

    expect(response.provider).toBe('ollama');
    expect(response.cost).toBe(0); // Local models have no cost
  });
});
