/**
 * Multi-LLM Orchestration
 * 
 * Supports multiple LLM providers (Claude, GPT-4, local Llama models)
 * with automatic fallback and cost optimization.
 */

export type LLMProvider = 'github-copilot' | 'openai' | 'anthropic' | 'ollama' | 'azure-openai' | 'custom';

export interface LLMConfig {
  provider: LLMProvider;
  model: string;
  apiKey?: string;
  baseUrl?: string;
  maxTokens?: number;
  temperature?: number;
  timeout?: number;
  costPer1kTokens?: number;
  rateLimit?: number;
}

export interface LLMMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

export interface LLMResponse {
  content: string;
  model: string;
  provider: LLMProvider;
  usage: {
    promptTokens: number;
    completionTokens: number;
    totalTokens: number;
  };
  latencyMs: number;
  cost?: number;
}

// API Response types for different providers
interface OpenAIAPIResponse {
  choices: Array<{ message?: { content?: string } }>;
  usage?: {
    prompt_tokens?: number;
    completion_tokens?: number;
    total_tokens?: number;
  };
}

interface AnthropicAPIResponse {
  content: Array<{ text?: string }>;
  usage?: {
    input_tokens?: number;
    output_tokens?: number;
  };
}

interface OllamaAPIResponse {
  message?: { content?: string };
  prompt_eval_count?: number;
  eval_count?: number;
}

interface CustomAPIResponse {
  choices?: Array<{ message?: { content?: string } }>;
  content?: string;
  usage?: {
    prompt_tokens?: number;
    completion_tokens?: number;
    total_tokens?: number;
  };
}

export interface OrchestratorConfig {
  primaryProvider: LLMConfig;
  fallbackProviders: LLMConfig[];
  strategy: 'failover' | 'load-balance' | 'cost-optimize' | 'latency-optimize';
  maxRetries: number;
  retryDelayMs: number;
  enableCaching: boolean;
  cacheTTLSeconds: number;
}

export interface ProviderHealth {
  provider: LLMProvider;
  model: string;
  available: boolean;
  latencyMs: number;
  errorRate: number;
  lastChecked: string;
}

export class MultiLLMOrchestrator {
  private config: OrchestratorConfig;
  private providerHealth: Map<string, ProviderHealth> = new Map();
  private responseCache: Map<string, { response: LLMResponse; expiry: number }> = new Map();
  private requestCounts: Map<string, number> = new Map();
  private errorCounts: Map<string, number> = new Map();

  constructor(config: OrchestratorConfig) {
    this.config = config;
    this.initializeProviders();
  }

  private initializeProviders(): void {
    const allProviders = [this.config.primaryProvider, ...this.config.fallbackProviders];
    
    for (const provider of allProviders) {
      const key = this.getProviderKey(provider);
      this.providerHealth.set(key, {
        provider: provider.provider,
        model: provider.model,
        available: true,
        latencyMs: 0,
        errorRate: 0,
        lastChecked: new Date().toISOString(),
      });
      this.requestCounts.set(key, 0);
      this.errorCounts.set(key, 0);
    }
  }

  /**
   * Send a completion request with automatic failover
   */
  async complete(messages: LLMMessage[]): Promise<LLMResponse> {
    // Check cache first
    if (this.config.enableCaching) {
      const cached = this.getCachedResponse(messages);
      if (cached) return cached;
    }

    // Select provider based on strategy
    const providers = this.selectProviders();
    
    let lastError: Error | null = null;
    
    for (const provider of providers) {
      for (let attempt = 0; attempt <= this.config.maxRetries; attempt++) {
        try {
          const response = await this.executeRequest(provider, messages);
          
          // Update health metrics
          this.updateProviderHealth(provider, response.latencyMs, false);
          
          // Cache response
          if (this.config.enableCaching) {
            this.cacheResponse(messages, response);
          }
          
          return response;
        } catch (error) {
          lastError = error instanceof Error ? error : new Error(String(error));
          this.updateProviderHealth(provider, 0, true);
          
          if (attempt < this.config.maxRetries) {
            await this.delay(this.config.retryDelayMs * (attempt + 1));
          }
        }
      }
    }
    
    throw new Error(`All providers failed. Last error: ${lastError?.message}`);
  }

  /**
   * Execute request with specific provider
   */
  private async executeRequest(provider: LLMConfig, messages: LLMMessage[]): Promise<LLMResponse> {
    const startTime = Date.now();
    
    switch (provider.provider) {
      case 'github-copilot':
        return this.executeGitHubCopilot(provider, messages, startTime);
      case 'openai':
        return this.executeOpenAI(provider, messages, startTime);
      case 'anthropic':
        return this.executeAnthropic(provider, messages, startTime);
      case 'ollama':
        return this.executeOllama(provider, messages, startTime);
      case 'azure-openai':
        return this.executeAzureOpenAI(provider, messages, startTime);
      case 'custom':
        return this.executeCustom(provider, messages, startTime);
      default:
        throw new Error(`Unknown provider: ${provider.provider}`);
    }
  }

  private async executeGitHubCopilot(
    config: LLMConfig,
    messages: LLMMessage[],
    startTime: number
  ): Promise<LLMResponse> {
    const response = await fetch('https://api.githubcopilot.com/chat/completions', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${config.apiKey || process.env.GITHUB_TOKEN}`,
        'Editor-Version': 'vscode/1.85.0',
        'Editor-Plugin-Version': 'copilot/1.0.0',
      },
      body: JSON.stringify({
        model: config.model || 'gpt-4',
        messages,
        max_tokens: config.maxTokens || 4000,
        temperature: config.temperature || 0.1,
      }),
      signal: AbortSignal.timeout(config.timeout || 60000),
    });

    if (!response.ok) {
      throw new Error(`GitHub Copilot API error: ${response.status}`);
    }

    const data = await response.json() as OpenAIAPIResponse;
    const latencyMs = Date.now() - startTime;

    return {
      content: data.choices[0]?.message?.content || '',
      model: config.model || 'gpt-4',
      provider: 'github-copilot',
      usage: {
        promptTokens: data.usage?.prompt_tokens || 0,
        completionTokens: data.usage?.completion_tokens || 0,
        totalTokens: data.usage?.total_tokens || 0,
      },
      latencyMs,
      cost: this.calculateCost(config, data.usage?.total_tokens || 0),
    };
  }

  private async executeOpenAI(
    config: LLMConfig,
    messages: LLMMessage[],
    startTime: number
  ): Promise<LLMResponse> {
    const response = await fetch('https://api.openai.com/v1/chat/completions', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${config.apiKey || process.env.OPENAI_API_KEY}`,
      },
      body: JSON.stringify({
        model: config.model || 'gpt-4-turbo-preview',
        messages,
        max_tokens: config.maxTokens || 4000,
        temperature: config.temperature || 0.1,
      }),
      signal: AbortSignal.timeout(config.timeout || 60000),
    });

    if (!response.ok) {
      throw new Error(`OpenAI API error: ${response.status}`);
    }

    const data = await response.json() as OpenAIAPIResponse;
    const latencyMs = Date.now() - startTime;

    return {
      content: data.choices[0]?.message?.content || '',
      model: config.model || 'gpt-4-turbo-preview',
      provider: 'openai',
      usage: {
        promptTokens: data.usage?.prompt_tokens || 0,
        completionTokens: data.usage?.completion_tokens || 0,
        totalTokens: data.usage?.total_tokens || 0,
      },
      latencyMs,
      cost: this.calculateCost(config, data.usage?.total_tokens || 0),
    };
  }

  private async executeAnthropic(
    config: LLMConfig,
    messages: LLMMessage[],
    startTime: number
  ): Promise<LLMResponse> {
    // Convert to Anthropic format
    const systemMessage = messages.find(m => m.role === 'system');
    const otherMessages = messages.filter(m => m.role !== 'system');

    const response = await fetch('https://api.anthropic.com/v1/messages', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'x-api-key': config.apiKey || process.env.ANTHROPIC_API_KEY || '',
        'anthropic-version': '2024-01-01',
      },
      body: JSON.stringify({
        model: config.model || 'claude-3-opus-20240229',
        max_tokens: config.maxTokens || 4000,
        system: systemMessage?.content,
        messages: otherMessages.map(m => ({
          role: m.role === 'assistant' ? 'assistant' : 'user',
          content: m.content,
        })),
      }),
      signal: AbortSignal.timeout(config.timeout || 60000),
    });

    if (!response.ok) {
      throw new Error(`Anthropic API error: ${response.status}`);
    }

    const data = await response.json() as AnthropicAPIResponse;
    const latencyMs = Date.now() - startTime;

    return {
      content: data.content[0]?.text || '',
      model: config.model || 'claude-3-opus-20240229',
      provider: 'anthropic',
      usage: {
        promptTokens: data.usage?.input_tokens || 0,
        completionTokens: data.usage?.output_tokens || 0,
        totalTokens: (data.usage?.input_tokens || 0) + (data.usage?.output_tokens || 0),
      },
      latencyMs,
      cost: this.calculateCost(config, (data.usage?.input_tokens || 0) + (data.usage?.output_tokens || 0)),
    };
  }

  private async executeOllama(
    config: LLMConfig,
    messages: LLMMessage[],
    startTime: number
  ): Promise<LLMResponse> {
    const baseUrl = config.baseUrl || 'http://localhost:11434';
    
    const response = await fetch(`${baseUrl}/api/chat`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        model: config.model || 'llama2',
        messages,
        stream: false,
        options: {
          num_predict: config.maxTokens || 4000,
          temperature: config.temperature || 0.1,
        },
      }),
      signal: AbortSignal.timeout(config.timeout || 120000), // Longer timeout for local models
    });

    if (!response.ok) {
      throw new Error(`Ollama API error: ${response.status}`);
    }

    const data = await response.json() as OllamaAPIResponse;
    const latencyMs = Date.now() - startTime;

    return {
      content: data.message?.content || '',
      model: config.model || 'llama2',
      provider: 'ollama',
      usage: {
        promptTokens: data.prompt_eval_count || 0,
        completionTokens: data.eval_count || 0,
        totalTokens: (data.prompt_eval_count || 0) + (data.eval_count || 0),
      },
      latencyMs,
      cost: 0, // Local models have no API cost
    };
  }

  private async executeAzureOpenAI(
    config: LLMConfig,
    messages: LLMMessage[],
    startTime: number
  ): Promise<LLMResponse> {
    if (!config.baseUrl) {
      throw new Error('Azure OpenAI requires baseUrl (deployment endpoint)');
    }

    const response = await fetch(`${config.baseUrl}/chat/completions?api-version=2024-02-15-preview`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'api-key': config.apiKey || process.env.AZURE_OPENAI_API_KEY || '',
      },
      body: JSON.stringify({
        messages,
        max_tokens: config.maxTokens || 4000,
        temperature: config.temperature || 0.1,
      }),
      signal: AbortSignal.timeout(config.timeout || 60000),
    });

    if (!response.ok) {
      throw new Error(`Azure OpenAI API error: ${response.status}`);
    }

    const data = await response.json() as OpenAIAPIResponse;
    const latencyMs = Date.now() - startTime;

    return {
      content: data.choices[0]?.message?.content || '',
      model: config.model || 'gpt-4',
      provider: 'azure-openai',
      usage: {
        promptTokens: data.usage?.prompt_tokens || 0,
        completionTokens: data.usage?.completion_tokens || 0,
        totalTokens: data.usage?.total_tokens || 0,
      },
      latencyMs,
      cost: this.calculateCost(config, data.usage?.total_tokens || 0),
    };
  }

  private async executeCustom(
    config: LLMConfig,
    messages: LLMMessage[],
    startTime: number
  ): Promise<LLMResponse> {
    if (!config.baseUrl) {
      throw new Error('Custom provider requires baseUrl');
    }

    const response = await fetch(config.baseUrl, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...(config.apiKey ? { 'Authorization': `Bearer ${config.apiKey}` } : {}),
      },
      body: JSON.stringify({
        model: config.model,
        messages,
        max_tokens: config.maxTokens,
        temperature: config.temperature,
      }),
      signal: AbortSignal.timeout(config.timeout || 60000),
    });

    if (!response.ok) {
      throw new Error(`Custom API error: ${response.status}`);
    }

    const data = await response.json() as CustomAPIResponse;
    const latencyMs = Date.now() - startTime;

    return {
      content: data.choices?.[0]?.message?.content || data.content || '',
      model: config.model || 'custom',
      provider: 'custom',
      usage: {
        promptTokens: data.usage?.prompt_tokens || 0,
        completionTokens: data.usage?.completion_tokens || 0,
        totalTokens: data.usage?.total_tokens || 0,
      },
      latencyMs,
      cost: this.calculateCost(config, data.usage?.total_tokens || 0),
    };
  }

  /**
   * Select providers based on strategy
   */
  private selectProviders(): LLMConfig[] {
    const allProviders = [this.config.primaryProvider, ...this.config.fallbackProviders];
    
    switch (this.config.strategy) {
      case 'failover':
        return allProviders;
      
      case 'load-balance':
        return this.shuffleArray([...allProviders]);
      
      case 'cost-optimize':
        return [...allProviders].sort((a, b) => 
          (a.costPer1kTokens || 0) - (b.costPer1kTokens || 0)
        );
      
      case 'latency-optimize':
        return [...allProviders].sort((a, b) => {
          const healthA = this.providerHealth.get(this.getProviderKey(a));
          const healthB = this.providerHealth.get(this.getProviderKey(b));
          return (healthA?.latencyMs || Infinity) - (healthB?.latencyMs || Infinity);
        });
      
      default:
        return allProviders;
    }
  }

  /**
   * Get health status of all providers
   */
  getProviderHealth(): ProviderHealth[] {
    return Array.from(this.providerHealth.values());
  }

  /**
   * Update provider health metrics
   */
  private updateProviderHealth(provider: LLMConfig, latencyMs: number, isError: boolean): void {
    const key = this.getProviderKey(provider);
    const health = this.providerHealth.get(key);
    
    if (!health) return;
    
    const requests = (this.requestCounts.get(key) || 0) + 1;
    const errors = (this.errorCounts.get(key) || 0) + (isError ? 1 : 0);
    
    this.requestCounts.set(key, requests);
    this.errorCounts.set(key, errors);
    
    health.latencyMs = isError 
      ? health.latencyMs 
      : (health.latencyMs * 0.8 + latencyMs * 0.2); // Exponential moving average
    health.errorRate = errors / requests;
    health.available = health.errorRate < 0.5;
    health.lastChecked = new Date().toISOString();
  }

  /**
   * Cache response
   */
  private cacheResponse(messages: LLMMessage[], response: LLMResponse): void {
    const key = this.getCacheKey(messages);
    this.responseCache.set(key, {
      response,
      expiry: Date.now() + (this.config.cacheTTLSeconds * 1000),
    });
  }

  /**
   * Get cached response
   */
  private getCachedResponse(messages: LLMMessage[]): LLMResponse | null {
    const key = this.getCacheKey(messages);
    const cached = this.responseCache.get(key);
    
    if (cached && cached.expiry > Date.now()) {
      return cached.response;
    }
    
    if (cached) {
      this.responseCache.delete(key);
    }
    
    return null;
  }

  /**
   * Calculate cost based on token usage
   */
  private calculateCost(config: LLMConfig, tokens: number): number {
    if (!config.costPer1kTokens) return 0;
    return (tokens / 1000) * config.costPer1kTokens;
  }

  /**
   * Get unique key for provider
   */
  private getProviderKey(provider: LLMConfig): string {
    return `${provider.provider}:${provider.model}`;
  }

  /**
   * Get cache key for messages
   */
  private getCacheKey(messages: LLMMessage[]): string {
    const hash = messages.map(m => `${m.role}:${m.content}`).join('|');
    return Buffer.from(hash).toString('base64').slice(0, 64);
  }

  /**
   * Shuffle array for load balancing
   */
  private shuffleArray<T>(array: T[]): T[] {
    for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [array[i], array[j]] = [array[j]!, array[i]!];
    }
    return array;
  }

  /**
   * Delay helper
   */
  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

/**
 * Create a pre-configured orchestrator for common scenarios
 */
export function createOrchestrator(
  scenario: 'high-availability' | 'cost-effective' | 'air-gapped' | 'enterprise',
  overrides?: Partial<OrchestratorConfig>
): MultiLLMOrchestrator {
  const configs: Record<string, OrchestratorConfig> = {
    'high-availability': {
      primaryProvider: { provider: 'github-copilot', model: 'gpt-4' },
      fallbackProviders: [
        { provider: 'openai', model: 'gpt-4-turbo-preview' },
        { provider: 'anthropic', model: 'claude-3-opus-20240229' },
      ],
      strategy: 'failover',
      maxRetries: 3,
      retryDelayMs: 1000,
      enableCaching: true,
      cacheTTLSeconds: 300,
    },
    'cost-effective': {
      primaryProvider: { provider: 'ollama', model: 'llama2', baseUrl: 'http://localhost:11434' },
      fallbackProviders: [
        { provider: 'github-copilot', model: 'gpt-3.5-turbo', costPer1kTokens: 0.001 },
      ],
      strategy: 'cost-optimize',
      maxRetries: 2,
      retryDelayMs: 500,
      enableCaching: true,
      cacheTTLSeconds: 600,
    },
    'air-gapped': {
      primaryProvider: { provider: 'ollama', model: 'codellama', baseUrl: 'http://localhost:11434' },
      fallbackProviders: [
        { provider: 'ollama', model: 'llama2', baseUrl: 'http://localhost:11434' },
      ],
      strategy: 'failover',
      maxRetries: 3,
      retryDelayMs: 2000,
      enableCaching: true,
      cacheTTLSeconds: 3600,
    },
    'enterprise': {
      primaryProvider: { provider: 'azure-openai', model: 'gpt-4' },
      fallbackProviders: [
        { provider: 'github-copilot', model: 'gpt-4' },
        { provider: 'anthropic', model: 'claude-3-opus-20240229' },
      ],
      strategy: 'latency-optimize',
      maxRetries: 3,
      retryDelayMs: 1000,
      enableCaching: true,
      cacheTTLSeconds: 300,
    },
  };

  return new MultiLLMOrchestrator({
    ...configs[scenario],
    ...overrides,
  } as OrchestratorConfig);
}
