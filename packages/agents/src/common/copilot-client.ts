/**
 * Copilot SDK Client Wrapper
 * 
 * Provides a unified interface for interacting with the GitHub Copilot SDK
 */

import type { AgentTool } from '@migrationpilot/core';
import {
  retry,
  AGENT_DEFAULT_MODEL,
  DEFAULT_API_TIMEOUT_MS,
  DEFAULT_MAX_RETRIES,
  COPILOT_MAX_TOOL_ITERATIONS,
} from '@migrationpilot/core';

export interface CopilotMessage {
  role: 'system' | 'user' | 'assistant' | 'tool';
  content: string;
  tool_call_id?: string;
  tool_calls?: Array<{
    id: string;
    type: 'function';
    function: {
      name: string;
      arguments: string;
    };
  }>;
}

export interface CopilotCompletionRequest {
  messages: CopilotMessage[];
  model?: string;
  temperature?: number;
  max_tokens?: number;
  tools?: Array<{
    type: 'function';
    function: {
      name: string;
      description: string;
      parameters: Record<string, unknown>;
    };
  }>;
  tool_choice?: 'auto' | 'none' | { type: 'function'; function: { name: string } };
  stream?: boolean;
}

export interface CopilotCompletionResponse {
  id: string;
  object: string;
  created: number;
  model: string;
  choices: Array<{
    index: number;
    message: {
      role: 'assistant';
      content: string | null;
      tool_calls?: Array<{
        id: string;
        type: 'function';
        function: {
          name: string;
          arguments: string;
        };
      }>;
    };
    finish_reason: 'stop' | 'tool_calls' | 'length';
  }>;
  usage: {
    prompt_tokens: number;
    completion_tokens: number;
    total_tokens: number;
  };
}

export interface CopilotClientConfig {
  apiKey?: string;
  baseUrl?: string;
  defaultModel?: string;
  timeout?: number;
  maxRetries?: number;
}

/**
 * Interface for CopilotClient to enable dependency injection and testing
 */
export interface ICopilotClient {
  createCompletion(request: CopilotCompletionRequest): Promise<CopilotCompletionResponse>;
  createStreamingCompletion(request: CopilotCompletionRequest): AsyncGenerator<string>;
  executeWithTools(
    messages: CopilotMessage[],
    tools: AgentTool[],
    options?: {
      model?: string;
      temperature?: number;
      maxTokens?: number;
      maxIterations?: number;
    }
  ): Promise<{
    content: string;
    toolCalls: Array<{
      name: string;
      arguments: Record<string, unknown>;
      result: unknown;
    }>;
  }>;
}

export class CopilotClient implements ICopilotClient {
  private config: Required<CopilotClientConfig>;

  constructor(config: CopilotClientConfig = {}) {
    this.config = {
      apiKey: config.apiKey || process.env.GITHUB_TOKEN || '',
      baseUrl: config.baseUrl || 'https://api.githubcopilot.com',
      defaultModel: config.defaultModel || AGENT_DEFAULT_MODEL,
      timeout: config.timeout || DEFAULT_API_TIMEOUT_MS,
      maxRetries: config.maxRetries || DEFAULT_MAX_RETRIES,
    };

    if (!this.config.apiKey) {
      console.warn('CopilotClient: No API key provided. Set GITHUB_TOKEN environment variable.');
    }
  }

  /**
   * Create a chat completion
   */
  async createCompletion(
    request: CopilotCompletionRequest
  ): Promise<CopilotCompletionResponse> {
    return retry(
      async () => {
        const response = await fetch(`${this.config.baseUrl}/chat/completions`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${this.config.apiKey}`,
            'Editor-Version': 'vscode/1.85.0',
            'Editor-Plugin-Version': 'copilot/1.0.0',
            'Copilot-Integration-Id': 'migrationpilot',
          },
          body: JSON.stringify({
            model: request.model || this.config.defaultModel,
            ...request,
          }),
          signal: AbortSignal.timeout(this.config.timeout),
        });

        if (!response.ok) {
          const error = await response.text();
          throw new Error(`Copilot API error: ${response.status} - ${error}`);
        }

        return response.json() as Promise<CopilotCompletionResponse>;
      },
      { maxRetries: this.config.maxRetries }
    );
  }

  /**
   * Create a streaming chat completion
   */
  async *createStreamingCompletion(
    request: CopilotCompletionRequest
  ): AsyncGenerator<string> {
    const response = await fetch(`${this.config.baseUrl}/chat/completions`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.config.apiKey}`,
        'Editor-Version': 'vscode/1.85.0',
        'Editor-Plugin-Version': 'copilot/1.0.0',
        'Copilot-Integration-Id': 'migrationpilot',
      },
      body: JSON.stringify({
        model: request.model || this.config.defaultModel,
        stream: true,
        ...request,
      }),
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Copilot API error: ${response.status} - ${error}`);
    }

    const reader = response.body?.getReader();
    if (!reader) {
      throw new Error('No response body');
    }

    const decoder = new TextDecoder();
    let buffer = '';

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split('\n');
      buffer = lines.pop() || '';

      for (const line of lines) {
        if (line.startsWith('data: ')) {
          const data = line.slice(6);
          if (data === '[DONE]') return;
          
          try {
            const parsed = JSON.parse(data);
            const content = parsed.choices?.[0]?.delta?.content;
            if (content) {
              yield content;
            }
          } catch {
            // Skip invalid JSON
          }
        }
      }
    }
  }

  /**
   * Execute a completion with tool calling support
   */
  async executeWithTools(
    messages: CopilotMessage[],
    tools: AgentTool[],
    options: {
      model?: string;
      temperature?: number;
      maxTokens?: number;
      maxIterations?: number;
    } = {}
  ): Promise<{
    content: string;
    toolCalls: Array<{
      name: string;
      arguments: Record<string, unknown>;
      result: unknown;
    }>;
  }> {
    const { maxIterations = COPILOT_MAX_TOOL_ITERATIONS } = options;
    const allToolCalls: Array<{
      name: string;
      arguments: Record<string, unknown>;
      result: unknown;
    }> = [];

    const workingMessages = [...messages];
    const toolDefinitions = tools.map(tool => ({
      type: 'function' as const,
      function: {
        name: tool.name,
        description: tool.description,
        parameters: {
          type: 'object',
          properties: Object.fromEntries(
            Object.entries(tool.parameters).map(([key, param]) => [
              key,
              {
                type: param.type,
                description: param.description,
                ...(param.enum ? { enum: param.enum } : {}),
              },
            ])
          ),
          required: Object.entries(tool.parameters)
            .filter(([, param]) => param.required)
            .map(([key]) => key),
        },
      },
    }));

    for (let i = 0; i < maxIterations; i++) {
      const response = await this.createCompletion({
        messages: workingMessages,
        tools: toolDefinitions,
        model: options.model,
        temperature: options.temperature,
        max_tokens: options.maxTokens,
      });

      const choice = response.choices[0];
      if (!choice) {
        throw new Error('No response from Copilot');
      }

      // Add assistant message
      workingMessages.push({
        role: 'assistant',
        content: choice.message.content || '',
        tool_calls: choice.message.tool_calls,
      });

      // If no tool calls, we're done
      if (choice.finish_reason !== 'tool_calls' || !choice.message.tool_calls) {
        return {
          content: choice.message.content || '',
          toolCalls: allToolCalls,
        };
      }

      // Execute tool calls
      for (const toolCall of choice.message.tool_calls) {
        const tool = tools.find(t => t.name === toolCall.function.name);
        if (!tool) {
          throw new Error(`Unknown tool: ${toolCall.function.name}`);
        }

        const args = JSON.parse(toolCall.function.arguments);
        const result = await tool.execute(args);

        allToolCalls.push({
          name: toolCall.function.name,
          arguments: args,
          result,
        });

        // Add tool result message
        workingMessages.push({
          role: 'tool',
          content: JSON.stringify(result),
          tool_call_id: toolCall.id,
        });
      }
    }

    throw new Error(`Exceeded maximum iterations (${maxIterations})`);
  }
}

// Export singleton instance
let clientInstance: CopilotClient | null = null;

export function getCopilotClient(config?: CopilotClientConfig): CopilotClient {
  if (!clientInstance || config) {
    clientInstance = new CopilotClient(config);
  }
  return clientInstance;
}
