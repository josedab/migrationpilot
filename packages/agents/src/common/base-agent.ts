/**
 * Base Agent Class
 * 
 * Foundation for all MigrationPilot AI agents using GitHub Copilot SDK
 */

import type {
  AgentType,
  AgentSession,
  AgentMessage,
  AgentTool,
  ToolCall,
} from '@migrationpilot/core';
import { generateId, wrapError } from '@migrationpilot/core';
import type { ICopilotClient, CopilotMessage } from './copilot-client.js';

export interface AgentConfig {
  model: string;
  systemPrompt: string;
  tools: AgentTool[];
  temperature?: number;
  maxTokens?: number;
}

export interface AgentContext {
  projectId: string;
  sessionId: string;
  userId: string;
  metadata?: Record<string, unknown>;
}

export interface AgentResponse<T = unknown> {
  success: boolean;
  data?: T;
  error?: string;
  messages: AgentMessage[];
  toolCalls: ToolCall[];
}

export abstract class BaseAgent {
  protected config: AgentConfig;
  protected context: AgentContext | null = null;
  protected session: AgentSession | null = null;
  protected messages: AgentMessage[] = [];

  constructor(config: AgentConfig) {
    this.config = config;
  }

  abstract get type(): AgentType;

  /**
   * Initialize the agent with a project context
   */
  async initialize(context: AgentContext): Promise<void> {
    this.context = context;
    this.session = {
      id: context.sessionId,
      projectId: context.projectId,
      agentType: this.type,
      status: 'active',
      startedAt: new Date(),
      messages: [],
    };
    
    // Add system message
    this.addMessage({
      role: 'system',
      content: this.config.systemPrompt,
      timestamp: new Date(),
    });
  }

  /**
   * Send a message to the agent and get a response
   */
  async send(prompt: string): Promise<AgentResponse> {
    if (!this.session) {
      throw new Error('Agent not initialized. Call initialize() first.');
    }

    // Add user message
    this.addMessage({
      role: 'user',
      content: prompt,
      timestamp: new Date(),
    });

    try {
      // Execute the agent logic (implemented by subclasses)
      const response = await this.execute(prompt);
      
      // Add assistant response
      this.addMessage({
        role: 'assistant',
        content: typeof response.data === 'string' 
          ? response.data 
          : JSON.stringify(response.data, null, 2),
        toolCalls: response.toolCalls,
        timestamp: new Date(),
      });

      return response;
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
      this.session.status = 'failed';
      this.session.error = errorMessage;
      
      return {
        success: false,
        error: errorMessage,
        messages: this.messages,
        toolCalls: [],
      };
    }
  }

  /**
   * Execute the agent's main logic
   * Must be implemented by subclasses
   */
  protected abstract execute(prompt: string): Promise<AgentResponse>;

  /**
   * Execute a tool call
   */
  protected async executeTool(
    toolName: string,
    args: Record<string, unknown>
  ): Promise<unknown> {
    const tool = this.config.tools.find(t => t.name === toolName);
    if (!tool) {
      throw new Error(`Tool not found: ${toolName}`);
    }

    const result = await tool.execute(args);
    
    // Log tool call
    this.addMessage({
      role: 'tool',
      content: JSON.stringify(result),
      toolCalls: [{
        id: generateId(),
        name: toolName,
        arguments: args,
        result,
      }],
      timestamp: new Date(),
    });

    return result;
  }

  /**
   * Add a message to the conversation history
   */
  protected addMessage(message: AgentMessage): void {
    this.messages.push(message);
    if (this.session) {
      this.session.messages.push(message);
    }
  }

  /**
   * Get the current session
   */
  getSession(): AgentSession | null {
    return this.session;
  }

  /**
   * Get conversation history
   */
  getMessages(): AgentMessage[] {
    return [...this.messages];
  }

  /**
   * Complete the session
   */
  complete(result?: unknown): void {
    if (this.session) {
      this.session.status = 'completed';
      this.session.completedAt = new Date();
      this.session.result = result;
    }
  }

  /**
   * Build the messages array for API calls
   */
  protected buildMessages(): Array<{ role: string; content: string }> {
    return this.messages.map(m => ({
      role: m.role,
      content: m.content,
    }));
  }

  /**
   * Get tool definitions for API calls
   */
  protected getToolDefinitions(): Array<{
    type: 'function';
    function: {
      name: string;
      description: string;
      parameters: {
        type: 'object';
        properties: Record<string, unknown>;
        required: string[];
      };
    };
  }> {
    return this.config.tools.map(tool => ({
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
  }

  /**
   * Execute agent logic using a Copilot client
   * 
   * This method encapsulates the common execution pattern used by all agents:
   * 1. Build messages from conversation history
   * 2. Execute with tools via the Copilot client
   * 3. Parse JSON response or return raw content
   * 4. Handle errors consistently
   * 
   * @param copilot - The Copilot client instance to use
   * @returns AgentResponse with parsed data or error
   */
  protected async executeWithCopilot(copilot: ICopilotClient): Promise<AgentResponse> {
    try {
      const messages: CopilotMessage[] = this.buildMessages().map(m => ({
        role: m.role as 'system' | 'user' | 'assistant' | 'tool',
        content: m.content,
      }));

      const result = await copilot.executeWithTools(
        messages,
        this.config.tools,
        {
          model: this.config.model,
          temperature: this.config.temperature,
          maxTokens: this.config.maxTokens,
        }
      );

      // Try to parse response as JSON, fallback to raw content
      let data: unknown;
      try {
        data = JSON.parse(result.content);
      } catch {
        data = result.content;
      }

      return {
        success: true,
        data,
        messages: this.messages,
        toolCalls: result.toolCalls.map((tc, i) => ({
          id: `tc-${i}`,
          name: tc.name,
          arguments: tc.arguments,
          result: tc.result,
        })),
      };
    } catch (error) {
      const wrappedError = wrapError(error, {
        agentType: this.type,
        projectId: this.context?.projectId,
        sessionId: this.context?.sessionId,
      });

      return {
        success: false,
        error: wrappedError.message,
        messages: this.messages,
        toolCalls: [],
      };
    }
  }
}
