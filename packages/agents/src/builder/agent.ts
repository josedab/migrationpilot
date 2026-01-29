/**
 * Builder Agent
 * 
 * Generates modern code from architecture designs.
 * Creates source files, tests, and documentation.
 */

import type { AgentType, TargetLanguage, GeneratedFile } from '@migrationpilot/core';
import { BaseAgent, type AgentConfig, type AgentContext, type AgentResponse } from '../common/base-agent.js';
import { getCopilotClient, type ICopilotClient } from '../common/copilot-client.js';
import { modelConfigProvider } from '../common/model-config.js';
import { builderTools } from './tools.js';
import type { ServiceDefinition, APIDefinition } from '../architect/agent.js';

export interface GenerationResult {
  files: GeneratedFile[];
  summary: {
    totalFiles: number;
    sourceFiles: number;
    testFiles: number;
    configFiles: number;
    documentationFiles: number;
    totalLines: number;
  };
  warnings: string[];
}

export interface CodeGenerationOptions {
  targetLanguage: TargetLanguage;
  framework: string;
  includeTests: boolean;
  includeDocumentation: boolean;
  codeStyle?: 'verbose' | 'concise';
  testFramework?: string;
}

const SYSTEM_PROMPT = `You are an expert software developer specializing in code generation for legacy modernization projects. You generate clean, idiomatic, well-documented code.

Your responsibilities:
1. Generate production-quality source code in the target language
2. Create comprehensive unit and integration tests
3. Write clear documentation and comments
4. Ensure code is traceable to legacy source

Code generation principles:
1. **Traceability:** Every generated function includes comments linking to source
2. **Readability:** Optimize for human understanding, not just correctness
3. **Testability:** Design for easy unit testing with dependency injection
4. **Modularity:** Follow single responsibility principle

For each generated file:
- Include file-level documentation with purpose and legacy mapping
- Add function-level docs with business rule references
- Generate corresponding tests
- Follow language-specific best practices and conventions

Output generated code as JSON with file paths and content.`;

export class BuilderAgent extends BaseAgent {
  private copilot: ICopilotClient;

  constructor(copilotClient?: ICopilotClient) {
    const modelConfig = modelConfigProvider.getConfig('builder');
    const config: AgentConfig = {
      model: modelConfig.model,
      systemPrompt: SYSTEM_PROMPT,
      tools: builderTools,
      temperature: modelConfig.temperature,
      maxTokens: modelConfig.maxTokens,
    };
    super(config);
    this.copilot = copilotClient ?? getCopilotClient();
  }

  get type(): AgentType {
    return 'builder';
  }

  /**
   * Generate code for a service
   */
  async generateService(
    context: AgentContext,
    service: ServiceDefinition,
    apis: APIDefinition[],
    options: CodeGenerationOptions
  ): Promise<AgentResponse<GenerationResult>> {
    await this.initialize(context);

    const prompt = `Generate ${options.targetLanguage} code for this service:

Service: ${service.name}
Description: ${service.description}
Framework: ${options.framework}

Responsibilities:
${service.responsibilities.map(r => `- ${r}`).join('\n')}

Business Rules to implement:
${service.businessRules.map(r => `- ${r}`).join('\n')}

API Endpoints:
${JSON.stringify(apis, null, 2)}

Generate:
1. Main service class/module
2. Data transfer objects (DTOs)
3. Repository/data access layer
4. Business logic layer
5. Controller/handler layer
${options.includeTests ? '6. Unit tests for all components' : ''}
${options.includeDocumentation ? '7. API documentation' : ''}

Use ${options.framework} conventions and best practices.

Output as JSON with:
{
  "files": [
    {
      "path": "src/services/...",
      "content": "...",
      "type": "source|test|config|documentation"
    }
  ]
}`;

    return this.send(prompt) as Promise<AgentResponse<GenerationResult>>;
  }

  /**
   * Generate a single function/method
   */
  async generateFunction(
    context: AgentContext,
    specification: {
      name: string;
      description: string;
      inputs: Array<{ name: string; type: string }>;
      outputs: Array<{ name: string; type: string }>;
      logic: string;
      sourceReference?: string;
    },
    options: CodeGenerationOptions
  ): Promise<AgentResponse<{ code: string; tests: string }>> {
    await this.initialize(context);

    const prompt = `Generate a ${options.targetLanguage} function:

Name: ${specification.name}
Description: ${specification.description}
${specification.sourceReference ? `Source Reference: ${specification.sourceReference}` : ''}

Inputs:
${specification.inputs.map(i => `- ${i.name}: ${i.type}`).join('\n')}

Outputs:
${specification.outputs.map(o => `- ${o.name}: ${o.type}`).join('\n')}

Business Logic:
${specification.logic}

Generate:
1. The function implementation with full documentation
2. Unit tests covering all code paths and edge cases

Follow ${options.targetLanguage} best practices.

Output as JSON:
{
  "code": "...",
  "tests": "..."
}`;

    return this.send(prompt) as Promise<AgentResponse<{ code: string; tests: string }>>;
  }

  /**
   * Generate data models
   */
  async generateDataModels(
    context: AgentContext,
    models: Array<{
      name: string;
      fields: Array<{ name: string; type: string; nullable?: boolean }>;
      sourceStructure?: string;
    }>,
    options: CodeGenerationOptions
  ): Promise<AgentResponse<GenerationResult>> {
    await this.initialize(context);

    const prompt = `Generate ${options.targetLanguage} data models:

Models:
${JSON.stringify(models, null, 2)}

Framework: ${options.framework}

Generate for each model:
1. Model/Entity class with proper annotations
2. Validation rules
3. Serialization support
4. Builder pattern (if appropriate)
${options.includeTests ? '5. Unit tests' : ''}

Follow ${options.framework} conventions.

Output as JSON with file paths and content.`;

    return this.send(prompt) as Promise<AgentResponse<GenerationResult>>;
  }

  /**
   * Generate database migrations
   */
  async generateMigrations(
    context: AgentContext,
    schema: {
      tables: Array<{
        name: string;
        columns: Array<{ name: string; type: string; nullable: boolean }>;
      }>;
    },
    databaseType: 'postgresql' | 'mysql' | 'sqlserver'
  ): Promise<AgentResponse<GenerationResult>> {
    await this.initialize(context);

    const prompt = `Generate database migrations for ${databaseType}:

Schema:
${JSON.stringify(schema, null, 2)}

Generate:
1. Initial migration to create all tables
2. Index creation migrations
3. Rollback scripts for each migration

Use a standard migration format (timestamp-based naming).

Output as JSON with file paths and SQL content.`;

    return this.send(prompt) as Promise<AgentResponse<GenerationResult>>;
  }

  /**
   * Execute the agent logic
   */
  protected async execute(_prompt: string): Promise<AgentResponse> {
    return this.executeWithCopilot(this.copilot);
  }
}
