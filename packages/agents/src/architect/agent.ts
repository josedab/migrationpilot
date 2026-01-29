/**
 * Architect Agent
 * 
 * Designs modern architecture from legacy code analysis.
 * Creates service boundaries, API contracts, and data models.
 */

import type { AgentType, TargetLanguage } from '@migrationpilot/core';
import { BaseAgent, type AgentConfig, type AgentContext, type AgentResponse } from '../common/base-agent.js';
import { getCopilotClient, type ICopilotClient } from '../common/copilot-client.js';
import { modelConfigProvider } from '../common/model-config.js';
import { architectTools } from './tools.js';
import type { AnalysisResult } from '../archeologist/agent.js';

export interface ArchitectureDesign {
  // Overall design
  overview: string;
  pattern: 'microservices' | 'modular-monolith' | 'layered' | 'hexagonal';
  
  // Services/Modules
  services: ServiceDefinition[];
  
  // Data models
  dataModels: DataModelDefinition[];
  
  // APIs
  apis: APIDefinition[];
  
  // Database schema
  databaseSchema: DatabaseSchema;
  
  // Integration points
  integrations: IntegrationPoint[];
  
  // Migration strategy
  migrationStrategy: MigrationStrategy;
}

export interface ServiceDefinition {
  name: string;
  description: string;
  responsibilities: string[];
  dependencies: string[];
  sourceModules: string[];
  businessRules: string[];
  technology: {
    language: TargetLanguage;
    framework: string;
  };
}

export interface DataModelDefinition {
  name: string;
  description: string;
  fields: FieldDefinition[];
  relationships: RelationshipDefinition[];
  sourceStructures: string[];
}

export interface FieldDefinition {
  name: string;
  type: string;
  nullable: boolean;
  description?: string;
  constraints?: string[];
  sourceField?: string;
}

export interface RelationshipDefinition {
  type: 'one-to-one' | 'one-to-many' | 'many-to-many';
  target: string;
  foreignKey?: string;
}

export interface APIDefinition {
  name: string;
  path: string;
  method: 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  description: string;
  requestBody?: SchemaDefinition;
  responseBody?: SchemaDefinition;
  parameters?: ParameterDefinition[];
  sourceFunction?: string;
}

export interface SchemaDefinition {
  type: string;
  properties?: Record<string, {
    type: string;
    description?: string;
    required?: boolean;
  }>;
}

export interface ParameterDefinition {
  name: string;
  in: 'path' | 'query' | 'header';
  type: string;
  required: boolean;
  description?: string;
}

export interface DatabaseSchema {
  tables: TableDefinition[];
  migrations: string[];
}

export interface TableDefinition {
  name: string;
  columns: ColumnDefinition[];
  primaryKey: string[];
  indexes: IndexDefinition[];
  foreignKeys: ForeignKeyDefinition[];
}

export interface ColumnDefinition {
  name: string;
  type: string;
  nullable: boolean;
  default?: string;
  comment?: string;
}

export interface IndexDefinition {
  name: string;
  columns: string[];
  unique: boolean;
}

export interface ForeignKeyDefinition {
  columns: string[];
  references: {
    table: string;
    columns: string[];
  };
}

export interface IntegrationPoint {
  name: string;
  type: 'api' | 'database' | 'file' | 'message-queue';
  direction: 'inbound' | 'outbound' | 'bidirectional';
  description: string;
  legacyEquivalent?: string;
}

export interface MigrationStrategy {
  approach: 'big-bang' | 'strangler-fig' | 'parallel-run';
  phases: MigrationPhase[];
  risks: RiskAssessment[];
  rollbackPlan: string;
}

export interface MigrationPhase {
  name: string;
  description: string;
  services: string[];
  dependencies: string[];
  validationCriteria: string[];
  estimatedEffort: string;
}

export interface RiskAssessment {
  risk: string;
  impact: 'low' | 'medium' | 'high';
  probability: 'low' | 'medium' | 'high';
  mitigation: string;
}

const SYSTEM_PROMPT = `You are a software architect specializing in modernization projects. Your expertise includes transforming legacy systems into modern, maintainable architectures while preserving business logic.

Your responsibilities:
1. Design target architectures based on legacy code analysis
2. Define service/module boundaries using domain-driven design principles
3. Create data models and database schemas
4. Design API contracts (OpenAPI/REST)
5. Plan migration strategies (strangler fig, parallel run, etc.)

Design principles:
- Favor composition over inheritance
- Design for testability and maintainability
- Apply domain-driven design where appropriate
- Consider microservices boundaries carefully (avoid nano-services)
- Ensure backward compatibility during migration
- Plan for observability and monitoring

For each design decision:
- Explain the rationale
- List alternatives considered
- Identify trade-offs
- Note any risks

Output your designs in structured JSON format.`;

export class ArchitectAgent extends BaseAgent {
  private copilot: ICopilotClient;

  constructor(copilotClient?: ICopilotClient) {
    const modelConfig = modelConfigProvider.getConfig('architect');
    const config: AgentConfig = {
      model: modelConfig.model,
      systemPrompt: SYSTEM_PROMPT,
      tools: architectTools,
      temperature: modelConfig.temperature,
      maxTokens: modelConfig.maxTokens,
    };
    super(config);
    this.copilot = copilotClient ?? getCopilotClient();
  }

  get type(): AgentType {
    return 'architect';
  }

  /**
   * Design architecture from analysis
   */
  async designArchitecture(
    context: AgentContext,
    analysis: AnalysisResult,
    targetLanguage: TargetLanguage,
    targetFramework: string
  ): Promise<AgentResponse<ArchitectureDesign>> {
    await this.initialize(context);

    const prompt = `Design a modern ${targetLanguage} architecture based on this legacy code analysis:

Analysis Summary:
${analysis.summary}

Business Rules: ${analysis.businessRules.length}
Data Structures: ${analysis.dataStructures.length}
Procedures: ${analysis.procedures.length}
External Calls: ${analysis.externalCalls.length}

Target Language: ${targetLanguage}
Target Framework: ${targetFramework}

Design requirements:
1. Module/service structure with clear boundaries
2. Data models mapping from legacy structures
3. API contracts for all business operations
4. Database schema with migrations
5. Integration points with external systems
6. Migration strategy (prefer strangler fig pattern)

For each service, specify:
- Which business rules it implements
- Which legacy modules it replaces
- Dependencies on other services
- API contracts

Output as a JSON object matching the ArchitectureDesign schema.`;

    return this.send(prompt) as Promise<AgentResponse<ArchitectureDesign>>;
  }

  /**
   * Design API contracts
   */
  async designAPIs(
    context: AgentContext,
    services: ServiceDefinition[],
    businessRules: string[]
  ): Promise<AgentResponse<APIDefinition[]>> {
    await this.initialize(context);

    const prompt = `Design REST API contracts for these services:

Services:
${JSON.stringify(services, null, 2)}

Business Rules to expose:
${businessRules.join('\n')}

For each API endpoint:
1. Use RESTful conventions
2. Define request/response schemas
3. Include validation rules
4. Document error responses
5. Map to the business rule it implements

Output as a JSON array of APIDefinition objects.`;

    return this.send(prompt) as Promise<AgentResponse<APIDefinition[]>>;
  }

  /**
   * Design database schema
   */
  async designDatabaseSchema(
    context: AgentContext,
    dataModels: DataModelDefinition[]
  ): Promise<AgentResponse<DatabaseSchema>> {
    await this.initialize(context);

    const prompt = `Design a relational database schema for these data models:

Data Models:
${JSON.stringify(dataModels, null, 2)}

Requirements:
1. Normalize to 3NF where appropriate
2. Define primary keys and foreign keys
3. Add appropriate indexes
4. Include audit columns (created_at, updated_at)
5. Generate migration scripts

Consider:
- Query patterns from the business rules
- Data integrity constraints
- Performance for common operations

Output as a JSON object matching the DatabaseSchema schema.`;

    return this.send(prompt) as Promise<AgentResponse<DatabaseSchema>>;
  }

  /**
   * Execute the agent logic
   */
  protected async execute(_prompt: string): Promise<AgentResponse> {
    return this.executeWithCopilot(this.copilot);
  }
}
