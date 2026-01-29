/**
 * Archeologist Agent
 * 
 * Specializes in understanding and analyzing legacy code.
 * Extracts business rules, identifies patterns, and documents behavior.
 */

import type {
  AgentType,
  BusinessRule,
  DataStructure,
  Procedure,
  SourceLocation,
} from '@migrationpilot/core';
import { BaseAgent, type AgentConfig, type AgentContext, type AgentResponse } from '../common/base-agent.js';
import { getCopilotClient, type ICopilotClient } from '../common/copilot-client.js';
import { modelConfigProvider } from '../common/model-config.js';
import { archeologyTools } from './tools.js';

export interface AnalysisResult {
  // Program structure
  programs: ProgramInfo[];
  dataStructures: DataStructure[];
  procedures: Procedure[];
  
  // Business logic
  businessRules: BusinessRule[];
  
  // Dependencies
  externalCalls: ExternalCall[];
  fileOperations: FileOperation[];
  databaseOperations: DatabaseOperation[];
  
  // Quality metrics
  complexity: ComplexityMetrics;
  
  // Metadata
  summary: string;
  warnings: string[];
  confidence: number;
}

export interface ProgramInfo {
  name: string;
  type: 'main' | 'subprogram' | 'copybook';
  language: string;
  dialect?: string;
  location: SourceLocation;
  description?: string;
}

export interface ExternalCall {
  name: string;
  type: 'call' | 'invoke' | 'exec';
  parameters: string[];
  location: SourceLocation;
}

export interface FileOperation {
  filename: string;
  operation: 'read' | 'write' | 'update' | 'delete';
  recordType?: string;
  location: SourceLocation;
}

export interface DatabaseOperation {
  type: 'select' | 'insert' | 'update' | 'delete' | 'exec';
  table?: string;
  location: SourceLocation;
}

export interface ComplexityMetrics {
  cyclomaticComplexity: number;
  linesOfCode: number;
  commentRatio: number;
  nestingDepth: number;
  procedureCount: number;
  dataItemCount: number;
}

const SYSTEM_PROMPT = `You are a legacy code archeologist with deep expertise in COBOL, Fortran, Visual Basic, and legacy Java systems. Your job is to thoroughly understand legacy code: its structure, business logic, data flows, and hidden rules.

Your primary responsibilities:
1. Parse and understand the structure of legacy code
2. Identify and extract all business rules, no matter how obscure
3. Document data structures and their purposes
4. Map dependencies and external interactions
5. Identify edge cases and special handling

Pay special attention to:
- Implicit rules encoded in variable names or comments
- Edge cases handled by specific code paths
- Temporal logic (end of month, fiscal year, holidays, etc.)
- Industry-specific calculations (interest, amortization, depreciation, etc.)
- Data transformations and formatting
- Error handling and recovery logic

For each business rule you identify:
- Assign a confidence score (0.0 to 1.0)
- Document assumptions
- List edge cases
- Note any ambiguities that need SME clarification

Output your analysis in structured JSON format.`;

export class ArcheologistAgent extends BaseAgent {
  private copilot: ICopilotClient;

  constructor(copilotClient?: ICopilotClient) {
    const modelConfig = modelConfigProvider.getConfig('archeologist');
    const config: AgentConfig = {
      model: modelConfig.model,
      systemPrompt: SYSTEM_PROMPT,
      tools: archeologyTools,
      temperature: modelConfig.temperature,
      maxTokens: modelConfig.maxTokens,
    };
    super(config);
    this.copilot = copilotClient ?? getCopilotClient();
  }

  get type(): AgentType {
    return 'archeologist';
  }

  /**
   * Analyze a source file
   */
  async analyzeFile(
    context: AgentContext,
    sourceCode: string,
    language: string,
    filename: string
  ): Promise<AgentResponse<AnalysisResult>> {
    await this.initialize(context);

    const prompt = `Analyze this ${language} code file and extract all information:

File: ${filename}
Language: ${language}

\`\`\`${language}
${sourceCode}
\`\`\`

Provide a comprehensive analysis including:
1. Program structure (divisions, sections, paragraphs/procedures)
2. All data structures with their types and purposes
3. Every business rule with confidence scores
4. Control flow and decision logic
5. External dependencies (files, databases, other programs)
6. Edge cases and special handling
7. Complexity metrics

Output your analysis as a JSON object matching the AnalysisResult schema.`;

    return this.send(prompt) as Promise<AgentResponse<AnalysisResult>>;
  }

  /**
   * Extract business rules from analyzed code
   */
  async extractBusinessRules(
    context: AgentContext,
    sourceCode: string,
    language: string
  ): Promise<AgentResponse<BusinessRule[]>> {
    await this.initialize(context);

    const prompt = `Extract all business rules from this ${language} code:

\`\`\`${language}
${sourceCode}
\`\`\`

For each rule, provide:
- A unique identifier
- A descriptive name
- The category (calculation, validation, decision, transformation, workflow)
- The source location (lines)
- Input parameters with types
- Output values with types
- The logic in plain language
- Formula if applicable
- Edge cases
- Confidence score (0.0-1.0)

Focus on:
- COMPUTE/calculation statements
- IF/EVALUATE decision logic
- Data validations
- Business-specific formulas
- Workflow/process rules

Output as a JSON array of BusinessRule objects.`;

    return this.send(prompt) as Promise<AgentResponse<BusinessRule[]>>;
  }

  /**
   * Analyze data structures
   */
  async analyzeDataStructures(
    context: AgentContext,
    sourceCode: string,
    language: string
  ): Promise<AgentResponse<DataStructure[]>> {
    await this.initialize(context);

    const prompt = `Analyze the data structures in this ${language} code:

\`\`\`${language}
${sourceCode}
\`\`\`

For ${language === 'cobol' ? 'COBOL' : language}:
${language === 'cobol' ? `
- Parse the DATA DIVISION
- Handle WORKING-STORAGE SECTION
- Handle FILE SECTION
- Parse level numbers (01, 05, 10, etc.)
- Extract PIC clauses and determine data types
- Handle OCCURS clauses for arrays
- Handle REDEFINES
- Handle COPY statements (note the copybook names)
` : ''}

Output as a JSON array of DataStructure objects with:
- name
- type (string, integer, decimal, date, boolean, binary, group, array)
- level (for COBOL)
- picture/format (for COBOL)
- children (for group items)
- location in source`;

    return this.send(prompt) as Promise<AgentResponse<DataStructure[]>>;
  }

  /**
   * Execute the agent logic using Copilot
   */
  protected async execute(_prompt: string): Promise<AgentResponse> {
    return this.executeWithCopilot(this.copilot);
  }
}
