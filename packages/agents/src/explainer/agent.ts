/**
 * Code Explainer Agent
 * 
 * Provides natural language explanations of legacy code with interactive Q&A.
 * Helps SMEs and developers understand complex legacy code sections.
 */

import type {
  AgentType,
  SourceLanguage,
} from '@migrationpilot/core';
import { BaseAgent, type AgentConfig, type AgentContext, type AgentResponse } from '../common/base-agent.js';
import { getCopilotClient } from '../common/copilot-client.js';
import { explainerTools } from './tools.js';

export interface ExplanationResult {
  summary: string;
  detailedExplanation: string;
  businessPurpose: string;
  keyOperations: KeyOperation[];
  dataFlow: DataFlowStep[];
  edgeCases: string[];
  assumptions: string[];
  confidence: number;
  suggestedQuestions: string[];
}

export interface KeyOperation {
  name: string;
  description: string;
  location: { startLine: number; endLine: number };
  importance: 'critical' | 'important' | 'supporting';
}

export interface DataFlowStep {
  step: number;
  description: string;
  inputs: string[];
  outputs: string[];
  transformation?: string;
}

export interface ConversationMessage {
  role: 'user' | 'assistant';
  content: string;
  timestamp: Date;
}

export interface QASession {
  sessionId: string;
  codeContext: string;
  language: SourceLanguage;
  filename: string;
  conversationHistory: ConversationMessage[];
  explanationCache: Map<string, ExplanationResult>;
}

const SYSTEM_PROMPT = `You are an expert legacy code educator with deep knowledge of COBOL, Fortran, Visual Basic, and legacy Java systems. Your role is to explain complex legacy code in clear, accessible language that both technical and non-technical stakeholders can understand.

Your core responsibilities:
1. Explain what code does in plain language
2. Identify the business purpose behind technical implementations
3. Describe data flows and transformations
4. Highlight critical operations and edge cases
5. Answer follow-up questions maintaining context

Guidelines for explanations:
- Start with a high-level summary (1-2 sentences)
- Then provide detailed explanation with business context
- Use analogies when helpful for complex concepts
- Point out potential issues or concerns
- Always reference specific line numbers when relevant
- Be honest about uncertainty - if something is ambiguous, say so

For COBOL specifically:
- Explain COBOL division structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Clarify PICTURE clauses and data types
- Explain PERFORM statements and control flow
- Describe file handling (FD, SELECT, OPEN, READ, WRITE)
- Explain COPYBOOK usage

For follow-up questions:
- Maintain context from previous explanations
- Reference the specific code being discussed
- Build on previous answers rather than repeating
- Suggest related questions the user might want to ask

Output JSON with the following structure for explanations.`;

export class ExplainerAgent extends BaseAgent {
  private copilot = getCopilotClient();
  private sessions: Map<string, QASession> = new Map();

  constructor() {
    const config: AgentConfig = {
      model: 'gpt-4',
      systemPrompt: SYSTEM_PROMPT,
      tools: explainerTools,
      temperature: 0.3, // Slightly higher for more natural explanations
      maxTokens: 4000,
    };
    super(config);
  }

  get type(): AgentType {
    return 'archeologist'; // Reuse archeologist type for now
  }

  /**
   * Start a new Q&A session for a code file
   */
  startSession(
    sessionId: string,
    code: string,
    language: SourceLanguage,
    filename: string
  ): QASession {
    const session: QASession = {
      sessionId,
      codeContext: code,
      language,
      filename,
      conversationHistory: [],
      explanationCache: new Map(),
    };
    this.sessions.set(sessionId, session);
    return session;
  }

  /**
   * Get or create a session (renamed to avoid conflict with base class)
   */
  getQASession(sessionId: string): QASession | undefined {
    return this.sessions.get(sessionId);
  }

  /**
   * Explain a complete code file
   */
  async explainCode(
    context: AgentContext,
    sourceCode: string,
    language: SourceLanguage,
    filename: string
  ): Promise<AgentResponse<ExplanationResult>> {
    await this.initialize(context);

    const prompt = `Please explain this ${language.toUpperCase()} code file in detail:

File: ${filename}
Language: ${language}

\`\`\`${language}
${sourceCode}
\`\`\`

Provide a comprehensive explanation including:
1. A brief summary (1-2 sentences)
2. Detailed explanation of what the code does
3. The business purpose it serves
4. Key operations with their line numbers and importance
5. Data flow through the program
6. Edge cases and special handling
7. Any assumptions made in the code
8. Confidence score (0.0-1.0) for your explanation
9. Suggested follow-up questions

Output as JSON matching ExplanationResult schema.`;

    return this.send(prompt) as Promise<AgentResponse<ExplanationResult>>;
  }

  /**
   * Explain a specific code section (e.g., a paragraph, function, or block)
   */
  async explainSection(
    context: AgentContext,
    sourceCode: string,
    language: SourceLanguage,
    filename: string,
    startLine: number,
    endLine: number
  ): Promise<AgentResponse<ExplanationResult>> {
    await this.initialize(context);

    const lines = sourceCode.split('\n');
    const section = lines.slice(startLine - 1, endLine).join('\n');
    const contextBefore = lines.slice(Math.max(0, startLine - 10), startLine - 1).join('\n');
    const contextAfter = lines.slice(endLine, Math.min(lines.length, endLine + 5)).join('\n');

    const prompt = `Please explain this specific section of ${language.toUpperCase()} code (lines ${startLine}-${endLine}):

File: ${filename}

Context before (lines ${Math.max(1, startLine - 9)}-${startLine - 1}):
\`\`\`${language}
${contextBefore}
\`\`\`

TARGET SECTION (lines ${startLine}-${endLine}):
\`\`\`${language}
${section}
\`\`\`

Context after (lines ${endLine + 1}-${Math.min(lines.length, endLine + 5)}):
\`\`\`${language}
${contextAfter}
\`\`\`

Focus your explanation on the TARGET SECTION while using context to provide better understanding.

Output as JSON matching ExplanationResult schema.`;

    return this.send(prompt) as Promise<AgentResponse<ExplanationResult>>;
  }

  /**
   * Answer a follow-up question about code
   */
  async askQuestion(
    context: AgentContext,
    sessionId: string,
    question: string
  ): Promise<AgentResponse<{
    answer: string;
    relatedCode?: { startLine: number; endLine: number; snippet: string };
    confidence: number;
    suggestedFollowups: string[];
  }>> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      return {
        success: false,
        error: 'Session not found. Start a new session first.',
        messages: [],
        toolCalls: [],
      };
    }

    await this.initialize(context);

    // Build conversation context
    const conversationContext = session.conversationHistory
      .slice(-5) // Keep last 5 exchanges for context
      .map(m => `${m.role.toUpperCase()}: ${m.content}`)
      .join('\n\n');

    const prompt = `You are continuing a Q&A session about the following ${session.language.toUpperCase()} code:

File: ${session.filename}

\`\`\`${session.language}
${session.codeContext}
\`\`\`

${conversationContext ? `Previous conversation:\n${conversationContext}\n\n` : ''}

New question: ${question}

Please provide:
1. A clear, helpful answer to the question
2. Reference to specific code lines if relevant (provide line numbers and snippets)
3. Confidence score (0.0-1.0)
4. 2-3 suggested follow-up questions

Output as JSON with fields: answer, relatedCode (optional), confidence, suggestedFollowups`;

    const response = await this.send(prompt);

    // Update conversation history
    session.conversationHistory.push(
      { role: 'user', content: question, timestamp: new Date() },
      { role: 'assistant', content: typeof response.data === 'string' ? response.data : JSON.stringify(response.data), timestamp: new Date() }
    );

    return response as AgentResponse<{
      answer: string;
      relatedCode?: { startLine: number; endLine: number; snippet: string };
      confidence: number;
      suggestedFollowups: string[];
    }>;
  }

  /**
   * Get explanation for a specific COBOL paragraph/section
   */
  async explainParagraph(
    context: AgentContext,
    sourceCode: string,
    paragraphName: string,
    filename: string
  ): Promise<AgentResponse<ExplanationResult>> {
    await this.initialize(context);

    const prompt = `Find and explain the COBOL paragraph named "${paragraphName}" in this code:

File: ${filename}

\`\`\`cobol
${sourceCode}
\`\`\`

Provide:
1. What this paragraph does
2. When/why it gets called (look for PERFORM statements)
3. Data it reads and modifies
4. Business purpose
5. Any edge cases or conditions

Output as JSON matching ExplanationResult schema.`;

    return this.send(prompt) as Promise<AgentResponse<ExplanationResult>>;
  }

  /**
   * Explain the purpose of a data item/variable
   */
  async explainDataItem(
    context: AgentContext,
    sourceCode: string,
    dataItemName: string,
    language: SourceLanguage,
    filename: string
  ): Promise<AgentResponse<{
    name: string;
    type: string;
    purpose: string;
    usedIn: Array<{ location: string; usage: 'read' | 'write' | 'both' }>;
    relatedItems: string[];
    businessMeaning: string;
    confidence: number;
  }>> {
    await this.initialize(context);

    const prompt = `Analyze the data item/variable "${dataItemName}" in this ${language.toUpperCase()} code:

File: ${filename}

\`\`\`${language}
${sourceCode}
\`\`\`

Provide:
1. Name and type of the data item
2. Its purpose in the program
3. Where it's used (read, written, or both) with locations
4. Related data items it interacts with
5. Business meaning of this data
6. Confidence score (0.0-1.0)

Output as JSON.`;

    return this.send(prompt) as Promise<AgentResponse<{
      name: string;
      type: string;
      purpose: string;
      usedIn: Array<{ location: string; usage: 'read' | 'write' | 'both' }>;
      relatedItems: string[];
      businessMeaning: string;
      confidence: number;
    }>>;
  }

  /**
   * Generate executive summary for non-technical stakeholders
   */
  async generateExecutiveSummary(
    context: AgentContext,
    sourceCode: string,
    language: SourceLanguage,
    filename: string
  ): Promise<AgentResponse<{
    title: string;
    summary: string;
    businessFunctions: string[];
    dataProcessed: string[];
    integrations: string[];
    risks: string[];
    modernizationNotes: string;
  }>> {
    await this.initialize(context);

    const prompt = `Generate an executive summary of this ${language.toUpperCase()} program for non-technical stakeholders:

File: ${filename}

\`\`\`${language}
${sourceCode}
\`\`\`

Create a business-focused summary that includes:
1. A descriptive title
2. 2-3 sentence summary of what this program does
3. Key business functions it performs
4. Types of data it processes
5. External systems or files it integrates with
6. Potential risks or concerns
7. Brief notes on modernization considerations

Use plain language, avoid technical jargon. Output as JSON.`;

    return this.send(prompt) as Promise<AgentResponse<{
      title: string;
      summary: string;
      businessFunctions: string[];
      dataProcessed: string[];
      integrations: string[];
      risks: string[];
      modernizationNotes: string;
    }>>;
  }

  /**
   * Execute the agent logic using Copilot
   */
  protected async execute(_prompt: string): Promise<AgentResponse> {
    return this.executeWithCopilot(this.copilot);
  }
}
