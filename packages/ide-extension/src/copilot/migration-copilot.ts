/**
 * Real-Time Migration Copilot
 * 
 * AI-powered inline assistance for legacy code understanding and migration.
 * Provides streaming suggestions, explanations, and conversational help.
 */

import type { Range } from '../index.js';

// ============================================================================
// Copilot Types
// ============================================================================

export interface CopilotConfig {
  apiEndpoint?: string;
  modelId?: string;
  maxTokens?: number;
  temperature?: number;
  enableStreaming?: boolean;
  cacheResponses?: boolean;
}

export interface CopilotSession {
  id: string;
  projectId: string;
  documentUri: string;
  language: string;
  startedAt: Date;
  messages: CopilotMessage[];
  context: SessionContext;
}

export interface CopilotMessage {
  id: string;
  role: 'user' | 'assistant' | 'system';
  content: string;
  timestamp: Date;
  metadata?: {
    range?: Range;
    ruleId?: string;
    confidence?: number;
  };
}

export interface SessionContext {
  documentContent: string;
  cursorPosition: { line: number; column: number };
  selectedRange?: Range;
  visibleRange?: Range;
  extractedRules?: ExtractedRule[];
  recentEdits?: RecentEdit[];
}

export interface ExtractedRule {
  id: string;
  name: string;
  type: 'calculation' | 'validation' | 'transformation' | 'workflow';
  description: string;
  sourceCode: string;
  range: Range;
  confidence: number;
}

export interface RecentEdit {
  range: Range;
  oldText: string;
  newText: string;
  timestamp: Date;
}

// ============================================================================
// Suggestion Types
// ============================================================================

export interface InlineSuggestion {
  id: string;
  range: Range;
  text: string;
  displayText: string;
  type: 'completion' | 'explanation' | 'refactor' | 'migration';
  confidence: number;
  source: 'rule' | 'pattern' | 'ai';
  metadata?: Record<string, unknown>;
}

export interface CodeExplanation {
  range: Range;
  summary: string;
  details: string;
  businessContext?: string;
  dataFlow?: DataFlowInfo[];
  relatedRules?: string[];
  migrationNotes?: string;
}

export interface DataFlowInfo {
  variable: string;
  type: 'input' | 'output' | 'intermediate';
  sourceRange?: Range;
  transformations?: string[];
}

export interface MigrationSuggestion {
  id: string;
  originalRange: Range;
  originalCode: string;
  suggestedCode: string;
  targetLanguage: string;
  explanation: string;
  confidence: number;
  warnings?: string[];
  testCases?: SuggestedTestCase[];
}

export interface SuggestedTestCase {
  name: string;
  inputs: Record<string, unknown>;
  expectedOutputs: Record<string, unknown>;
  description: string;
}

// ============================================================================
// Copilot Service
// ============================================================================

export class MigrationCopilot {
  private config: CopilotConfig;
  private sessions: Map<string, CopilotSession> = new Map();
  private suggestionCache: Map<string, InlineSuggestion[]> = new Map();

  constructor(config: CopilotConfig = {}) {
    this.config = {
      maxTokens: 4096,
      temperature: 0.3,
      enableStreaming: true,
      cacheResponses: true,
      ...config,
    };
  }

  /**
   * Start a new copilot session
   */
  startSession(
    projectId: string,
    documentUri: string,
    language: string,
    content: string
  ): CopilotSession {
    const session: CopilotSession = {
      id: `session_${Date.now()}_${Math.random().toString(36).slice(2)}`,
      projectId,
      documentUri,
      language,
      startedAt: new Date(),
      messages: [],
      context: {
        documentContent: content,
        cursorPosition: { line: 1, column: 1 },
      },
    };

    // Add system context
    session.messages.push({
      id: 'system_1',
      role: 'system',
      content: this.buildSystemPrompt(language),
      timestamp: new Date(),
    });

    this.sessions.set(session.id, session);
    return session;
  }

  /**
   * Get inline suggestions at cursor position
   */
  async getInlineSuggestions(
    sessionId: string,
    line: number,
    column: number,
    triggerKind: 'manual' | 'automatic'
  ): Promise<InlineSuggestion[]> {
    const session = this.sessions.get(sessionId);
    if (!session) return [];

    const cacheKey = `${sessionId}:${line}:${column}`;
    if (this.config.cacheResponses && this.suggestionCache.has(cacheKey)) {
      return this.suggestionCache.get(cacheKey)!;
    }

    const suggestions: InlineSuggestion[] = [];
    const context = this.getLineContext(session.context.documentContent, line);

    // Generate contextual suggestions
    if (context.isInCalculation) {
      suggestions.push({
        id: `sug_calc_${Date.now()}`,
        range: { startLine: line, startColumn: column, endLine: line, endColumn: column },
        text: '/* Extracted as business rule: BR-001 */',
        displayText: '💡 Extract as business rule',
        type: 'refactor',
        confidence: 0.85,
        source: 'pattern',
      });
    }

    if (context.isInCondition && triggerKind === 'manual') {
      suggestions.push({
        id: `sug_cond_${Date.now()}`,
        range: { startLine: line, startColumn: column, endLine: line, endColumn: column },
        text: '',
        displayText: '📖 Explain this condition',
        type: 'explanation',
        confidence: 0.9,
        source: 'ai',
      });
    }

    if (this.config.cacheResponses) {
      this.suggestionCache.set(cacheKey, suggestions);
    }

    return suggestions;
  }

  /**
   * Get explanation for selected code
   */
  async explainCode(
    sessionId: string,
    range: Range
  ): Promise<CodeExplanation> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    const selectedCode = this.extractCodeInRange(
      session.context.documentContent,
      range
    );

    // Analyze the code structure
    const analysis = this.analyzeCodeStructure(selectedCode, session.language);

    // Build explanation
    const explanation: CodeExplanation = {
      range,
      summary: analysis.summary,
      details: analysis.details,
      businessContext: analysis.businessContext,
      dataFlow: analysis.dataFlow,
      migrationNotes: this.generateMigrationNotes(analysis, session.language),
    };

    // Record in session
    session.messages.push({
      id: `msg_${Date.now()}`,
      role: 'assistant',
      content: explanation.summary,
      timestamp: new Date(),
      metadata: { range },
    });

    return explanation;
  }

  /**
   * Get migration suggestion for selected code
   */
  async getMigrationSuggestion(
    sessionId: string,
    range: Range,
    targetLanguage: string
  ): Promise<MigrationSuggestion> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    const originalCode = this.extractCodeInRange(
      session.context.documentContent,
      range
    );

    // Generate migrated code based on language
    const migrated = this.generateMigratedCode(
      originalCode,
      session.language,
      targetLanguage
    );

    return {
      id: `mig_${Date.now()}`,
      originalRange: range,
      originalCode,
      suggestedCode: migrated.code,
      targetLanguage,
      explanation: migrated.explanation,
      confidence: migrated.confidence,
      warnings: migrated.warnings,
      testCases: migrated.testCases,
    };
  }

  /**
   * Ask a question in the chat context
   */
  async chat(
    sessionId: string,
    message: string,
    range?: Range
  ): Promise<CopilotMessage> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    // Add user message
    const userMessage: CopilotMessage = {
      id: `msg_user_${Date.now()}`,
      role: 'user',
      content: message,
      timestamp: new Date(),
      metadata: range ? { range } : undefined,
    };
    session.messages.push(userMessage);

    // Generate response based on context
    const response = await this.generateChatResponse(session, message, range);

    const assistantMessage: CopilotMessage = {
      id: `msg_assistant_${Date.now()}`,
      role: 'assistant',
      content: response,
      timestamp: new Date(),
    };
    session.messages.push(assistantMessage);

    return assistantMessage;
  }

  /**
   * Stream suggestions as user types
   */
  async *streamSuggestions(
    sessionId: string,
    prefix: string,
    line: number,
    _column: number
  ): AsyncGenerator<InlineSuggestion, void, unknown> {
    const session = this.sessions.get(sessionId);
    if (!session) return;

    // Simulate streaming suggestions
    const suggestions = await this.generateCompletions(session, prefix, line);

    for (const suggestion of suggestions) {
      yield suggestion;
      // Small delay to simulate streaming
      await new Promise(resolve => setTimeout(resolve, 50));
    }
  }

  /**
   * Update session context
   */
  updateContext(
    sessionId: string,
    updates: Partial<SessionContext>
  ): void {
    const session = this.sessions.get(sessionId);
    if (!session) return;

    session.context = { ...session.context, ...updates };

    // Clear cache when context changes
    this.clearCacheForSession(sessionId);
  }

  /**
   * Close session
   */
  closeSession(sessionId: string): void {
    this.sessions.delete(sessionId);
    this.clearCacheForSession(sessionId);
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private buildSystemPrompt(language: string): string {
    return `You are a migration assistant for ${language} code. Your role is to:
1. Help developers understand legacy code patterns and business logic
2. Explain complex conditions, calculations, and data flows
3. Suggest modern equivalents for legacy constructs
4. Identify potential migration issues and edge cases
5. Generate test cases for business rules

Always be concise and focus on actionable insights.`;
  }

  private getLineContext(content: string, line: number): {
    isInCalculation: boolean;
    isInCondition: boolean;
    isInLoop: boolean;
    currentLine: string;
    surroundingLines: string[];
  } {
    const lines = content.split('\n');
    const currentLine = lines[line - 1] || '';
    const start = Math.max(0, line - 3);
    const end = Math.min(lines.length, line + 2);
    const surroundingLines = lines.slice(start, end);

    const upperLine = currentLine.toUpperCase();

    return {
      isInCalculation: /COMPUTE|CALC|=.*[+\-*/]/.test(upperLine),
      isInCondition: /IF|WHEN|EVALUATE|CASE/.test(upperLine),
      isInLoop: /PERFORM|DO|LOOP|UNTIL/.test(upperLine),
      currentLine,
      surroundingLines,
    };
  }

  private extractCodeInRange(content: string, range: Range): string {
    const lines = content.split('\n');
    const extracted: string[] = [];

    for (let i = range.startLine - 1; i < range.endLine; i++) {
      if (lines[i] !== undefined) {
        if (i === range.startLine - 1) {
          extracted.push(lines[i]!.slice(range.startColumn - 1));
        } else if (i === range.endLine - 1) {
          extracted.push(lines[i]!.slice(0, range.endColumn));
        } else {
          extracted.push(lines[i]!);
        }
      }
    }

    return extracted.join('\n');
  }

  private analyzeCodeStructure(
    code: string,
    _language: string
  ): {
    summary: string;
    details: string;
    businessContext?: string;
    dataFlow: DataFlowInfo[];
  } {
    const upperCode = code.toUpperCase();
    const dataFlow: DataFlowInfo[] = [];

    // Detect pattern type
    let summary = 'Code block analysis';
    let details = '';
    let businessContext: string | undefined;

    if (/COMPUTE|CALCULATE|ADD|SUBTRACT|MULTIPLY|DIVIDE/.test(upperCode)) {
      summary = 'Business Calculation';
      details = 'This code performs arithmetic operations that likely implement a business rule.';
      businessContext = 'May represent pricing, fee calculation, or financial computation.';
      
      // Extract variables
      const varMatches = code.match(/\b([A-Z][A-Z0-9-]+)\b/gi) || [];
      const uniqueVars = [...new Set(varMatches)];
      for (const v of uniqueVars.slice(0, 5)) {
        dataFlow.push({
          variable: v,
          type: code.indexOf(v) < code.length / 2 ? 'input' : 'output',
        });
      }
    } else if (/IF|EVALUATE|WHEN|ELSE/.test(upperCode)) {
      summary = 'Decision Logic';
      details = 'This code implements conditional logic for business decisions.';
      businessContext = 'Controls process flow based on business conditions.';
    } else if (/PERFORM|CALL|INVOKE/.test(upperCode)) {
      summary = 'Procedure Call';
      details = 'This code invokes another procedure or subroutine.';
    } else if (/READ|WRITE|FETCH|INSERT|UPDATE|DELETE/.test(upperCode)) {
      summary = 'Data Access';
      details = 'This code performs database or file I/O operations.';
      businessContext = 'Accesses persistent business data.';
    }

    return { summary, details, businessContext, dataFlow };
  }

  private generateMigrationNotes(
    analysis: { summary: string; details: string; dataFlow: DataFlowInfo[] },
    sourceLanguage: string
  ): string {
    const notes: string[] = [];

    if (sourceLanguage === 'cobol') {
      notes.push('• COBOL decimal precision should be preserved in target language');
      notes.push('• Consider BigDecimal for Java or Decimal for Python');
    }

    if (analysis.dataFlow.length > 0) {
      notes.push(`• ${analysis.dataFlow.length} variables identified for mapping`);
    }

    notes.push('• Add unit tests before and after migration');
    notes.push('• Verify edge cases with original SMEs');

    return notes.join('\n');
  }

  private generateMigratedCode(
    originalCode: string,
    sourceLanguage: string,
    targetLanguage: string
  ): {
    code: string;
    explanation: string;
    confidence: number;
    warnings: string[];
    testCases: SuggestedTestCase[];
  } {
    // Simple template-based migration for demonstration
    let code = '';
    const warnings: string[] = [];
    const testCases: SuggestedTestCase[] = [];

    if (sourceLanguage === 'cobol' && targetLanguage === 'java') {
      code = this.cobolToJavaTemplate(originalCode);
      warnings.push('Verify decimal precision handling');
      warnings.push('Check for COMP-3 packed decimal conversions');
    } else if (sourceLanguage === 'cobol' && targetLanguage === 'python') {
      code = this.cobolToPythonTemplate(originalCode);
      warnings.push('Consider using Decimal module for precision');
    } else {
      code = `// Migrated from ${sourceLanguage}\n// Original:\n${originalCode.split('\n').map(l => '// ' + l).join('\n')}\n\n// TODO: Implement migration`;
    }

    // Generate basic test case
    testCases.push({
      name: 'test_basic_functionality',
      inputs: { input1: 'example' },
      expectedOutputs: { result: 'expected' },
      description: 'Basic functionality test - verify with SME',
    });

    return {
      code,
      explanation: `Migrated ${sourceLanguage.toUpperCase()} code to ${targetLanguage}. Review all warnings and test thoroughly.`,
      confidence: 0.7,
      warnings,
      testCases,
    };
  }

  private cobolToJavaTemplate(cobol: string): string {
    const lines = cobol.split('\n');
    const javaLines: string[] = ['public class MigratedLogic {', '    '];

    for (const line of lines) {
      const upper = line.toUpperCase().trim();
      
      if (upper.startsWith('COMPUTE')) {
        const match = line.match(/COMPUTE\s+(\S+)\s*=\s*(.+)/i);
        if (match) {
          javaLines.push(`    BigDecimal ${this.toJavaVar(match[1] || 'result')} = ${this.toJavaExpr(match[2] || '0')};`);
        }
      } else if (upper.startsWith('IF')) {
        const condition = line.replace(/^IF\s+/i, '').replace(/\s+THEN\s*$/i, '');
        javaLines.push(`    if (${this.toJavaCondition(condition)}) {`);
      } else if (upper.startsWith('ELSE')) {
        javaLines.push('    } else {');
      } else if (upper.startsWith('END-IF')) {
        javaLines.push('    }');
      } else if (upper.startsWith('MOVE')) {
        const match = line.match(/MOVE\s+(\S+)\s+TO\s+(\S+)/i);
        if (match) {
          javaLines.push(`    ${this.toJavaVar(match[2] || 'target')} = ${this.toJavaVar(match[1] || 'source')};`);
        }
      } else if (line.trim()) {
        javaLines.push(`    // TODO: ${line.trim()}`);
      }
    }

    javaLines.push('}');
    return javaLines.join('\n');
  }

  private cobolToPythonTemplate(cobol: string): string {
    const lines = cobol.split('\n');
    const pyLines: string[] = ['from decimal import Decimal', '', 'def migrated_logic():'];

    for (const line of lines) {
      const upper = line.toUpperCase().trim();
      
      if (upper.startsWith('COMPUTE')) {
        const match = line.match(/COMPUTE\s+(\S+)\s*=\s*(.+)/i);
        if (match) {
          pyLines.push(`    ${this.toPythonVar(match[1] || 'result')} = Decimal("${match[2] || '0'}")`);
        }
      } else if (upper.startsWith('IF')) {
        const condition = line.replace(/^IF\s+/i, '').replace(/\s+THEN\s*$/i, '');
        pyLines.push(`    if ${this.toPythonCondition(condition)}:`);
      } else if (upper.startsWith('ELSE')) {
        pyLines.push('    else:');
      } else if (upper.startsWith('END-IF')) {
        pyLines.push('        pass  # end if');
      } else if (line.trim()) {
        pyLines.push(`    # TODO: ${line.trim()}`);
      }
    }

    return pyLines.join('\n');
  }

  private toJavaVar(cobolVar: string): string {
    return cobolVar.toLowerCase().replace(/-/g, '_');
  }

  private toPythonVar(cobolVar: string): string {
    return cobolVar.toLowerCase().replace(/-/g, '_');
  }

  private toJavaExpr(expr: string): string {
    return `new BigDecimal("${expr.trim()}")`;
  }

  private toJavaCondition(condition: string): string {
    return condition
      .replace(/\s+EQUAL\s+TO\s+/gi, ' == ')
      .replace(/\s+GREATER\s+THAN\s+/gi, ' > ')
      .replace(/\s+LESS\s+THAN\s+/gi, ' < ')
      .replace(/\s+NOT\s+/gi, ' !')
      .replace(/\s+AND\s+/gi, ' && ')
      .replace(/\s+OR\s+/gi, ' || ');
  }

  private toPythonCondition(condition: string): string {
    return condition
      .replace(/\s+EQUAL\s+TO\s+/gi, ' == ')
      .replace(/\s+GREATER\s+THAN\s+/gi, ' > ')
      .replace(/\s+LESS\s+THAN\s+/gi, ' < ')
      .replace(/\s+NOT\s+/gi, ' not ')
      .replace(/\s+AND\s+/gi, ' and ')
      .replace(/\s+OR\s+/gi, ' or ');
  }

  private async generateChatResponse(
    session: CopilotSession,
    message: string,
    range?: Range
  ): Promise<string> {
    const lowerMessage = message.toLowerCase();
    let context = '';
    
    if (range) {
      context = this.extractCodeInRange(session.context.documentContent, range);
    }

    // Simple response generation based on keywords
    if (lowerMessage.includes('explain')) {
      return `Based on my analysis of this ${session.language.toUpperCase()} code${context ? ` at the selected location` : ''}:

This appears to be a business logic implementation. Key observations:
- The code structure suggests a calculation or validation routine
- Data flows from input parameters through transformations to output
- Consider extracting this as a named business rule for documentation

Would you like me to generate test cases or migration code?`;
    }

    if (lowerMessage.includes('migrate') || lowerMessage.includes('convert')) {
      return `I can help migrate this code. To proceed:

1. Select the code you want to migrate
2. Choose target language (Java, Python, TypeScript, Go, C#)
3. I'll generate the equivalent code with:
   - Preserved business logic
   - Appropriate data types
   - Suggested test cases

What target language would you prefer?`;
    }

    if (lowerMessage.includes('test')) {
      return `For testing this code, I recommend:

1. **Boundary tests**: Test edge values for numeric inputs
2. **Equivalence partitioning**: Test representative values from each valid range
3. **Error conditions**: Test invalid inputs and error paths

Shall I generate specific test cases based on the selected code?`;
    }

    // Default response
    return `I'm here to help with your ${session.language.toUpperCase()} code migration. You can:

- Select code and ask me to **explain** it
- Ask me to **migrate** code to a modern language
- Request **test cases** for business rules
- Ask questions about specific patterns or constructs

What would you like to do?`;
  }

  private async generateCompletions(
    session: CopilotSession,
    prefix: string,
    line: number
  ): Promise<InlineSuggestion[]> {
    const suggestions: InlineSuggestion[] = [];
    // Get context for potential future enhancements
    this.getLineContext(session.context.documentContent, line);

    // Context-aware completions
    if (prefix.toUpperCase().endsWith('COMPUTE')) {
      suggestions.push({
        id: `comp_${Date.now()}_1`,
        range: { startLine: line, startColumn: prefix.length + 1, endLine: line, endColumn: prefix.length + 1 },
        text: ' RESULT = INPUT-A + INPUT-B.',
        displayText: ' RESULT = ... (calculation template)',
        type: 'completion',
        confidence: 0.8,
        source: 'pattern',
      });
    }

    if (prefix.toUpperCase().endsWith('IF')) {
      suggestions.push({
        id: `comp_${Date.now()}_2`,
        range: { startLine: line, startColumn: prefix.length + 1, endLine: line, endColumn: prefix.length + 1 },
        text: ' CONDITION-VAR EQUAL TO EXPECTED-VALUE',
        displayText: ' ... EQUAL TO ... (condition template)',
        type: 'completion',
        confidence: 0.75,
        source: 'pattern',
      });
    }

    return suggestions;
  }

  private clearCacheForSession(sessionId: string): void {
    const keysToDelete: string[] = [];
    for (const key of this.suggestionCache.keys()) {
      if (key.startsWith(sessionId)) {
        keysToDelete.push(key);
      }
    }
    for (const key of keysToDelete) {
      this.suggestionCache.delete(key);
    }
  }
}
