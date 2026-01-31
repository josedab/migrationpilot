/**
 * @migrationpilot/ide-extension
 * 
 * IDE integration layer for VS Code and IntelliJ.
 * Provides code analysis, rule highlighting, and migration assistance.
 */

// ============================================================================
// Types
// ============================================================================

export interface IDECapabilities {
  supportsCodeLens: boolean;
  supportsDiagnostics: boolean;
  supportsHover: boolean;
  supportsCodeActions: boolean;
  supportsCommands: boolean;
  supportsTreeView: boolean;
}

export interface CodeAnalysisResult {
  uri: string;
  language: string;
  diagnostics: Diagnostic[];
  codeLenses: CodeLens[];
  highlights: Highlight[];
}

export interface Diagnostic {
  range: Range;
  message: string;
  severity: 'error' | 'warning' | 'info' | 'hint';
  code: string;
  source: string;
  relatedInformation?: DiagnosticRelatedInfo[];
}

export interface DiagnosticRelatedInfo {
  uri: string;
  range: Range;
  message: string;
}

export interface Range {
  startLine: number;
  startColumn: number;
  endLine: number;
  endColumn: number;
}

export interface CodeLens {
  range: Range;
  command: Command;
}

export interface Command {
  title: string;
  command: string;
  arguments?: unknown[];
}

export interface Highlight {
  range: Range;
  type: 'business-rule' | 'data-structure' | 'external-call' | 'complex-logic';
  description: string;
  confidence: number;
}

export interface HoverInfo {
  contents: string[];
  range: Range;
}

export interface CodeAction {
  title: string;
  kind: 'quickfix' | 'refactor' | 'extract' | 'migrate';
  diagnostics?: Diagnostic[];
  edit?: WorkspaceEdit;
  command?: Command;
}

export interface WorkspaceEdit {
  changes: Record<string, TextEdit[]>;
}

export interface TextEdit {
  range: Range;
  newText: string;
}

// ============================================================================
// IDE Provider Interface
// ============================================================================

export interface IDEProvider {
  capabilities: IDECapabilities;
  analyze(uri: string, content: string): Promise<CodeAnalysisResult>;
  getHover(uri: string, line: number, column: number): Promise<HoverInfo | null>;
  getCodeActions(uri: string, range: Range, diagnostics: Diagnostic[]): Promise<CodeAction[]>;
  executeCommand(command: string, args: unknown[]): Promise<unknown>;
}

// ============================================================================
// Base IDE Provider Implementation
// ============================================================================

export class MigrationPilotIDEProvider implements IDEProvider {
  capabilities: IDECapabilities = {
    supportsCodeLens: true,
    supportsDiagnostics: true,
    supportsHover: true,
    supportsCodeActions: true,
    supportsCommands: true,
    supportsTreeView: true,
  };

  private analysisCache: Map<string, CodeAnalysisResult> = new Map();

  /**
   * Analyze source code
   */
  async analyze(uri: string, content: string): Promise<CodeAnalysisResult> {
    const language = this.detectLanguage(uri);
    const diagnostics: Diagnostic[] = [];
    const codeLenses: CodeLens[] = [];
    const highlights: Highlight[] = [];

    // Analyze for business rules
    const ruleMatches = this.findBusinessRules(content, language);
    for (const match of ruleMatches) {
      highlights.push({
        range: match.range,
        type: 'business-rule',
        description: match.description,
        confidence: match.confidence,
      });

      codeLenses.push({
        range: match.range,
        command: {
          title: `📋 Rule: ${match.name} (${Math.round(match.confidence * 100)}% confidence)`,
          command: 'migrationpilot.viewRule',
          arguments: [uri, match.range],
        },
      });
    }

    // Find complex logic that needs attention
    const complexityIssues = this.findComplexLogic(content, language);
    for (const issue of complexityIssues) {
      diagnostics.push({
        range: issue.range,
        message: issue.message,
        severity: 'warning',
        code: 'MP001',
        source: 'MigrationPilot',
      });
    }

    // Find external calls
    const externalCalls = this.findExternalCalls(content, language);
    for (const call of externalCalls) {
      highlights.push({
        range: call.range,
        type: 'external-call',
        description: `External call to ${call.target}`,
        confidence: 1.0,
      });

      codeLenses.push({
        range: call.range,
        command: {
          title: `🔗 External: ${call.target}`,
          command: 'migrationpilot.viewExternalCall',
          arguments: [uri, call.target],
        },
      });
    }

    const result: CodeAnalysisResult = {
      uri,
      language,
      diagnostics,
      codeLenses,
      highlights,
    };

    this.analysisCache.set(uri, result);
    return result;
  }

  /**
   * Get hover information
   */
  async getHover(uri: string, line: number, column: number): Promise<HoverInfo | null> {
    const analysis = this.analysisCache.get(uri);
    if (!analysis) return null;

    // Find highlight at position
    for (const highlight of analysis.highlights) {
      if (this.isInRange(line, column, highlight.range)) {
        return {
          contents: [
            `**${highlight.type.toUpperCase()}**`,
            highlight.description,
            `Confidence: ${Math.round(highlight.confidence * 100)}%`,
            '',
            '*Click the CodeLens above for more details*',
          ],
          range: highlight.range,
        };
      }
    }

    return null;
  }

  /**
   * Get code actions
   */
  async getCodeActions(
    uri: string,
    range: Range,
    diagnostics: Diagnostic[]
  ): Promise<CodeAction[]> {
    const actions: CodeAction[] = [];

    // Add migration-related actions
    if (diagnostics.some(d => d.code === 'MP001')) {
      actions.push({
        title: '🔄 Extract as business rule',
        kind: 'extract',
        diagnostics: diagnostics.filter(d => d.code === 'MP001'),
        command: {
          title: 'Extract Business Rule',
          command: 'migrationpilot.extractRule',
          arguments: [uri, range],
        },
      });
    }

    actions.push({
      title: '📖 Generate documentation',
      kind: 'refactor',
      command: {
        title: 'Generate Documentation',
        command: 'migrationpilot.generateDocs',
        arguments: [uri, range],
      },
    });

    actions.push({
      title: '🧪 Generate tests',
      kind: 'refactor',
      command: {
        title: 'Generate Tests',
        command: 'migrationpilot.generateTests',
        arguments: [uri, range],
      },
    });

    return actions;
  }

  /**
   * Execute a command
   */
  async executeCommand(command: string, args: unknown[]): Promise<unknown> {
    switch (command) {
      case 'migrationpilot.viewRule':
        console.log('View rule:', args);
        return { success: true };

      case 'migrationpilot.viewExternalCall':
        console.log('View external call:', args);
        return { success: true };

      case 'migrationpilot.extractRule':
        console.log('Extract rule:', args);
        return { success: true, ruleId: `rule_${Date.now()}` };

      case 'migrationpilot.generateDocs':
        console.log('Generate docs:', args);
        return { success: true };

      case 'migrationpilot.generateTests':
        console.log('Generate tests:', args);
        return { success: true };

      default:
        throw new Error(`Unknown command: ${command}`);
    }
  }

  private detectLanguage(uri: string): string {
    const ext = uri.split('.').pop()?.toLowerCase();
    const languageMap: Record<string, string> = {
      'cbl': 'cobol',
      'cob': 'cobol',
      'cpy': 'cobol',
      'f': 'fortran',
      'f77': 'fortran',
      'f90': 'fortran',
      'for': 'fortran',
      'bas': 'vb6',
      'frm': 'vb6',
      'cls': 'vb6',
      'java': 'java',
      'pli': 'pli',
      'pl1': 'pli',
      'rpg': 'rpg',
      'rpgle': 'rpg',
    };
    return languageMap[ext || ''] || 'unknown';
  }

  private findBusinessRules(content: string, _language: string): Array<{
    range: Range;
    name: string;
    description: string;
    confidence: number;
  }> {
    const rules: Array<{ range: Range; name: string; description: string; confidence: number }> = [];
    const lines = content.split('\n');

    // Simple pattern matching for demo - real impl would use parsers
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (!line) continue;

      // Look for calculation patterns
      if (/COMPUTE|CALCULATE|=.*\+.*-|\*.*\//.test(line)) {
        rules.push({
          range: { startLine: i + 1, startColumn: 1, endLine: i + 1, endColumn: line.length },
          name: `Calculation at line ${i + 1}`,
          description: 'Potential business calculation',
          confidence: 0.75,
        });
      }

      // Look for validation patterns
      if (/IF.*<|>|=|NOT|INVALID|VALID|CHECK|VERIFY/.test(line.toUpperCase())) {
        rules.push({
          range: { startLine: i + 1, startColumn: 1, endLine: i + 1, endColumn: line.length },
          name: `Validation at line ${i + 1}`,
          description: 'Potential validation rule',
          confidence: 0.7,
        });
      }
    }

    return rules;
  }

  private findComplexLogic(content: string, _language: string): Array<{
    range: Range;
    message: string;
  }> {
    const issues: Array<{ range: Range; message: string }> = [];
    const lines = content.split('\n');

    // Look for nested conditions
    let nestingLevel = 0;
    for (let i = 0; i < lines.length; i++) {
      const lineContent = lines[i];
      if (!lineContent) continue;
      
      const line = lineContent.toUpperCase();

      if (/^\s*(IF|EVALUATE|SELECT|DECIDE)/.test(line)) {
        nestingLevel++;
        if (nestingLevel > 3) {
          issues.push({
            range: { startLine: i + 1, startColumn: 1, endLine: i + 1, endColumn: lineContent.length },
            message: `High nesting level (${nestingLevel}). Consider extracting to separate procedure.`,
          });
        }
      }

      if (/^\s*(END-IF|END-EVALUATE|END-SELECT|END-DECIDE)/.test(line)) {
        nestingLevel = Math.max(0, nestingLevel - 1);
      }
    }

    return issues;
  }

  private findExternalCalls(content: string, _language: string): Array<{
    range: Range;
    target: string;
  }> {
    const calls: Array<{ range: Range; target: string }> = [];
    const lines = content.split('\n');

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (!line) continue;

      // COBOL CALL statement
      const callMatch = line.match(/CALL\s+['"]?(\w+)['"]?/i);
      if (callMatch && callMatch[1]) {
        calls.push({
          range: { startLine: i + 1, startColumn: 1, endLine: i + 1, endColumn: line.length },
          target: callMatch[1],
        });
      }

      // CICS commands
      const cicsMatch = line.match(/EXEC\s+CICS\s+(\w+)/i);
      if (cicsMatch && cicsMatch[1]) {
        calls.push({
          range: { startLine: i + 1, startColumn: 1, endLine: i + 1, endColumn: line.length },
          target: `CICS:${cicsMatch[1]}`,
        });
      }
    }

    return calls;
  }

  private isInRange(line: number, column: number, range: Range): boolean {
    if (line < range.startLine || line > range.endLine) return false;
    if (line === range.startLine && column < range.startColumn) return false;
    if (line === range.endLine && column > range.endColumn) return false;
    return true;
  }
}

// ============================================================================
// VS Code Extension Types
// ============================================================================

export interface VSCodeExtensionConfig {
  enableCodeLens: boolean;
  enableDiagnostics: boolean;
  enableHover: boolean;
  autoAnalyze: boolean;
  highlightBusinessRules: boolean;
  highlightExternalCalls: boolean;
}

export const DEFAULT_VSCODE_CONFIG: VSCodeExtensionConfig = {
  enableCodeLens: true,
  enableDiagnostics: true,
  enableHover: true,
  autoAnalyze: true,
  highlightBusinessRules: true,
  highlightExternalCalls: true,
};

// ============================================================================
// Copilot Features
// ============================================================================

export * from './copilot/index.js';
