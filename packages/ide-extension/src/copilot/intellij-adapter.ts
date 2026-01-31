/**
 * IntelliJ Plugin Compatibility Layer
 * 
 * Provides adapters and interfaces for IntelliJ IDEA plugin development.
 * Enables MigrationPilot to work in both VS Code and IntelliJ ecosystems.
 */

// ============================================================================
// IntelliJ Platform Types (compatibility layer)
// ============================================================================

export interface IntelliJPosition {
  line: number;
  column: number;
  offset: number;
}

export interface IntelliJRange {
  startOffset: number;
  endOffset: number;
  startPosition: IntelliJPosition;
  endPosition: IntelliJPosition;
}

export interface IntelliJDocument {
  getText(): string;
  getTextRange(range: IntelliJRange): string;
  getLineCount(): number;
  getLineStartOffset(line: number): number;
  getLineEndOffset(line: number): number;
}

export interface IntelliJEditor {
  getDocument(): IntelliJDocument;
  getCaretModel(): IntelliJCaretModel;
  getSelectionModel(): IntelliJSelectionModel;
  getProject(): IntelliJProject;
  getVirtualFile(): IntelliJVirtualFile;
}

export interface IntelliJCaretModel {
  getOffset(): number;
  getLogicalPosition(): IntelliJPosition;
  moveToOffset(offset: number): void;
}

export interface IntelliJSelectionModel {
  getSelectedText(): string | null;
  getSelectionStart(): number;
  getSelectionEnd(): number;
  hasSelection(): boolean;
}

export interface IntelliJProject {
  getName(): string;
  getBasePath(): string | null;
  isOpen(): boolean;
}

export interface IntelliJVirtualFile {
  getName(): string;
  getPath(): string;
  getExtension(): string | null;
  isDirectory(): boolean;
}

export interface IntelliJAnAction {
  actionPerformed(event: IntelliJAnActionEvent): void;
  update?(event: IntelliJAnActionEvent): void;
}

export interface IntelliJAnActionEvent {
  getProject(): IntelliJProject | null;
  getEditor(): IntelliJEditor | null;
  getData<T>(dataKey: string): T | null;
}

// ============================================================================
// IntelliJ Inspection Types
// ============================================================================

export interface IntelliJProblemDescriptor {
  getPsiElement(): unknown;
  getDescriptionTemplate(): string;
  getHighlightType(): IntelliJHighlightType;
  getFixes(): IntelliJQuickFix[];
}

export type IntelliJHighlightType = 
  | 'ERROR'
  | 'WARNING'
  | 'WEAK_WARNING'
  | 'INFORMATION'
  | 'GENERIC_SERVER_ERROR_OR_WARNING';

export interface IntelliJQuickFix {
  getName(): string;
  getFamilyName(): string;
  applyFix(project: IntelliJProject, descriptor: IntelliJProblemDescriptor): void;
}

// ============================================================================
// VS Code to IntelliJ Adapter
// ============================================================================

export interface VSCodeRange {
  startLine: number;
  startColumn: number;
  endLine: number;
  endColumn: number;
}

export interface VSCodeDiagnostic {
  range: VSCodeRange;
  message: string;
  severity: 'error' | 'warning' | 'info' | 'hint';
  source?: string;
  code?: string | number;
}

export class IntelliJAdapter {
  /**
   * Convert VS Code range to IntelliJ range
   */
  static toIntelliJRange(
    vsRange: VSCodeRange,
    document: IntelliJDocument
  ): IntelliJRange {
    const startOffset = document.getLineStartOffset(vsRange.startLine - 1) + vsRange.startColumn - 1;
    const endOffset = document.getLineStartOffset(vsRange.endLine - 1) + vsRange.endColumn - 1;

    return {
      startOffset,
      endOffset,
      startPosition: {
        line: vsRange.startLine - 1,
        column: vsRange.startColumn - 1,
        offset: startOffset,
      },
      endPosition: {
        line: vsRange.endLine - 1,
        column: vsRange.endColumn - 1,
        offset: endOffset,
      },
    };
  }

  /**
   * Convert IntelliJ range to VS Code range
   */
  static toVSCodeRange(ijRange: IntelliJRange): VSCodeRange {
    return {
      startLine: ijRange.startPosition.line + 1,
      startColumn: ijRange.startPosition.column + 1,
      endLine: ijRange.endPosition.line + 1,
      endColumn: ijRange.endPosition.column + 1,
    };
  }

  /**
   * Convert VS Code diagnostic to IntelliJ problem descriptor
   */
  static toProblemDescriptor(
    diagnostic: VSCodeDiagnostic,
    element: unknown,
    fixes: IntelliJQuickFix[] = []
  ): IntelliJProblemDescriptor {
    const highlightType = this.mapSeverityToHighlightType(diagnostic.severity);

    return {
      getPsiElement: () => element,
      getDescriptionTemplate: () => diagnostic.message,
      getHighlightType: () => highlightType,
      getFixes: () => fixes,
    };
  }

  /**
   * Map VS Code severity to IntelliJ highlight type
   */
  static mapSeverityToHighlightType(
    severity: VSCodeDiagnostic['severity']
  ): IntelliJHighlightType {
    const mapping: Record<VSCodeDiagnostic['severity'], IntelliJHighlightType> = {
      error: 'ERROR',
      warning: 'WARNING',
      info: 'WEAK_WARNING',
      hint: 'INFORMATION',
    };
    return mapping[severity];
  }

  /**
   * Map IntelliJ highlight type to VS Code severity
   */
  static mapHighlightTypeToSeverity(
    highlightType: IntelliJHighlightType
  ): VSCodeDiagnostic['severity'] {
    const mapping: Record<IntelliJHighlightType, VSCodeDiagnostic['severity']> = {
      ERROR: 'error',
      WARNING: 'warning',
      WEAK_WARNING: 'info',
      INFORMATION: 'hint',
      GENERIC_SERVER_ERROR_OR_WARNING: 'warning',
    };
    return mapping[highlightType];
  }
}

// ============================================================================
// IntelliJ Service Interface
// ============================================================================

export interface IntelliJMigrationService {
  /**
   * Analyze a file for migration patterns
   */
  analyzeFile(virtualFile: IntelliJVirtualFile): Promise<MigrationAnalysisResult>;

  /**
   * Get quick fixes for a specific problem
   */
  getQuickFixes(
    project: IntelliJProject,
    file: IntelliJVirtualFile,
    range: IntelliJRange
  ): IntelliJQuickFix[];

  /**
   * Execute a migration action
   */
  executeMigration(
    project: IntelliJProject,
    file: IntelliJVirtualFile,
    migrationId: string
  ): Promise<MigrationResult>;

  /**
   * Get inline hints for the editor
   */
  getInlineHints(
    editor: IntelliJEditor,
    visibleRange: IntelliJRange
  ): InlineHint[];
}

export interface MigrationAnalysisResult {
  fileUri: string;
  language: string;
  problems: IntelliJProblemDescriptor[];
  suggestions: MigrationSuggestion[];
  metrics: {
    linesAnalyzed: number;
    rulesDetected: number;
    complexityScore: number;
  };
}

export interface MigrationSuggestion {
  id: string;
  title: string;
  description: string;
  range: IntelliJRange;
  priority: 'high' | 'medium' | 'low';
  category: string;
}

export interface MigrationResult {
  success: boolean;
  changedFiles: string[];
  errors?: string[];
  warnings?: string[];
}

export interface InlineHint {
  offset: number;
  text: string;
  tooltip?: string;
  icon?: string;
}

// ============================================================================
// IntelliJ Action Implementations
// ============================================================================

export abstract class BaseMigrationAction implements IntelliJAnAction {
  abstract actionPerformed(event: IntelliJAnActionEvent): void;

  update(_event: IntelliJAnActionEvent): void {
    // Default: action is enabled if editor and project are available
    // Subclasses can override for more specific conditions
  }

  protected getSelectedTextOrLine(editor: IntelliJEditor): string {
    const selection = editor.getSelectionModel();
    if (selection.hasSelection()) {
      return selection.getSelectedText() || '';
    }
    
    const document = editor.getDocument();
    const position = editor.getCaretModel().getLogicalPosition();
    const startOffset = document.getLineStartOffset(position.line);
    const endOffset = document.getLineEndOffset(position.line);
    
    return document.getText().substring(startOffset, endOffset);
  }

  protected getDocumentLanguage(file: IntelliJVirtualFile): string {
    const ext = file.getExtension()?.toLowerCase();
    const languageMap: Record<string, string> = {
      cob: 'cobol',
      cbl: 'cobol',
      pli: 'pli',
      asm: 'assembler',
      jcl: 'jcl',
      rpg: 'rpg',
      java: 'java',
      py: 'python',
      ts: 'typescript',
      js: 'javascript',
    };
    return languageMap[ext || ''] || 'unknown';
  }
}

export class ExplainCodeAction extends BaseMigrationAction {
  actionPerformed(event: IntelliJAnActionEvent): void {
    const editor = event.getEditor();
    if (!editor) return;

    const text = this.getSelectedTextOrLine(editor);
    const language = this.getDocumentLanguage(editor.getVirtualFile());

    // Trigger explanation flow
    console.log(`Explaining ${language} code: ${text.substring(0, 100)}...`);
  }
}

export class MigrateCodeAction extends BaseMigrationAction {
  actionPerformed(event: IntelliJAnActionEvent): void {
    const editor = event.getEditor();
    if (!editor) return;

    const selection = editor.getSelectionModel();
    if (!selection.hasSelection()) {
      console.log('Please select code to migrate');
      return;
    }

    const text = selection.getSelectedText() || '';
    const language = this.getDocumentLanguage(editor.getVirtualFile());

    // Trigger migration flow
    console.log(`Migrating ${language} code: ${text.substring(0, 100)}...`);
  }
}

export class ExtractRuleAction extends BaseMigrationAction {
  actionPerformed(event: IntelliJAnActionEvent): void {
    const editor = event.getEditor();
    if (!editor) return;

    const selection = editor.getSelectionModel();
    if (!selection.hasSelection()) {
      console.log('Please select code containing a business rule');
      return;
    }

    const text = selection.getSelectedText() || '';
    
    // Trigger rule extraction flow
    console.log(`Extracting rule from: ${text.substring(0, 100)}...`);
  }
}

// ============================================================================
// IntelliJ Plugin Entry Point Interface
// ============================================================================

export interface IntelliJPluginConfig {
  serviceEndpoint?: string;
  enableTelemetry?: boolean;
  cacheTimeout?: number;
  maxConcurrentAnalysis?: number;
}

export abstract class MigrationPilotIntelliJPlugin {
  protected config: IntelliJPluginConfig;
  protected migrationService: IntelliJMigrationService | null = null;

  constructor(config: IntelliJPluginConfig = {}) {
    this.config = {
      enableTelemetry: false,
      cacheTimeout: 300000, // 5 minutes
      maxConcurrentAnalysis: 3,
      ...config,
    };
  }

  abstract initialize(): Promise<void>;
  abstract dispose(): void;

  /**
   * Get the migration service instance
   */
  getMigrationService(): IntelliJMigrationService | null {
    return this.migrationService;
  }

  /**
   * Register custom actions
   */
  abstract registerActions(): void;

  /**
   * Register inspections
   */
  abstract registerInspections(): void;

  /**
   * Register tool windows
   */
  abstract registerToolWindows(): void;
}

// ============================================================================
// Utility Functions
// ============================================================================

export function createQuickFix(
  name: string,
  familyName: string,
  applyFn: (project: IntelliJProject, descriptor: IntelliJProblemDescriptor) => void
): IntelliJQuickFix {
  return {
    getName: () => name,
    getFamilyName: () => familyName,
    applyFix: applyFn,
  };
}

export function createMockDocument(content: string): IntelliJDocument {
  const lines = content.split('\n');
  const lineOffsets: number[] = [0];
  
  for (let i = 0; i < lines.length - 1; i++) {
    lineOffsets.push(lineOffsets[i]! + lines[i]!.length + 1);
  }

  return {
    getText: () => content,
    getTextRange: (range: IntelliJRange) => content.substring(range.startOffset, range.endOffset),
    getLineCount: () => lines.length,
    getLineStartOffset: (line: number) => lineOffsets[line] ?? 0,
    getLineEndOffset: (line: number) => (lineOffsets[line] ?? 0) + (lines[line]?.length ?? 0),
  };
}
