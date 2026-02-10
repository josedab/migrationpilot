/**
 * MigrationPilot VS Code Extension
 * AI-powered legacy code migration assistant
 */

import * as vscode from 'vscode';
import { MigrationPilotApiClient } from './api-client';
import { RulesProvider } from './providers/rules-provider';
import { AnalysisProvider } from './providers/analysis-provider';
import type { ExtensionConfig, BusinessRuleInfo } from './types';

let apiClient: MigrationPilotApiClient;
let rulesProvider: RulesProvider;
let analysisProvider: AnalysisProvider;
let decorationType: vscode.TextEditorDecorationType;
let statusBarItem: vscode.StatusBarItem;

/**
 * Extension activation
 */
export function activate(context: vscode.ExtensionContext): void {
  console.log('MigrationPilot extension is now active');

  // Initialize configuration
  const config = getConfig();

  // Initialize API client
  apiClient = new MigrationPilotApiClient(config.apiUrl);

  // Initialize providers
  rulesProvider = new RulesProvider();
  analysisProvider = new AnalysisProvider();

  // Register tree views
  vscode.window.registerTreeDataProvider('migrationpilot.rules', rulesProvider);
  vscode.window.registerTreeDataProvider('migrationpilot.analysis', analysisProvider);

  // Create decoration type for inline hints
  decorationType = vscode.window.createTextEditorDecorationType({
    after: {
      margin: '0 0 0 1em',
      color: new vscode.ThemeColor('editorCodeLens.foreground'),
    },
  });

  // Create status bar item
  statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
  statusBarItem.text = '$(beaker) MigrationPilot';
  statusBarItem.tooltip = 'MigrationPilot: Click to analyze file';
  statusBarItem.command = 'migrationpilot.analyzeFile';
  statusBarItem.show();
  context.subscriptions.push(statusBarItem);

  // Register commands
  context.subscriptions.push(
    vscode.commands.registerCommand('migrationpilot.analyzeFile', analyzeCurrentFile),
    vscode.commands.registerCommand('migrationpilot.extractRules', extractBusinessRules),
    vscode.commands.registerCommand('migrationpilot.suggestMigration', suggestMigration),
    vscode.commands.registerCommand('migrationpilot.explainCode', explainSelectedCode),
    vscode.commands.registerCommand('migrationpilot.generateTests', generateTests),
    vscode.commands.registerCommand('migrationpilot.showKnowledgeGraph', showKnowledgeGraph),
    vscode.commands.registerCommand('migrationpilot.askQuestion', askQuestion),
    vscode.commands.registerCommand('migrationpilot.goToLine', goToLine)
  );

  // Watch for configuration changes
  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration(e => {
      if (e.affectsConfiguration('migrationpilot')) {
        const newConfig = getConfig();
        apiClient.setBaseUrl(newConfig.apiUrl);
      }
    })
  );

  // Auto-analyze on file open (if enabled)
  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(editor => {
      if (editor && config.autoAnalyze) {
        const language = editor.document.languageId;
        if (isLegacyLanguage(language)) {
          analyzeCurrentFile();
        }
      }
    })
  );

  // Initial analysis if a legacy file is open
  const activeEditor = vscode.window.activeTextEditor;
  if (activeEditor && config.autoAnalyze && isLegacyLanguage(activeEditor.document.languageId)) {
    analyzeCurrentFile();
  }
}

/**
 * Extension deactivation
 */
export function deactivate(): void {
  console.log('MigrationPilot extension is now deactivated');
}

// ============================================================================
// COMMANDS
// ============================================================================

/**
 * Analyze the current file
 */
async function analyzeCurrentFile(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('No file is currently open');
    return;
  }

  const document = editor.document;
  const filePath = document.uri.fsPath;
  const language = document.languageId;
  const content = document.getText();

  statusBarItem.text = '$(sync~spin) Analyzing...';

  try {
    const analysis = await apiClient.analyzeFile(content, language, filePath);

    // Update providers
    analysisProvider.updateAnalysis(analysis);
    rulesProvider.updateRules(filePath, analysis.businessRules);

    // Show inline hints for business rules
    const config = getConfig();
    if (config.showInlineHints) {
      showBusinessRuleHints(editor, analysis.businessRules);
    }

    statusBarItem.text = `$(check) ${analysis.businessRules.length} rules found`;

    vscode.window.showInformationMessage(
      `Analysis complete: ${analysis.procedures.length} procedures, ${analysis.businessRules.length} business rules`
    );
  } catch (error) {
    statusBarItem.text = '$(error) Analysis failed';
    vscode.window.showErrorMessage(
      `Failed to analyze file: ${error instanceof Error ? error.message : 'Unknown error'}`
    );
  }
}

/**
 * Extract business rules from current file
 */
async function extractBusinessRules(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('No file is currently open');
    return;
  }

  const content = editor.document.getText();
  const language = editor.document.languageId;

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Extracting business rules...',
      cancellable: false,
    },
    async () => {
      try {
        const rules = await apiClient.extractRules(content, language);

        if (rules.length === 0) {
          vscode.window.showInformationMessage('No business rules found in this file');
          return;
        }

        rulesProvider.updateRules(editor.document.uri.fsPath, rules);

        // Show rules in output panel
        const outputChannel = vscode.window.createOutputChannel('MigrationPilot Rules');
        outputChannel.clear();
        outputChannel.appendLine('Extracted Business Rules\n');

        for (const rule of rules) {
          outputChannel.appendLine(`📋 ${rule.name}`);
          outputChannel.appendLine(`   Category: ${rule.category}`);
          outputChannel.appendLine(`   Description: ${rule.description}`);
          outputChannel.appendLine(`   Confidence: ${Math.round(rule.confidence * 100)}%`);
          outputChannel.appendLine(`   Lines: ${rule.startLine}-${rule.endLine}`);
          if (rule.formula) {
            outputChannel.appendLine(`   Formula: ${rule.formula}`);
          }
          outputChannel.appendLine('');
        }

        outputChannel.show();
        vscode.window.showInformationMessage(`Extracted ${rules.length} business rules`);
      } catch (error) {
        vscode.window.showErrorMessage(
          `Failed to extract rules: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    }
  );
}

/**
 * Suggest migration for selected code
 */
async function suggestMigration(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('No file is currently open');
    return;
  }

  const selection = editor.selection;
  const content = selection.isEmpty ? editor.document.getText() : editor.document.getText(selection);
  const sourceLanguage = editor.document.languageId;

  // Get target language from configuration
  const config = getConfig();
  const targetLanguage = config.targetLanguage;

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `Generating migration to ${targetLanguage}...`,
      cancellable: false,
    },
    async () => {
      try {
        const suggestions = await apiClient.suggestMigration(content, sourceLanguage, targetLanguage);

        if (suggestions.length === 0) {
          vscode.window.showInformationMessage('No migration suggestions available');
          return;
        }

        // Show suggestions in a new document
        const doc = await vscode.workspace.openTextDocument({
          language: targetLanguage,
          content: suggestions
            .map(s => `// ${s.title}\n// ${s.description}\n// Confidence: ${Math.round(s.confidence * 100)}%\n\n${s.targetCode || '// Migration code will be generated here'}`)
            .join('\n\n'),
        });

        await vscode.window.showTextDocument(doc, { preview: true });
        vscode.window.showInformationMessage(`Generated ${suggestions.length} migration suggestions`);
      } catch (error) {
        vscode.window.showErrorMessage(
          `Failed to generate migration: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    }
  );
}

/**
 * Explain selected code
 */
async function explainSelectedCode(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('No file is currently open');
    return;
  }

  const selection = editor.selection;
  if (selection.isEmpty) {
    vscode.window.showWarningMessage('Please select some code to explain');
    return;
  }

  const content = editor.document.getText(selection);
  const language = editor.document.languageId;

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Analyzing code...',
      cancellable: false,
    },
    async () => {
      try {
        const explanation = await apiClient.explainCode(content, language);

        // Show in webview panel
        const panel = vscode.window.createWebviewPanel(
          'migrationpilotExplanation',
          'Code Explanation',
          vscode.ViewColumn.Beside,
          { enableScripts: false }
        );

        panel.webview.html = generateExplanationHtml(explanation);
      } catch (error) {
        vscode.window.showErrorMessage(
          `Failed to explain code: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    }
  );
}

/**
 * Generate tests for business rules
 */
async function generateTests(): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showWarningMessage('No file is currently open');
    return;
  }

  const filePath = editor.document.uri.fsPath;
  const content = editor.document.getText();
  const language = editor.document.languageId;

  // First extract rules
  const rules = await apiClient.extractRules(content, language);

  if (rules.length === 0) {
    vscode.window.showWarningMessage('No business rules found to generate tests for');
    return;
  }

  const ruleIds = rules.map(r => r.id);

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Generating tests...',
      cancellable: false,
    },
    async () => {
      try {
        const result = await apiClient.generateTests(ruleIds);

        if (result.testCases.length === 0) {
          vscode.window.showWarningMessage('No tests were generated');
          return;
        }

        // Show generated tests
        for (const testCase of result.testCases) {
          const doc = await vscode.workspace.openTextDocument({
            language: 'typescript',
            content: testCase.code,
          });
          await vscode.window.showTextDocument(doc, { preview: true });
        }

        vscode.window.showInformationMessage(
          `Generated ${result.testCases.length} test files covering ${result.coverage.percentage}% of rules`
        );
      } catch (error) {
        vscode.window.showErrorMessage(
          `Failed to generate tests: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    }
  );
}

/**
 * Show knowledge graph visualization
 */
async function showKnowledgeGraph(): Promise<void> {
  const panel = vscode.window.createWebviewPanel(
    'migrationpilotGraph',
    'Knowledge Graph',
    vscode.ViewColumn.One,
    { enableScripts: true }
  );

  const config = getConfig();

  panel.webview.html = `
    <!DOCTYPE html>
    <html>
      <head>
        <title>Knowledge Graph</title>
        <style>
          body { margin: 0; padding: 20px; font-family: var(--vscode-font-family); }
          h1 { color: var(--vscode-foreground); }
          .message { color: var(--vscode-descriptionForeground); }
          iframe { width: 100%; height: 80vh; border: 1px solid var(--vscode-panel-border); }
        </style>
      </head>
      <body>
        <h1>Knowledge Graph</h1>
        <p class="message">The knowledge graph visualization is available at:</p>
        <p><a href="${config.apiUrl.replace('/api', '')}/visualization">Open in Browser</a></p>
        <iframe src="${config.apiUrl.replace('/api', '')}/visualization" />
      </body>
    </html>
  `;
}

/**
 * Ask a question about the code
 */
async function askQuestion(): Promise<void> {
  const question = await vscode.window.showInputBox({
    prompt: 'Ask a question about the code',
    placeHolder: 'e.g., Where is the interest calculation?',
  });

  if (!question) {
    return;
  }

  const editor = vscode.window.activeTextEditor;
  const context = editor?.selection.isEmpty ? undefined : editor?.document.getText(editor.selection);

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: 'Searching for answer...',
      cancellable: false,
    },
    async () => {
      try {
        const result = await apiClient.askQuestion(question, context);

        // Show answer in info message or output channel
        if (result.answer.length < 200) {
          vscode.window.showInformationMessage(result.answer);
        } else {
          const outputChannel = vscode.window.createOutputChannel('MigrationPilot Q&A');
          outputChannel.clear();
          outputChannel.appendLine(`Question: ${question}\n`);
          outputChannel.appendLine(`Answer:\n${result.answer}`);
          outputChannel.appendLine(`\nConfidence: ${Math.round(result.confidence * 100)}%`);

          if (result.sources.length > 0) {
            outputChannel.appendLine('\nSources:');
            for (const source of result.sources) {
              outputChannel.appendLine(`  - ${source.name} (${source.type})${source.file ? `: ${source.file}:${source.line}` : ''}`);
            }
          }

          outputChannel.show();
        }
      } catch (error) {
        vscode.window.showErrorMessage(
          `Failed to get answer: ${error instanceof Error ? error.message : 'Unknown error'}`
        );
      }
    }
  );
}

/**
 * Navigate to a specific line
 */
function goToLine(line: number): void {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;

  const position = new vscode.Position(line - 1, 0);
  const range = new vscode.Range(position, position);

  editor.selection = new vscode.Selection(position, position);
  editor.revealRange(range, vscode.TextEditorRevealType.InCenter);
}

// ============================================================================
// HELPERS
// ============================================================================

function getConfig(): ExtensionConfig {
  const config = vscode.workspace.getConfiguration('migrationpilot');
  return {
    apiUrl: config.get<string>('apiUrl') || 'http://localhost:4000/api',
    targetLanguage: config.get<string>('targetLanguage') || 'typescript',
    autoAnalyze: config.get<boolean>('autoAnalyze') ?? true,
    showInlineHints: config.get<boolean>('showInlineHints') ?? true,
    confidenceThreshold: config.get<number>('confidenceThreshold') || 0.7,
  };
}

function isLegacyLanguage(languageId: string): boolean {
  const legacyLanguages = ['cobol', 'fortran', 'vb', 'vba', 'java'];
  return legacyLanguages.includes(languageId.toLowerCase());
}

function showBusinessRuleHints(editor: vscode.TextEditor, rules: BusinessRuleInfo[]): void {
  const decorations: vscode.DecorationOptions[] = [];

  for (const rule of rules) {
    const line = rule.startLine - 1;
    if (line < 0 || line >= editor.document.lineCount) continue;

    const range = new vscode.Range(
      new vscode.Position(line, editor.document.lineAt(line).text.length),
      new vscode.Position(line, editor.document.lineAt(line).text.length)
    );

    decorations.push({
      range,
      renderOptions: {
        after: {
          contentText: ` 📋 ${rule.name} (${Math.round(rule.confidence * 100)}%)`,
          color: new vscode.ThemeColor('editorCodeLens.foreground'),
        },
      },
    });
  }

  editor.setDecorations(decorationType, decorations);
}

function generateExplanationHtml(explanation: { summary: string; detailedExplanation: string; businessPurpose?: string; complexity: string; relatedRules: string[] }): string {
  return `
    <!DOCTYPE html>
    <html>
      <head>
        <style>
          body {
            font-family: var(--vscode-font-family);
            padding: 20px;
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
          }
          h1 { color: var(--vscode-textLink-foreground); }
          h2 { color: var(--vscode-textLink-activeForeground); margin-top: 20px; }
          .section { margin: 15px 0; padding: 10px; background: var(--vscode-editor-inactiveSelectionBackground); border-radius: 4px; }
          .label { font-weight: bold; color: var(--vscode-textPreformat-foreground); }
          ul { padding-left: 20px; }
        </style>
      </head>
      <body>
        <h1>Code Explanation</h1>

        <div class="section">
          <h2>Summary</h2>
          <p>${explanation.summary}</p>
        </div>

        <div class="section">
          <h2>Detailed Explanation</h2>
          <p>${explanation.detailedExplanation}</p>
        </div>

        ${explanation.businessPurpose ? `
          <div class="section">
            <h2>Business Purpose</h2>
            <p>${explanation.businessPurpose}</p>
          </div>
        ` : ''}

        <div class="section">
          <span class="label">Complexity:</span> ${explanation.complexity}
        </div>

        ${explanation.relatedRules.length > 0 ? `
          <div class="section">
            <h2>Related Business Rules</h2>
            <ul>
              ${explanation.relatedRules.map(r => `<li>${r}</li>`).join('')}
            </ul>
          </div>
        ` : ''}
      </body>
    </html>
  `;
}
