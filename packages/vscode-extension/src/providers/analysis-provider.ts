/**
 * Analysis Tree View Provider
 * Displays file analysis results in the sidebar
 */

import * as vscode from 'vscode';
import type { AnalysisResult, ProcedureInfo, DataStructureInfo, DependencyInfo } from '../types';

export class AnalysisProvider implements vscode.TreeDataProvider<AnalysisItem> {
  private _onDidChangeTreeData: vscode.EventEmitter<AnalysisItem | undefined | void> = new vscode.EventEmitter<AnalysisItem | undefined | void>();
  readonly onDidChangeTreeData: vscode.Event<AnalysisItem | undefined | void> = this._onDidChangeTreeData.event;

  private analysis: AnalysisResult | null = null;

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  updateAnalysis(analysis: AnalysisResult): void {
    this.analysis = analysis;
    this.refresh();
  }

  clearAnalysis(): void {
    this.analysis = null;
    this.refresh();
  }

  getTreeItem(element: AnalysisItem): vscode.TreeItem {
    return element;
  }

  getChildren(element?: AnalysisItem): Thenable<AnalysisItem[]> {
    if (!element) {
      // Root level
      if (!this.analysis) {
        return Promise.resolve([
          new AnalysisItem('No file analyzed', vscode.TreeItemCollapsibleState.None, 'empty'),
        ]);
      }

      const items: AnalysisItem[] = [];

      // File info
      items.push(new AnalysisItem(
        `📄 ${this.getFileName(this.analysis.filePath)}`,
        vscode.TreeItemCollapsibleState.None,
        'file'
      ));

      // Language
      items.push(new AnalysisItem(
        `🌐 Language: ${this.analysis.language}`,
        vscode.TreeItemCollapsibleState.None,
        'language'
      ));

      // Complexity
      items.push(new AnalysisItem(
        `📊 Complexity: ${this.formatComplexity(this.analysis.complexity)}`,
        vscode.TreeItemCollapsibleState.None,
        'complexity'
      ));

      // Procedures
      if (this.analysis.procedures.length > 0) {
        items.push(new AnalysisItem(
          `🔧 Procedures (${this.analysis.procedures.length})`,
          vscode.TreeItemCollapsibleState.Collapsed,
          'procedures',
          { procedures: this.analysis.procedures }
        ));
      }

      // Data Structures
      if (this.analysis.dataStructures.length > 0) {
        items.push(new AnalysisItem(
          `📦 Data Structures (${this.analysis.dataStructures.length})`,
          vscode.TreeItemCollapsibleState.Collapsed,
          'dataStructures',
          { structures: this.analysis.dataStructures }
        ));
      }

      // Dependencies
      if (this.analysis.dependencies.length > 0) {
        items.push(new AnalysisItem(
          `🔗 Dependencies (${this.analysis.dependencies.length})`,
          vscode.TreeItemCollapsibleState.Collapsed,
          'dependencies',
          { dependencies: this.analysis.dependencies }
        ));
      }

      // Suggestions
      if (this.analysis.suggestions.length > 0) {
        items.push(new AnalysisItem(
          `💡 Suggestions (${this.analysis.suggestions.length})`,
          vscode.TreeItemCollapsibleState.Collapsed,
          'suggestions',
          { suggestions: this.analysis.suggestions }
        ));
      }

      return Promise.resolve(items);
    }

    // Children
    switch (element.contextValue) {
      case 'procedures':
        return Promise.resolve(
          (element.data?.procedures as ProcedureInfo[] || []).map(p =>
            new AnalysisItem(
              `${this.getProcedureIcon(p.type)} ${p.name}`,
              vscode.TreeItemCollapsibleState.None,
              'procedure',
              { procedure: p }
            )
          )
        );

      case 'dataStructures':
        return Promise.resolve(
          (element.data?.structures as DataStructureInfo[] || []).map(ds =>
            new AnalysisItem(
              `📋 ${ds.name} (${ds.fields.length} fields)`,
              vscode.TreeItemCollapsibleState.None,
              'dataStructure',
              { structure: ds }
            )
          )
        );

      case 'dependencies':
        return Promise.resolve(
          (element.data?.dependencies as DependencyInfo[] || []).map(d =>
            new AnalysisItem(
              `${this.getDependencyIcon(d.type)} ${d.name}`,
              vscode.TreeItemCollapsibleState.None,
              'dependency',
              { dependency: d }
            )
          )
        );

      case 'suggestions':
        return Promise.resolve(
          (element.data?.suggestions as Array<{ title: string; priority: string }> || []).map(s =>
            new AnalysisItem(
              `${this.getPriorityIcon(s.priority)} ${s.title}`,
              vscode.TreeItemCollapsibleState.None,
              'suggestion',
              { suggestion: s }
            )
          )
        );

      default:
        return Promise.resolve([]);
    }
  }

  private getFileName(path: string): string {
    return path.split(/[/\\]/).pop() || path;
  }

  private formatComplexity(complexity: number): string {
    if (complexity < 5) return `Low (${complexity.toFixed(1)})`;
    if (complexity < 10) return `Moderate (${complexity.toFixed(1)})`;
    if (complexity < 20) return `High (${complexity.toFixed(1)})`;
    return `Very High (${complexity.toFixed(1)})`;
  }

  private getProcedureIcon(type: string): string {
    const icons: Record<string, string> = {
      program: '🏠',
      section: '📁',
      paragraph: '📄',
      function: '⚡',
      subroutine: '🔁',
      method: '🔹',
    };
    return icons[type] || '🔧';
  }

  private getDependencyIcon(type: string): string {
    const icons: Record<string, string> = {
      internal: '📦',
      external: '🌐',
      database: '🗄️',
      file: '📁',
    };
    return icons[type] || '🔗';
  }

  private getPriorityIcon(priority: string): string {
    const icons: Record<string, string> = {
      high: '🔴',
      medium: '🟡',
      low: '🟢',
    };
    return icons[priority] || '⚪';
  }
}

export class AnalysisItem extends vscode.TreeItem {
  constructor(
    public readonly label: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState,
    public readonly contextValue: string,
    public readonly data?: Record<string, unknown>
  ) {
    super(label, collapsibleState);
    this.tooltip = label;

    // Add commands for clickable items
    if (contextValue === 'procedure' && data?.procedure) {
      const proc = data.procedure as ProcedureInfo;
      this.command = {
        command: 'migrationpilot.goToLine',
        title: 'Go to Procedure',
        arguments: [proc.startLine],
      };
      this.tooltip = `${proc.name}\nType: ${proc.type}\nComplexity: ${proc.complexity}\nLines: ${proc.startLine}-${proc.endLine}`;
    }

    if (contextValue === 'dataStructure' && data?.structure) {
      const ds = data.structure as DataStructureInfo;
      this.command = {
        command: 'migrationpilot.goToLine',
        title: 'Go to Data Structure',
        arguments: [ds.startLine],
      };
    }
  }
}
