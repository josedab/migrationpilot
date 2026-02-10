/**
 * Business Rules Tree View Provider
 * Displays extracted business rules in the sidebar
 */

import * as vscode from 'vscode';
import type { BusinessRuleInfo } from '../types';

export class RulesProvider implements vscode.TreeDataProvider<RuleItem> {
  private _onDidChangeTreeData: vscode.EventEmitter<RuleItem | undefined | void> = new vscode.EventEmitter<RuleItem | undefined | void>();
  readonly onDidChangeTreeData: vscode.Event<RuleItem | undefined | void> = this._onDidChangeTreeData.event;

  private rules: Map<string, BusinessRuleInfo[]> = new Map();
  private currentFile: string = '';

  refresh(): void {
    this._onDidChangeTreeData.fire();
  }

  updateRules(filePath: string, rules: BusinessRuleInfo[]): void {
    this.rules.set(filePath, rules);
    this.currentFile = filePath;
    this.refresh();
  }

  clearRules(): void {
    this.rules.clear();
    this.currentFile = '';
    this.refresh();
  }

  getTreeItem(element: RuleItem): vscode.TreeItem {
    return element;
  }

  getChildren(element?: RuleItem): Thenable<RuleItem[]> {
    if (!element) {
      // Root level - show files with rules
      if (this.currentFile && this.rules.has(this.currentFile)) {
        const fileRules = this.rules.get(this.currentFile)!;

        // Group by category
        const categories = new Map<string, BusinessRuleInfo[]>();
        for (const rule of fileRules) {
          const cat = rule.category || 'other';
          if (!categories.has(cat)) {
            categories.set(cat, []);
          }
          categories.get(cat)!.push(rule);
        }

        return Promise.resolve(
          Array.from(categories.entries()).map(([category, rules]) =>
            new RuleItem(
              this.formatCategory(category),
              vscode.TreeItemCollapsibleState.Expanded,
              'category',
              { category, rules }
            )
          )
        );
      }
      return Promise.resolve([]);
    }

    // Children of category - show rules
    if (element.contextValue === 'category' && element.data?.rules) {
      const rules = element.data.rules as BusinessRuleInfo[];
      return Promise.resolve(
        rules.map((rule: BusinessRuleInfo) =>
          new RuleItem(
            rule.name,
            vscode.TreeItemCollapsibleState.Collapsed,
            'rule',
            { rule }
          )
        )
      );
    }

    // Children of rule - show details
    if (element.contextValue === 'rule' && element.data?.rule) {
      const rule = element.data.rule as BusinessRuleInfo;
      const items: RuleItem[] = [];

      // Description
      if (rule.description) {
        items.push(new RuleItem(
          `📝 ${rule.description}`,
          vscode.TreeItemCollapsibleState.None,
          'description'
        ));
      }

      // Confidence
      items.push(new RuleItem(
        `🎯 Confidence: ${Math.round(rule.confidence * 100)}%`,
        vscode.TreeItemCollapsibleState.None,
        'confidence'
      ));

      // Location
      items.push(new RuleItem(
        `📍 Lines ${rule.startLine}-${rule.endLine}`,
        vscode.TreeItemCollapsibleState.None,
        'location',
        { startLine: rule.startLine, endLine: rule.endLine }
      ));

      // Inputs
      if (rule.inputs.length > 0) {
        items.push(new RuleItem(
          `📥 Inputs: ${rule.inputs.join(', ')}`,
          vscode.TreeItemCollapsibleState.None,
          'inputs'
        ));
      }

      // Outputs
      if (rule.outputs.length > 0) {
        items.push(new RuleItem(
          `📤 Outputs: ${rule.outputs.join(', ')}`,
          vscode.TreeItemCollapsibleState.None,
          'outputs'
        ));
      }

      // Formula
      if (rule.formula) {
        items.push(new RuleItem(
          `🔢 Formula: ${rule.formula}`,
          vscode.TreeItemCollapsibleState.None,
          'formula'
        ));
      }

      return Promise.resolve(items);
    }

    return Promise.resolve([]);
  }

  private formatCategory(category: string): string {
    const icons: Record<string, string> = {
      calculation: '🔢',
      validation: '✅',
      decision: '🔀',
      transformation: '🔄',
      other: '📋',
    };
    const icon = icons[category] || '📋';
    return `${icon} ${category.charAt(0).toUpperCase() + category.slice(1)}`;
  }
}

export class RuleItem extends vscode.TreeItem {
  constructor(
    public readonly label: string,
    public readonly collapsibleState: vscode.TreeItemCollapsibleState,
    public readonly contextValue: string,
    public readonly data?: Record<string, unknown>
  ) {
    super(label, collapsibleState);
    this.tooltip = label;

    // Set icons based on context
    switch (contextValue) {
      case 'category':
        this.iconPath = new vscode.ThemeIcon('folder');
        break;
      case 'rule':
        this.iconPath = new vscode.ThemeIcon('symbol-method');
        break;
      case 'location':
        this.command = {
          command: 'migrationpilot.goToLine',
          title: 'Go to Line',
          arguments: [data?.startLine],
        };
        break;
    }
  }
}
