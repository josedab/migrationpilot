/**
 * Documentation Export Service
 * 
 * Export business rules and migration documentation to various formats
 */

import type { BusinessRule, Project } from '@migrationpilot/core';

export type ExportFormat = 'json' | 'markdown' | 'html' | 'pdf' | 'word' | 'confluence';

export interface ExportOptions {
  format: ExportFormat;
  includeSourceCode?: boolean;
  includeGeneratedCode?: boolean;
  includeTestResults?: boolean;
  includeComments?: boolean;
  confluenceConfig?: {
    baseUrl: string;
    spaceKey: string;
    parentPageId?: string;
    apiToken: string;
    username: string;
  };
}

export interface ExportResult {
  success: boolean;
  format: ExportFormat;
  content?: string | Buffer;
  url?: string; // For Confluence
  error?: string;
}

export class DocumentationExportService {
  
  /**
   * Export business rules documentation
   */
  async exportRules(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): Promise<ExportResult> {
    try {
      switch (options.format) {
        case 'json':
          return this.exportToJson(project, rules, options);
        case 'markdown':
          return this.exportToMarkdown(project, rules, options);
        case 'html':
          return this.exportToHtml(project, rules, options);
        case 'pdf':
          return this.exportToPdf(project, rules, options);
        case 'word':
          return this.exportToWord(project, rules, options);
        case 'confluence':
          return this.exportToConfluence(project, rules, options);
        default:
          return { success: false, format: options.format, error: 'Unsupported format' };
      }
    } catch (error) {
      return {
        success: false,
        format: options.format,
        error: error instanceof Error ? error.message : 'Export failed',
      };
    }
  }

  private exportToJson(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): ExportResult {
    const data = {
      project: {
        id: project.id,
        name: project.name,
        sourceLanguage: project.sourceLanguage,
        targetLanguage: project.targetLanguage,
        exportedAt: new Date().toISOString(),
      },
      rules: rules.map(rule => ({
        id: rule.id,
        name: rule.name,
        description: rule.description,
        category: rule.category,
        confidence: rule.confidence,
        reviewStatus: rule.reviewStatus,
        inputs: rule.inputs,
        outputs: rule.outputs,
        logic: rule.logic,
        formula: rule.formula,
        edgeCases: rule.edgeCases,
        ...(options.includeSourceCode && { sourceCode: rule.sourceCode }),
      })),
      summary: {
        totalRules: rules.length,
        approved: rules.filter(r => r.reviewStatus === 'approved').length,
        pending: rules.filter(r => r.reviewStatus === 'pending').length,
        averageConfidence: rules.reduce((sum, r) => sum + r.confidence, 0) / rules.length,
      },
    };

    return {
      success: true,
      format: 'json',
      content: JSON.stringify(data, null, 2),
    };
  }

  private exportToMarkdown(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): ExportResult {
    const lines: string[] = [];

    // Header
    lines.push(`# ${project.name} - Business Rules Documentation`);
    lines.push('');
    lines.push(`**Source Language:** ${project.sourceLanguage.toUpperCase()}`);
    lines.push(`**Target Language:** ${project.targetLanguage.charAt(0).toUpperCase() + project.targetLanguage.slice(1)}`);
    lines.push(`**Export Date:** ${new Date().toLocaleDateString()}`);
    lines.push('');

    // Summary
    lines.push('## Summary');
    lines.push('');
    lines.push(`| Metric | Value |`);
    lines.push(`|--------|-------|`);
    lines.push(`| Total Rules | ${rules.length} |`);
    lines.push(`| Approved | ${rules.filter(r => r.reviewStatus === 'approved').length} |`);
    lines.push(`| Pending Review | ${rules.filter(r => r.reviewStatus === 'pending').length} |`);
    lines.push(`| Avg. Confidence | ${(rules.reduce((s, r) => s + r.confidence, 0) / rules.length * 100).toFixed(1)}% |`);
    lines.push('');

    // Rules by category
    const categories = [...new Set(rules.map(r => r.category))];
    
    for (const category of categories) {
      const categoryRules = rules.filter(r => r.category === category);
      
      lines.push(`## ${category.charAt(0).toUpperCase() + category.slice(1)} Rules`);
      lines.push('');

      for (const rule of categoryRules) {
        lines.push(`### ${rule.name}`);
        lines.push('');
        lines.push(`**ID:** ${rule.id}`);
        lines.push(`**Confidence:** ${(rule.confidence * 100).toFixed(0)}%`);
        lines.push(`**Status:** ${rule.reviewStatus}`);
        lines.push('');
        lines.push(`**Description:**`);
        lines.push(rule.description);
        lines.push('');

        if (rule.inputs.length > 0) {
          lines.push('**Inputs:**');
          for (const input of rule.inputs) {
            lines.push(`- \`${input.name}\` (${input.type}): ${input.description || ''}`);
          }
          lines.push('');
        }

        if (rule.outputs.length > 0) {
          lines.push('**Outputs:**');
          for (const output of rule.outputs) {
            lines.push(`- \`${output.name}\` (${output.type}): ${output.description || ''}`);
          }
          lines.push('');
        }

        if (rule.formula) {
          lines.push('**Formula:**');
          lines.push('```');
          lines.push(rule.formula);
          lines.push('```');
          lines.push('');
        }

        if (options.includeSourceCode && rule.sourceCode) {
          lines.push('**Source Code:**');
          lines.push(`\`\`\`${project.sourceLanguage}`);
          lines.push(rule.sourceCode);
          lines.push('```');
          lines.push('');
        }

        if (rule.edgeCases.length > 0) {
          lines.push('**Edge Cases:**');
          for (const edge of rule.edgeCases) {
            lines.push(`- ${edge}`);
          }
          lines.push('');
        }

        lines.push('---');
        lines.push('');
      }
    }

    return {
      success: true,
      format: 'markdown',
      content: lines.join('\n'),
    };
  }

  private exportToHtml(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): ExportResult {
    const markdown = this.exportToMarkdown(project, rules, options);
    if (!markdown.success || !markdown.content) {
      return { success: false, format: 'html', error: 'Failed to generate markdown' };
    }

    // Simple markdown to HTML conversion
    let html = markdown.content as string;
    html = html.replace(/^### (.+)$/gm, '<h3>$1</h3>');
    html = html.replace(/^## (.+)$/gm, '<h2>$1</h2>');
    html = html.replace(/^# (.+)$/gm, '<h1>$1</h1>');
    html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');
    html = html.replace(/`([^`]+)`/g, '<code>$1</code>');
    html = html.replace(/```[\w]*\n([\s\S]*?)```/g, '<pre><code>$1</code></pre>');
    html = html.replace(/^- (.+)$/gm, '<li>$1</li>');
    html = html.replace(/(<li>.*<\/li>\n?)+/g, '<ul>$&</ul>');
    html = html.replace(/\n\n/g, '</p><p>');
    html = html.replace(/^---$/gm, '<hr>');

    const fullHtml = `<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>${project.name} - Business Rules</title>
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 900px; margin: 0 auto; padding: 2rem; }
    h1, h2, h3 { color: #1a1a1a; }
    code { background: #f4f4f4; padding: 0.2em 0.4em; border-radius: 3px; }
    pre { background: #1e1e1e; color: #d4d4d4; padding: 1rem; border-radius: 5px; overflow-x: auto; }
    pre code { background: none; padding: 0; }
    table { border-collapse: collapse; width: 100%; }
    th, td { border: 1px solid #ddd; padding: 0.5rem; text-align: left; }
    th { background: #f4f4f4; }
    hr { border: none; border-top: 1px solid #eee; margin: 2rem 0; }
  </style>
</head>
<body>
${html}
</body>
</html>`;

    return {
      success: true,
      format: 'html',
      content: fullHtml,
    };
  }

  private async exportToPdf(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): Promise<ExportResult> {
    // Generate HTML first, then convert to PDF
    const html = this.exportToHtml(project, rules, options);
    if (!html.success) {
      return { success: false, format: 'pdf', error: 'Failed to generate HTML' };
    }

    // In production, use puppeteer or similar
    // For now, return HTML with PDF metadata
    return {
      success: true,
      format: 'pdf',
      content: html.content,
      // Note: Actual PDF generation requires puppeteer or a PDF service
    };
  }

  private async exportToWord(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): Promise<ExportResult> {
    // Generate a simple DOCX-compatible HTML
    const html = this.exportToHtml(project, rules, options);
    if (!html.success) {
      return { success: false, format: 'word', error: 'Failed to generate HTML' };
    }

    // Word can import HTML directly
    // In production, use docx library for proper .docx generation
    return {
      success: true,
      format: 'word',
      content: html.content,
    };
  }

  private async exportToConfluence(
    project: Project,
    rules: BusinessRule[],
    options: ExportOptions
  ): Promise<ExportResult> {
    const config = options.confluenceConfig;
    if (!config) {
      return { success: false, format: 'confluence', error: 'Confluence config required' };
    }

    // Generate Confluence storage format (XHTML-based)
    const content = this.generateConfluenceContent(project, rules, options);

    try {
      const auth = Buffer.from(`${config.username}:${config.apiToken}`).toString('base64');
      
      const response = await fetch(`${config.baseUrl}/rest/api/content`, {
        method: 'POST',
        headers: {
          'Authorization': `Basic ${auth}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          type: 'page',
          title: `${project.name} - Business Rules Documentation`,
          space: { key: config.spaceKey },
          ancestors: config.parentPageId ? [{ id: config.parentPageId }] : undefined,
          body: {
            storage: {
              value: content,
              representation: 'storage',
            },
          },
        }),
      });

      if (!response.ok) {
        const error = await response.text();
        return { success: false, format: 'confluence', error: `Confluence API error: ${error}` };
      }

      const result = await response.json() as { _links: { webui: string } };
      
      return {
        success: true,
        format: 'confluence',
        url: `${config.baseUrl}${result._links.webui}`,
      };
    } catch (error) {
      return {
        success: false,
        format: 'confluence',
        error: error instanceof Error ? error.message : 'Confluence export failed',
      };
    }
  }

  private generateConfluenceContent(
    project: Project,
    rules: BusinessRule[],
    _options: ExportOptions
  ): string {
    const lines: string[] = [];

    lines.push(`<h1>${project.name} - Business Rules</h1>`);
    lines.push(`<p><strong>Source:</strong> ${project.sourceLanguage.toUpperCase()}</p>`);
    lines.push(`<p><strong>Target:</strong> ${project.targetLanguage}</p>`);
    lines.push(`<p><strong>Generated:</strong> ${new Date().toLocaleDateString()}</p>`);

    // Summary table
    lines.push('<h2>Summary</h2>');
    lines.push('<table><tbody>');
    lines.push(`<tr><td>Total Rules</td><td>${rules.length}</td></tr>`);
    lines.push(`<tr><td>Approved</td><td>${rules.filter(r => r.reviewStatus === 'approved').length}</td></tr>`);
    lines.push(`<tr><td>Pending</td><td>${rules.filter(r => r.reviewStatus === 'pending').length}</td></tr>`);
    lines.push('</tbody></table>');

    // Rules
    for (const rule of rules) {
      lines.push(`<h3>${rule.name}</h3>`);
      lines.push(`<ac:structured-macro ac:name="info"><ac:rich-text-body>`);
      lines.push(`<p><strong>ID:</strong> ${rule.id} | <strong>Confidence:</strong> ${(rule.confidence * 100).toFixed(0)}% | <strong>Status:</strong> ${rule.reviewStatus}</p>`);
      lines.push(`</ac:rich-text-body></ac:structured-macro>`);
      lines.push(`<p>${rule.description}</p>`);

      if (rule.formula) {
        lines.push('<ac:structured-macro ac:name="code"><ac:plain-text-body><![CDATA[');
        lines.push(rule.formula);
        lines.push(']]></ac:plain-text-body></ac:structured-macro>');
      }
    }

    return lines.join('\n');
  }
}

export const documentationExportService = new DocumentationExportService();
