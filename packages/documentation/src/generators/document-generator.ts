/**
 * Document Generator
 * 
 * Generates documentation from templates and analysis data with AI enhancement
 */

import { generateId } from '@migrationpilot/core';
import type { BusinessRule, DataStructure } from '@migrationpilot/core';
import type {
  Document,
  DocumentType,
  DocumentSection,
  DocumentTemplate,
  TemplateSectionDefinition,
  GenerationContext,
  GenerationOptions,
  SourceReference,
  AIGenerationRequest,
  AIGenerationResult,
} from '../types.js';
import { getTemplate } from '../templates/index.js';

export interface DocumentGeneratorConfig {
  projectId: string;
  userId: string;
  aiEnabled: boolean;
  aiProvider?: AIContentProvider;
}

export interface AIContentProvider {
  generate(request: AIGenerationRequest): Promise<AIGenerationResult>;
}

const DEFAULT_OPTIONS: GenerationOptions = {
  format: 'markdown',
  includeSourceCode: true,
  includeConfidenceScores: true,
  includeLineNumbers: true,
  maxCodeBlockLines: 50,
  aiEnhance: true,
  detailLevel: 'standard',
};

export class DocumentGenerator {
  private config: DocumentGeneratorConfig;
  private options: GenerationOptions;

  constructor(config: DocumentGeneratorConfig, options?: Partial<GenerationOptions>) {
    this.config = config;
    this.options = { ...DEFAULT_OPTIONS, ...options };
  }

  /**
   * Get current generation options
   */
  getOptions(): GenerationOptions {
    return this.options;
  }

  /**
   * Generate a document from a template
   */
  async generate(
    type: DocumentType,
    context: GenerationContext
  ): Promise<Document> {
    const template = getTemplate(type);
    if (!template) {
      throw new Error(`No template found for document type: ${type}`);
    }

    const document: Document = {
      id: generateId(),
      projectId: this.config.projectId,
      type,
      title: this.generateTitle(template, context),
      version: '1.0',
      sections: [],
      generatedAt: new Date(),
      generatedBy: this.config.userId,
      lastModified: new Date(),
      sourceFiles: this.extractSourceFiles(context),
      businessRulesReferenced: this.extractRuleIds(context),
      completeness: 0,
      confidenceScore: 0,
      reviewStatus: 'draft',
    };

    // Generate sections from template
    document.sections = await this.generateSections(
      template.sections,
      context
    );

    // Calculate document metrics
    document.completeness = this.calculateCompleteness(document.sections);
    document.confidenceScore = this.calculateConfidence(document.sections);

    return document;
  }

  /**
   * Generate sections recursively
   */
  private async generateSections(
    templateSections: TemplateSectionDefinition[],
    context: GenerationContext
  ): Promise<DocumentSection[]> {
    const sections: DocumentSection[] = [];

    for (const templateSection of templateSections) {
      const section = await this.generateSection(templateSection, context);
      if (section) {
        sections.push(section);
      }
    }

    return sections;
  }

  /**
   * Generate a single section
   */
  private async generateSection(
    template: TemplateSectionDefinition,
    context: GenerationContext
  ): Promise<DocumentSection | null> {
    let content = '';
    let confidence = 1;
    let aiGenerated = false;
    let sourceReferences: SourceReference[] = [];

    switch (template.contentType) {
      case 'static':
        content = template.staticContent || '';
        break;

      case 'dynamic': {
        const dynamicResult = this.generateDynamicContent(
          template.dynamicContentKey!,
          context
        );
        content = dynamicResult.content;
        sourceReferences = dynamicResult.references;
        break;
      }

      case 'ai-generated':
        if (this.config.aiEnabled && this.config.aiProvider && template.aiPrompt) {
          const aiResult = await this.generateAIContent(
            template.aiPrompt,
            context
          );
          content = aiResult.content;
          confidence = aiResult.confidence;
          sourceReferences = aiResult.sourceReferences;
          aiGenerated = true;
        } else {
          content = `*[AI content generation disabled. Prompt: ${template.aiPrompt}]*`;
          confidence = 0;
        }
        break;
    }

    // Skip empty non-required sections
    if (!content && !template.required) {
      return null;
    }

    // Generate subsections
    let subsections: DocumentSection[] | undefined;
    if (template.subsections) {
      subsections = await this.generateSections(template.subsections, context);
    }

    return {
      id: template.id,
      title: template.title,
      level: template.level,
      content,
      subsections,
      sourceReferences,
      confidence,
      aiGenerated,
    };
  }

  /**
   * Generate dynamic content from analysis data
   */
  private generateDynamicContent(
    key: string,
    context: GenerationContext
  ): { content: string; references: SourceReference[] } {
    const references: SourceReference[] = [];

    switch (key) {
      case 'systemContext':
        return {
          content: this.generateSystemContext(context),
          references,
        };

      case 'technologyStack':
        return {
          content: this.generateTechnologyStack(context),
          references,
        };

      case 'programInventory':
        return {
          content: this.generateProgramInventory(context),
          references: context.analysis.programs.map(p => ({
            type: 'code' as const,
            id: p.id,
            name: p.name,
          })),
        };

      case 'dataAssets':
        return {
          content: this.generateDataAssets(context),
          references,
        };

      case 'businessRulesSummary':
        return {
          content: this.generateBusinessRulesSummary(context),
          references: context.analysis.businessRules.map(r => ({
            type: 'rule' as const,
            id: r.id,
            name: r.name,
          })),
        };

      case 'rulesByCategory':
        return {
          content: this.generateRulesByCategory(context),
          references,
        };

      case 'criticalRules':
        return {
          content: this.generateCriticalRules(context),
          references,
        };

      case 'complexityAnalysis':
        return {
          content: this.generateComplexityAnalysis(context),
          references,
        };

      case 'metricsSummary':
        return {
          content: this.generateMetricsSummary(context),
          references,
        };

      case 'highRiskComponents':
        return {
          content: this.generateHighRiskComponents(context),
          references,
        };

      case 'dataStatistics':
        return {
          content: this.generateDataStatistics(context),
          references,
        };

      case 'dataStructures':
        return {
          content: this.generateDataStructuresContent(context),
          references,
        };

      case 'inputStructures':
        return {
          content: this.generateInputStructures(context),
          references,
        };

      case 'outputStructures':
        return {
          content: this.generateOutputStructures(context),
          references,
        };

      case 'workingStorage':
        return {
          content: this.generateWorkingStorage(context),
          references,
        };

      case 'rulesSummary':
        return {
          content: this.generateRulesCatalogSummary(context),
          references,
        };

      case 'fullRuleCatalog':
        return {
          content: this.generateFullRuleCatalog(context),
          references: context.analysis.businessRules.map(r => ({
            type: 'rule' as const,
            id: r.id,
            name: r.name,
            location: r.sourceFile ? {
              file: r.sourceFile,
              startLine: r.sourceLines[0],
              endLine: r.sourceLines[1],
            } : undefined,
          })),
        };

      default:
        return { content: `*[Content for "${key}" not implemented]*`, references };
    }
  }

  /**
   * Generate AI-enhanced content
   */
  private async generateAIContent(
    promptTemplate: string,
    context: GenerationContext
  ): Promise<AIGenerationResult> {
    if (!this.config.aiProvider) {
      return {
        content: '*[AI content generation not available]*',
        confidence: 0,
        sourceReferences: [],
      };
    }

    // Substitute variables in prompt
    const prompt = this.substituteVariables(promptTemplate, context);

    const request: AIGenerationRequest = {
      documentType: 'system-overview', // Default
      section: 'general',
      context: {
        projectName: context.project.name,
        sourceLanguage: context.project.sourceLanguage,
        targetLanguage: context.project.targetLanguage,
        relevantRules: context.analysis.businessRules.slice(0, 10),
        relevantData: context.analysis.dataStructures.slice(0, 10),
      },
      instructions: prompt,
      maxLength: 2000,
      temperature: 0.3,
    };

    try {
      return await this.config.aiProvider.generate(request);
    } catch (error) {
      return {
        content: `*[AI generation failed: ${error instanceof Error ? error.message : 'Unknown error'}]*`,
        confidence: 0,
        sourceReferences: [],
        warnings: ['AI content generation failed'],
      };
    }
  }

  // Content generation helpers

  private generateTitle(template: DocumentTemplate, context: GenerationContext): string {
    return `${context.project.name} - ${template.name}`;
  }

  private generateSystemContext(context: GenerationContext): string {
    return `## System Information

| Property | Value |
|----------|-------|
| **Project Name** | ${context.project.name} |
| **Source Language** | ${context.project.sourceLanguage} |
| **Target Language** | ${context.project.targetLanguage} |
| **Total Files** | ${context.analysis.metrics.totalFiles} |
| **Total Lines of Code** | ${context.analysis.metrics.totalLines.toLocaleString()} |
| **Business Rules** | ${context.analysis.metrics.totalBusinessRules} |
`;
  }

  private generateTechnologyStack(context: GenerationContext): string {
    const sourceInfo = this.getLanguageInfo(context.project.sourceLanguage);
    const targetInfo = this.getLanguageInfo(context.project.targetLanguage);

    return `### Source Technology

| Component | Technology |
|-----------|------------|
| Language | ${sourceInfo.name} |
| Era | ${sourceInfo.era} |

### Target Technology

| Component | Technology |
|-----------|------------|
| Language | ${targetInfo.name} |
| Era | ${targetInfo.era} |
`;
  }

  private generateProgramInventory(context: GenerationContext): string {
    if (context.analysis.programs.length === 0) {
      return '*No programs analyzed yet.*';
    }

    let content = `| Program | Type | Lines | Complexity | Description |
|---------|------|-------|------------|-------------|
`;

    for (const program of context.analysis.programs) {
      content += `| ${program.name} | ${program.type} | ${program.linesOfCode} | ${program.complexity} | ${program.description || '-'} |\n`;
    }

    return content;
  }

  private generateDataAssets(context: GenerationContext): string {
    const structures = context.analysis.dataStructures;
    
    return `### Data Structures Summary

- **Total Structures**: ${structures.length}
- **Group Items**: ${structures.filter(s => s.type === 'group').length}
- **Elementary Items**: ${structures.filter(s => s.type !== 'group').length}
`;
  }

  private generateBusinessRulesSummary(context: GenerationContext): string {
    const rules = context.analysis.businessRules;
    const byCategory = this.groupRulesByCategory(rules);
    const avgConfidence = rules.length > 0
      ? rules.reduce((sum, r) => sum + r.confidence, 0) / rules.length
      : 0;

    let content = `### Business Rules Overview

- **Total Rules Extracted**: ${rules.length}
- **Average Confidence**: ${(avgConfidence * 100).toFixed(1)}%
- **Rules Requiring Review**: ${rules.filter(r => r.confidence < 0.8).length}

### Distribution by Category

| Category | Count | Avg Confidence |
|----------|-------|----------------|
`;

    for (const [category, categoryRules] of Object.entries(byCategory)) {
      const catAvg = categoryRules.reduce((s, r) => s + r.confidence, 0) / categoryRules.length;
      content += `| ${category} | ${categoryRules.length} | ${(catAvg * 100).toFixed(0)}% |\n`;
    }

    return content;
  }

  private generateRulesByCategory(context: GenerationContext): string {
    const byCategory = this.groupRulesByCategory(context.analysis.businessRules);
    let content = '';

    for (const [category, rules] of Object.entries(byCategory)) {
      content += `\n### ${this.formatCategoryName(category)} (${rules.length})\n\n`;
      for (const rule of rules.slice(0, 5)) {
        content += `- **${rule.name}** (${(rule.confidence * 100).toFixed(0)}% confidence)\n`;
        content += `  ${rule.description}\n`;
      }
      if (rules.length > 5) {
        content += `\n*...and ${rules.length - 5} more ${category} rules*\n`;
      }
    }

    return content;
  }

  private generateCriticalRules(context: GenerationContext): string {
    const critical = context.analysis.businessRules
      .filter(r => r.confidence >= 0.9 && r.category === 'calculation')
      .slice(0, 10);

    if (critical.length === 0) {
      return '*No critical rules identified.*';
    }

    let content = '';
    for (const rule of critical) {
      content += `### ${rule.name}\n\n`;
      content += `**Category**: ${rule.category}\n`;
      content += `**Confidence**: ${(rule.confidence * 100).toFixed(0)}%\n\n`;
      content += `${rule.description}\n\n`;
      if (rule.formula) {
        content += `**Formula**: \`${rule.formula}\`\n\n`;
      }
      if (rule.edgeCases.length > 0) {
        content += `**Edge Cases**:\n`;
        for (const ec of rule.edgeCases) {
          content += `- ${ec}\n`;
        }
        content += '\n';
      }
    }

    return content;
  }

  private generateComplexityAnalysis(context: GenerationContext): string {
    const { metrics } = context.analysis;
    
    return `## Complexity Metrics

| Metric | Value | Assessment |
|--------|-------|------------|
| Total Lines | ${metrics.totalLines.toLocaleString()} | ${this.assessSize(metrics.totalLines)} |
| Total Procedures | ${metrics.totalProcedures} | - |
| Average Complexity | ${metrics.averageComplexity.toFixed(1)} | ${this.assessComplexity(metrics.averageComplexity)} |
| High-Risk Components | ${metrics.highRiskCount} | ${metrics.highRiskCount > 0 ? '⚠️ Attention needed' : '✅ OK'} |
`;
  }

  private generateMetricsSummary(context: GenerationContext): string {
    return this.generateComplexityAnalysis(context);
  }

  private generateHighRiskComponents(context: GenerationContext): string {
    const highComplexity = context.analysis.procedures
      .filter(p => p.complexity > 20)
      .sort((a, b) => b.complexity - a.complexity)
      .slice(0, 10);

    if (highComplexity.length === 0) {
      return '*No high-risk components identified.*';
    }

    let content = `| Procedure | Complexity | Lines | Risk Level |
|-----------|------------|-------|------------|
`;

    for (const proc of highComplexity) {
      const risk = proc.complexity > 50 ? '🔴 Critical' : proc.complexity > 30 ? '🟠 High' : '🟡 Medium';
      const lines = proc.location.endLine - proc.location.startLine;
      content += `| ${proc.name} | ${proc.complexity} | ${lines} | ${risk} |\n`;
    }

    return content;
  }

  private generateDataStatistics(context: GenerationContext): string {
    const structures = context.analysis.dataStructures;
    const byType = this.groupDataByType(structures);

    let content = `| Category | Count |
|----------|-------|
`;
    for (const [type, items] of Object.entries(byType)) {
      content += `| ${type} | ${items.length} |\n`;
    }

    return content;
  }

  private generateDataStructuresContent(_context: GenerationContext): string {
    return `See subsections for detailed data structure documentation.`;
  }

  private generateInputStructures(context: GenerationContext): string {
    // Filter for input-related structures (simplified)
    const inputs = context.analysis.dataStructures.slice(0, 10);
    return this.formatDataStructureTable(inputs);
  }

  private generateOutputStructures(context: GenerationContext): string {
    const outputs = context.analysis.dataStructures.slice(0, 10);
    return this.formatDataStructureTable(outputs);
  }

  private generateWorkingStorage(context: GenerationContext): string {
    const working = context.analysis.dataStructures.slice(0, 10);
    return this.formatDataStructureTable(working);
  }

  private formatDataStructureTable(structures: DataStructure[]): string {
    if (structures.length === 0) {
      return '*No data structures found.*';
    }

    let content = `| Name | Type | Level | Picture | Description |
|------|------|-------|---------|-------------|
`;

    for (const ds of structures) {
      content += `| ${ds.name} | ${ds.type} | ${ds.level || '-'} | ${ds.picture || '-'} | - |\n`;
    }

    return content;
  }

  private generateRulesCatalogSummary(context: GenerationContext): string {
    const rules = context.analysis.businessRules;
    const avgConfidence = rules.length > 0
      ? rules.reduce((sum, r) => sum + r.confidence, 0) / rules.length
      : 0;

    return `## Summary

| Metric | Value |
|--------|-------|
| Total Business Rules | ${rules.length} |
| Average Confidence | ${(avgConfidence * 100).toFixed(1)}% |
| Approved Rules | ${rules.filter(r => r.reviewStatus === 'approved').length} |
| Pending Review | ${rules.filter(r => r.reviewStatus === 'pending').length} |
| Needs Clarification | ${rules.filter(r => r.reviewStatus === 'needs_clarification').length} |
`;
  }

  private generateFullRuleCatalog(context: GenerationContext): string {
    let content = '';

    for (const rule of context.analysis.businessRules) {
      content += `---\n\n### ${rule.id}: ${rule.name}\n\n`;
      content += `**Category**: ${rule.category} | `;
      content += `**Confidence**: ${(rule.confidence * 100).toFixed(0)}% | `;
      content += `**Status**: ${rule.reviewStatus}\n\n`;
      content += `**Description**: ${rule.description}\n\n`;

      if (rule.inputs.length > 0) {
        content += `**Inputs**:\n`;
        for (const input of rule.inputs) {
          content += `- \`${input.name}\` (${input.type}): ${input.description || 'No description'}\n`;
        }
        content += '\n';
      }

      if (rule.outputs.length > 0) {
        content += `**Outputs**:\n`;
        for (const output of rule.outputs) {
          content += `- \`${output.name}\` (${output.type}): ${output.description || 'No description'}\n`;
        }
        content += '\n';
      }

      content += `**Logic**:\n> ${rule.logic}\n\n`;

      if (rule.formula) {
        content += `**Formula**: \`${rule.formula}\`\n\n`;
      }

      if (rule.edgeCases.length > 0) {
        content += `**Edge Cases**:\n`;
        for (const ec of rule.edgeCases) {
          content += `- ${ec}\n`;
        }
        content += '\n';
      }

      content += `**Source**: ${rule.sourceFile}:${rule.sourceLines[0]}-${rule.sourceLines[1]}\n\n`;
    }

    return content;
  }

  // Utility methods

  private substituteVariables(template: string, context: GenerationContext): string {
    return template
      .replace(/\{\{projectName\}\}/g, context.project.name)
      .replace(/\{\{sourceLanguage\}\}/g, context.project.sourceLanguage)
      .replace(/\{\{targetLanguage\}\}/g, context.project.targetLanguage)
      .replace(/\{\{totalFiles\}\}/g, String(context.analysis.metrics.totalFiles))
      .replace(/\{\{totalLines\}\}/g, String(context.analysis.metrics.totalLines));
  }

  private groupRulesByCategory(rules: BusinessRule[]): Record<string, BusinessRule[]> {
    const grouped: Record<string, BusinessRule[]> = {};
    for (const rule of rules) {
      if (!grouped[rule.category]) {
        grouped[rule.category] = [];
      }
      grouped[rule.category]!.push(rule);
    }
    return grouped;
  }

  private groupDataByType(structures: DataStructure[]): Record<string, DataStructure[]> {
    const grouped: Record<string, DataStructure[]> = {};
    for (const ds of structures) {
      if (!grouped[ds.type]) {
        grouped[ds.type] = [];
      }
      grouped[ds.type]!.push(ds);
    }
    return grouped;
  }

  private formatCategoryName(category: string): string {
    return category.charAt(0).toUpperCase() + category.slice(1);
  }

  private getLanguageInfo(language: string): { name: string; era: string } {
    const info: Record<string, { name: string; era: string }> = {
      'cobol': { name: 'COBOL', era: '1959-present' },
      'fortran': { name: 'Fortran', era: '1957-present' },
      'vb6': { name: 'Visual Basic 6', era: '1998-2008' },
      'vba': { name: 'VBA', era: '1993-present' },
      'java-legacy': { name: 'Java (Legacy J2EE)', era: '2000s' },
      'java': { name: 'Java', era: 'Modern' },
      'python': { name: 'Python', era: 'Modern' },
      'typescript': { name: 'TypeScript', era: 'Modern' },
      'go': { name: 'Go', era: 'Modern' },
      'csharp': { name: 'C#', era: 'Modern' },
    };
    return info[language] || { name: language, era: 'Unknown' };
  }

  private assessSize(lines: number): string {
    if (lines < 10000) return '✅ Small';
    if (lines < 50000) return '🟡 Medium';
    if (lines < 200000) return '🟠 Large';
    return '🔴 Very Large';
  }

  private assessComplexity(complexity: number): string {
    if (complexity < 10) return '✅ Low';
    if (complexity < 20) return '🟡 Moderate';
    if (complexity < 30) return '🟠 High';
    return '🔴 Very High';
  }

  private extractSourceFiles(context: GenerationContext): string[] {
    return context.analysis.programs.map(p => p.name);
  }

  private extractRuleIds(context: GenerationContext): string[] {
    return context.analysis.businessRules.map(r => r.id);
  }

  private calculateCompleteness(sections: DocumentSection[]): number {
    let total = 0;
    let filled = 0;

    const countSections = (secs: DocumentSection[]) => {
      for (const sec of secs) {
        total++;
        if (sec.content && sec.content.length > 10) {
          filled++;
        }
        if (sec.subsections) {
          countSections(sec.subsections);
        }
      }
    };

    countSections(sections);
    return total > 0 ? filled / total : 0;
  }

  private calculateConfidence(sections: DocumentSection[]): number {
    const confidences: number[] = [];

    const collectConfidences = (secs: DocumentSection[]) => {
      for (const sec of secs) {
        if (sec.confidence !== undefined) {
          confidences.push(sec.confidence);
        }
        if (sec.subsections) {
          collectConfidences(sec.subsections);
        }
      }
    };

    collectConfidences(sections);
    return confidences.length > 0
      ? confidences.reduce((a, b) => a + b, 0) / confidences.length
      : 1;
  }
}
