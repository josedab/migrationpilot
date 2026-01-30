/**
 * Base Code Generator
 * 
 * Abstract base class for all target language generators
 */

import type {
  GeneratorConfig,
  GeneratedFile,
  GenerationResult,
  ModuleDesign,
  SourceMapping,
  CodeMappingDocument,
  GenerationStats,
  GenerationWarning,
  GeneratorBusinessRule,
  GeneratorDataStructure,
  GeneratorProcedure,
} from './types.js';

export interface LegacyAnalysis {
  rules: GeneratorBusinessRule[];
  dataStructures: GeneratorDataStructure[];
  procedures: GeneratorProcedure[];
  sourceFile: string;
}

export abstract class BaseGenerator {
  protected config: GeneratorConfig;
  protected warnings: GenerationWarning[] = [];
  protected mappings: SourceMapping[] = [];

  constructor(config: GeneratorConfig) {
    this.config = config;
  }

  /**
   * Generate code from legacy analysis
   */
  async generate(
    analysis: LegacyAnalysis,
    design: ModuleDesign
  ): Promise<GenerationResult> {
    this.warnings = [];
    this.mappings = [];

    const files: GeneratedFile[] = [];

    // Generate models/data classes
    const models = this.generateModels(analysis.dataStructures);
    files.push(...models);

    // Generate service interfaces
    const interfaces = this.generateInterfaces(design);
    files.push(...interfaces);

    // Generate service implementations
    const services = this.generateServices(design, analysis.rules);
    files.push(...services);

    // Generate tests if enabled
    if (this.config.generateTests) {
      const tests = this.generateTests(design, analysis.rules);
      files.push(...tests);
    }

    // Generate documentation if enabled
    if (this.config.generateDocs) {
      const docs = this.generateDocumentation(design, analysis);
      files.push(...docs);
    }

    // Build code mapping document
    const mappingDoc = this.buildMappingDocument(analysis, files);

    // Calculate stats
    const stats = this.calculateStats(files);

    return {
      files,
      mappings: mappingDoc,
      stats,
      warnings: this.warnings,
    };
  }

  /**
   * Generate model/data classes from data structures
   */
  protected abstract generateModels(structures: GeneratorDataStructure[]): GeneratedFile[];

  /**
   * Generate service interfaces
   */
  protected abstract generateInterfaces(design: ModuleDesign): GeneratedFile[];

  /**
   * Generate service implementations
   */
  protected abstract generateServices(
    design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[];

  /**
   * Generate unit tests
   */
  protected abstract generateTests(
    design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[];

  /**
   * Generate documentation
   */
  protected generateDocumentation(
    design: ModuleDesign,
    analysis: LegacyAnalysis
  ): GeneratedFile[] {
    const content = this.generateMarkdownDoc(design, analysis);
    return [{
      path: `${this.config.outputDir}/README.md`,
      content,
      type: 'documentation',
    }];
  }

  /**
   * Map legacy type to target language type
   */
  protected abstract mapType(legacyType: string): string;

  /**
   * Get file extension for target language
   */
  protected abstract getFileExtension(): string;

  /**
   * Format code according to style config
   */
  protected formatCode(code: string): string {
    const lines = code.split('\n');
    const indent = this.config.codeStyle.useTabs 
      ? '\t' 
      : ' '.repeat(this.config.codeStyle.indentSize);
    
    return lines
      .map(line => {
        // Basic formatting - real implementation would use language-specific formatter
        const trimmed = line.trimStart();
        const indentLevel = (line.length - trimmed.length) / 2;
        return indent.repeat(indentLevel) + trimmed;
      })
      .join('\n');
  }

  /**
   * Add a source mapping
   */
  protected addMapping(
    generatedLine: number,
    sourcePath: string,
    sourceLine: number,
    description?: string
  ): void {
    this.mappings.push({
      generatedLine,
      sourcePath,
      sourceLine,
      description,
    });
  }

  /**
   * Add a warning
   */
  protected addWarning(
    severity: GenerationWarning['severity'],
    message: string,
    sourcePath?: string,
    sourceLine?: number
  ): void {
    this.warnings.push({ severity, message, sourcePath, sourceLine });
  }

  /**
   * Build the code mapping document
   */
  private buildMappingDocument(
    analysis: LegacyAnalysis,
    files: GeneratedFile[]
  ): CodeMappingDocument {
    const targetFiles = files.filter(f => f.type === 'source').map(f => f.path);

    return {
      version: '1.0',
      sourceLanguage: 'cobol', // Would come from analysis
      targetLanguage: this.config.targetLanguage,
      mappings: [{
        sourceFile: analysis.sourceFile,
        targetFiles,
        description: 'Main module migration',
      }],
      ruleMappings: analysis.rules.map(rule => ({
        ruleId: rule.id,
        ruleName: rule.name,
        sourceLocations: rule.sourceLocation ? [{
          file: rule.sourceLocation.file,
          startLine: rule.sourceLocation.startLine,
          endLine: rule.sourceLocation.endLine,
        }] : [],
        targetLocations: this.findRuleTargetLocations(rule, files),
      })),
    };
  }

  /**
   * Find where a rule was implemented in target code
   */
  private findRuleTargetLocations(rule: GeneratorBusinessRule, files: GeneratedFile[]) {
    // Search for rule references in generated files
    const locations: { file: string; startLine: number; endLine: number; functionName?: string }[] = [];
    
    const ruleId = rule.id ?? '';
    const ruleName = rule.name ?? '';
    
    for (const file of files) {
      const lines = file.content.split('\n');
      for (let i = 0; i < lines.length; i++) {
        const line = lines[i] ?? '';
        if ((ruleId && line.includes(ruleId)) || (ruleName && line.includes(ruleName))) {
          // Found a reference to this rule
          let endLine = i;
          // Find end of function/method
          let braceCount = 0;
          for (let j = i; j < lines.length; j++) {
            const currentLine = lines[j] ?? '';
            braceCount += (currentLine.match(/{/g) || []).length;
            braceCount -= (currentLine.match(/}/g) || []).length;
            if (braceCount === 0 && j > i) {
              endLine = j;
              break;
            }
          }
          locations.push({
            file: file.path,
            startLine: i + 1,
            endLine: endLine + 1,
          });
        }
      }
    }
    
    return locations;
  }

  /**
   * Calculate generation statistics
   */
  private calculateStats(files: GeneratedFile[]): GenerationStats {
    const sourceFiles = files.filter(f => f.type === 'source');
    const testFiles = files.filter(f => f.type === 'test');
    
    let totalLines = 0;
    for (const file of files) {
      totalLines += file.content.split('\n').length;
    }

    return {
      totalFiles: files.length,
      totalLines,
      sourceFiles: sourceFiles.length,
      testFiles: testFiles.length,
      coverageEstimate: testFiles.length > 0 ? 0.8 : 0,
    };
  }

  /**
   * Generate markdown documentation
   */
  private generateMarkdownDoc(design: ModuleDesign, analysis: LegacyAnalysis): string {
    return `# ${design.name}

${design.description}

## Overview

This module was migrated from legacy code using MigrationPilot.

**Source File:** ${analysis.sourceFile}
**Business Rules:** ${analysis.rules.length}
**Data Structures:** ${analysis.dataStructures.length}

## Classes

${design.classes.map(cls => `### ${cls.name}

${cls.description}

**Fields:**
${cls.fields.map(f => `- \`${f.name}\`: ${f.type} - ${f.description || 'No description'}`).join('\n')}

**Methods:**
${cls.methods.map(m => `- \`${m.name}()\`: ${m.description}`).join('\n')}
`).join('\n')}

## Business Rules

${analysis.rules.map(rule => `### ${rule.name} (${rule.id})

${rule.description}

**Confidence:** ${(rule.confidence * 100).toFixed(1)}%
`).join('\n')}

---
*Generated by MigrationPilot*
`;
  }
}
