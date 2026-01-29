/**
 * Semantic Diff Tool
 * 
 * Compares legacy and modern code by mapping business logic, not just syntax.
 * Shows how business rules are preserved across the migration.
 */

import type { BusinessRule, SourceLocation } from '@migrationpilot/core';

export interface SemanticMapping {
  id: string;
  legacyCode: CodeFragment;
  modernCode: CodeFragment;
  businessRule?: {
    id: string;
    name: string;
    description: string;
  };
  mappingType: 'direct' | 'refactored' | 'split' | 'merged' | 'new' | 'removed';
  confidence: number;
  notes?: string;
}

export interface CodeFragment {
  language: string;
  filename: string;
  startLine: number;
  endLine: number;
  content: string;
  annotations?: CodeAnnotation[];
}

export interface CodeAnnotation {
  line: number;
  type: 'business-logic' | 'data-access' | 'calculation' | 'validation' | 'control-flow';
  description: string;
}

export interface SemanticDiffResult {
  projectId: string;
  timestamp: string;
  summary: {
    totalMappings: number;
    directMappings: number;
    refactoredMappings: number;
    splitMappings: number;
    mergedMappings: number;
    newCode: number;
    removedCode: number;
    averageConfidence: number;
    coveragePercentage: number;
  };
  mappings: SemanticMapping[];
  unmappedLegacy: CodeFragment[];
  unmappedModern: CodeFragment[];
  riskAreas: RiskArea[];
}

export interface RiskArea {
  legacyLocation: SourceLocation;
  modernLocation?: SourceLocation;
  riskLevel: 'low' | 'medium' | 'high' | 'critical';
  reason: string;
  recommendation: string;
}

export interface DiffOptions {
  includeAnnotations?: boolean;
  highlightBusinessLogic?: boolean;
  showUnmapped?: boolean;
  confidenceThreshold?: number;
  groupByRule?: boolean;
}

export class SemanticDiffTool {
  private defaultOptions: Required<DiffOptions> = {
    includeAnnotations: true,
    highlightBusinessLogic: true,
    showUnmapped: true,
    confidenceThreshold: 0.5,
    groupByRule: true,
  };

  /**
   * Generate semantic diff between legacy and modern code
   */
  generateDiff(
    legacyCode: string,
    legacyLanguage: string,
    legacyFilename: string,
    modernCode: string,
    modernLanguage: string,
    modernFilename: string,
    businessRules: BusinessRule[],
    options: DiffOptions = {}
  ): SemanticDiffResult {
    const opts = { ...this.defaultOptions, ...options };
    
    // Parse code into fragments
    const legacyFragments = this.parseIntoFragments(legacyCode, legacyLanguage, legacyFilename);
    const modernFragments = this.parseIntoFragments(modernCode, modernLanguage, modernFilename);
    
    // Map business rules to code fragments
    const ruleMappings = this.mapRulesToFragments(businessRules, legacyFragments, modernFragments);
    
    // Generate semantic mappings
    const mappings = this.generateMappings(
      legacyFragments,
      modernFragments,
      ruleMappings,
      opts
    );
    
    // Identify unmapped code
    const mappedLegacyLines = new Set<number>();
    const mappedModernLines = new Set<number>();
    
    for (const mapping of mappings) {
      for (let i = mapping.legacyCode.startLine; i <= mapping.legacyCode.endLine; i++) {
        mappedLegacyLines.add(i);
      }
      for (let i = mapping.modernCode.startLine; i <= mapping.modernCode.endLine; i++) {
        mappedModernLines.add(i);
      }
    }
    
    const unmappedLegacy = opts.showUnmapped 
      ? this.findUnmappedFragments(legacyFragments, mappedLegacyLines)
      : [];
    
    const unmappedModern = opts.showUnmapped
      ? this.findUnmappedFragments(modernFragments, mappedModernLines)
      : [];
    
    // Identify risk areas
    const riskAreas = this.identifyRiskAreas(mappings, unmappedLegacy, businessRules);
    
    // Calculate summary statistics
    const summary = this.calculateSummary(mappings, legacyFragments, modernFragments);
    
    return {
      projectId: '',
      timestamp: new Date().toISOString(),
      summary,
      mappings: mappings.filter(m => m.confidence >= opts.confidenceThreshold),
      unmappedLegacy,
      unmappedModern,
      riskAreas,
    };
  }

  /**
   * Generate side-by-side view HTML
   */
  generateSideBySideView(diff: SemanticDiffResult): string {
    const html: string[] = [];
    
    html.push('<div class="semantic-diff-container">');
    html.push('<style>');
    html.push(`
      .semantic-diff-container { font-family: monospace; }
      .diff-row { display: flex; border-bottom: 1px solid #e5e7eb; }
      .diff-legacy, .diff-modern { flex: 1; padding: 8px; overflow-x: auto; }
      .diff-legacy { background: #fef2f2; }
      .diff-modern { background: #f0fdf4; }
      .mapping-direct { border-left: 4px solid #10b981; }
      .mapping-refactored { border-left: 4px solid #f59e0b; }
      .mapping-split { border-left: 4px solid #3b82f6; }
      .mapping-merged { border-left: 4px solid #8b5cf6; }
      .line-number { color: #9ca3af; margin-right: 16px; min-width: 40px; display: inline-block; }
      .business-logic { background: #fef3c7; }
      .rule-badge { 
        display: inline-block; 
        padding: 2px 8px; 
        border-radius: 4px; 
        font-size: 12px; 
        margin: 4px;
        background: #dbeafe;
        color: #1e40af;
      }
      .confidence-high { color: #10b981; }
      .confidence-medium { color: #f59e0b; }
      .confidence-low { color: #ef4444; }
    `);
    html.push('</style>');
    
    // Summary section
    html.push('<div class="summary">');
    html.push(`<h3>Semantic Diff Summary</h3>`);
    html.push(`<p>Total Mappings: ${diff.summary.totalMappings} | Coverage: ${diff.summary.coveragePercentage}%</p>`);
    html.push('</div>');
    
    // Mappings
    for (const mapping of diff.mappings) {
      html.push(`<div class="diff-row mapping-${mapping.mappingType}">`);
      
      // Legacy side
      html.push('<div class="diff-legacy">');
      if (mapping.businessRule) {
        html.push(`<span class="rule-badge">${mapping.businessRule.name}</span>`);
      }
      html.push(`<div class="confidence-${this.getConfidenceClass(mapping.confidence)}">`);
      html.push(`Confidence: ${Math.round(mapping.confidence * 100)}%`);
      html.push('</div>');
      html.push(this.formatCodeFragment(mapping.legacyCode));
      html.push('</div>');
      
      // Modern side
      html.push('<div class="diff-modern">');
      html.push(`<span class="mapping-type">${mapping.mappingType}</span>`);
      html.push(this.formatCodeFragment(mapping.modernCode));
      html.push('</div>');
      
      html.push('</div>');
    }
    
    html.push('</div>');
    
    return html.join('\n');
  }

  /**
   * Generate JSON export of semantic diff
   */
  exportToJSON(diff: SemanticDiffResult): string {
    return JSON.stringify(diff, null, 2);
  }

  /**
   * Generate markdown report
   */
  generateMarkdownReport(diff: SemanticDiffResult): string {
    const lines: string[] = [];
    
    lines.push('# Semantic Diff Report');
    lines.push('');
    lines.push(`Generated: ${diff.timestamp}`);
    lines.push('');
    
    // Summary
    lines.push('## Summary');
    lines.push('');
    lines.push(`| Metric | Value |`);
    lines.push(`|--------|-------|`);
    lines.push(`| Total Mappings | ${diff.summary.totalMappings} |`);
    lines.push(`| Direct Mappings | ${diff.summary.directMappings} |`);
    lines.push(`| Refactored | ${diff.summary.refactoredMappings} |`);
    lines.push(`| Split | ${diff.summary.splitMappings} |`);
    lines.push(`| Merged | ${diff.summary.mergedMappings} |`);
    lines.push(`| Coverage | ${diff.summary.coveragePercentage}% |`);
    lines.push(`| Avg Confidence | ${Math.round(diff.summary.averageConfidence * 100)}% |`);
    lines.push('');
    
    // Risk Areas
    if (diff.riskAreas.length > 0) {
      lines.push('## ⚠️ Risk Areas');
      lines.push('');
      for (const risk of diff.riskAreas) {
        lines.push(`### ${risk.riskLevel.toUpperCase()}: ${risk.reason}`);
        lines.push(`- Location: ${risk.legacyLocation.file}:${risk.legacyLocation.startLine}`);
        lines.push(`- Recommendation: ${risk.recommendation}`);
        lines.push('');
      }
    }
    
    // Detailed Mappings
    lines.push('## Detailed Mappings');
    lines.push('');
    
    for (const mapping of diff.mappings) {
      lines.push(`### ${mapping.businessRule?.name || 'Code Mapping'}`);
      lines.push(`- Type: ${mapping.mappingType}`);
      lines.push(`- Confidence: ${Math.round(mapping.confidence * 100)}%`);
      lines.push('');
      
      lines.push('**Legacy Code:**');
      lines.push('```' + mapping.legacyCode.language);
      lines.push(mapping.legacyCode.content);
      lines.push('```');
      lines.push('');
      
      lines.push('**Modern Code:**');
      lines.push('```' + mapping.modernCode.language);
      lines.push(mapping.modernCode.content);
      lines.push('```');
      lines.push('');
    }
    
    return lines.join('\n');
  }

  // Private helper methods

  private parseIntoFragments(
    code: string,
    language: string,
    filename: string
  ): CodeFragment[] {
    const lines = code.split('\n');
    const fragments: CodeFragment[] = [];
    
    // Simple fragmentation by logical blocks (in production, use actual parser)
    let currentFragment: { start: number; end: number; content: string[] } | null = null;
    
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i] ?? '';
      const trimmed = line.trim();
      
      // Detect block boundaries based on language
      const isBlockStart = this.isBlockStart(trimmed, language);
      const isBlockEnd = this.isBlockEnd(trimmed, language);
      
      if (isBlockStart || !currentFragment) {
        if (currentFragment && currentFragment.content.length > 0) {
          fragments.push({
            language,
            filename,
            startLine: currentFragment.start + 1,
            endLine: currentFragment.end + 1,
            content: currentFragment.content.join('\n'),
          });
        }
        currentFragment = { start: i, end: i, content: [line] };
      } else {
        currentFragment.content.push(line);
        currentFragment.end = i;
        
        if (isBlockEnd) {
          fragments.push({
            language,
            filename,
            startLine: currentFragment.start + 1,
            endLine: currentFragment.end + 1,
            content: currentFragment.content.join('\n'),
          });
          currentFragment = null;
        }
      }
    }
    
    // Handle remaining fragment
    if (currentFragment && currentFragment.content.length > 0) {
      fragments.push({
        language,
        filename,
        startLine: currentFragment.start + 1,
        endLine: currentFragment.end + 1,
        content: currentFragment.content.join('\n'),
      });
    }
    
    return fragments;
  }

  private isBlockStart(line: string, language: string): boolean {
    if (language === 'cobol') {
      return /^\s*[A-Z0-9-]+\s+(SECTION|DIVISION)\s*\./i.test(line) ||
             /^\s*[A-Z0-9-]+\.\s*$/i.test(line);
    }
    if (['java', 'typescript', 'javascript'].includes(language)) {
      return /^\s*(public|private|protected)?\s*(static)?\s*(class|interface|function|def|async)\s/.test(line) ||
             /^\s*(public|private|protected)?\s*\w+\s*\(/.test(line);
    }
    return false;
  }

  private isBlockEnd(line: string, language: string): boolean {
    if (language === 'cobol') {
      return /^\s*EXIT\s*\./i.test(line) || /^\s*STOP\s+RUN\s*\./i.test(line);
    }
    if (['java', 'typescript', 'javascript'].includes(language)) {
      return /^\s*\}\s*$/.test(line);
    }
    return false;
  }

  private mapRulesToFragments(
    rules: BusinessRule[],
    legacyFragments: CodeFragment[],
    modernFragments: CodeFragment[]
  ): Map<string, { legacy: CodeFragment[]; modern: CodeFragment[] }> {
    const mappings = new Map<string, { legacy: CodeFragment[]; modern: CodeFragment[] }>();
    
    for (const rule of rules) {
      const legacyMatches = legacyFragments.filter(f => 
        f.startLine <= rule.sourceLines[1] && f.endLine >= rule.sourceLines[0]
      );
      
      // For modern code, we'd need source mappings from the builder agent
      // For now, use content similarity
      const modernMatches = modernFragments.filter(f => 
        this.fragmentMatchesRule(f, rule)
      );
      
      mappings.set(rule.id, { legacy: legacyMatches, modern: modernMatches });
    }
    
    return mappings;
  }

  private fragmentMatchesRule(fragment: CodeFragment, rule: BusinessRule): boolean {
    const content = fragment.content.toLowerCase();
    const ruleName = rule.name.toLowerCase().replace(/[^a-z0-9]/g, '');
    
    // Check if fragment contains rule-related identifiers
    return content.includes(ruleName) || 
           Boolean(rule.logic && content.includes(rule.logic.toLowerCase().slice(0, 20)));
  }

  private generateMappings(
    legacyFragments: CodeFragment[],
    modernFragments: CodeFragment[],
    ruleMappings: Map<string, { legacy: CodeFragment[]; modern: CodeFragment[] }>,
    _options: Required<DiffOptions>
  ): SemanticMapping[] {
    const mappings: SemanticMapping[] = [];
    let mappingId = 0;
    
    // Map by rules first
    for (const [ruleId, { legacy, modern }] of ruleMappings) {
      if (legacy.length === 0 || modern.length === 0) continue;
      
      const mappingType = this.determineMappingType(legacy, modern);
      
      // For now, create one-to-one mappings
      for (let i = 0; i < Math.max(legacy.length, modern.length); i++) {
        const legacyFrag = legacy[i] || legacy[0];
        const modernFrag = modern[i] || modern[0];
        
        if (legacyFrag && modernFrag) {
          mappings.push({
            id: `mapping-${mappingId++}`,
            legacyCode: legacyFrag,
            modernCode: modernFrag,
            businessRule: {
              id: ruleId,
              name: `Rule ${ruleId}`,
              description: '',
            },
            mappingType,
            confidence: this.calculateMappingConfidence(legacyFrag, modernFrag),
          });
        }
      }
    }
    
    // Add unmapped fragments as potential mappings using similarity
    const usedLegacy = new Set(mappings.map(m => m.legacyCode.startLine));
    const usedModern = new Set(mappings.map(m => m.modernCode.startLine));
    
    for (const legacy of legacyFragments) {
      if (usedLegacy.has(legacy.startLine)) continue;
      
      let bestMatch: { fragment: CodeFragment; score: number } | null = null;
      
      for (const modern of modernFragments) {
        if (usedModern.has(modern.startLine)) continue;
        
        const score = this.calculateSimilarity(legacy.content, modern.content);
        if (score > 0.3 && (!bestMatch || score > bestMatch.score)) {
          bestMatch = { fragment: modern, score };
        }
      }
      
      if (bestMatch) {
        mappings.push({
          id: `mapping-${mappingId++}`,
          legacyCode: legacy,
          modernCode: bestMatch.fragment,
          mappingType: bestMatch.score > 0.7 ? 'direct' : 'refactored',
          confidence: bestMatch.score,
        });
        usedLegacy.add(legacy.startLine);
        usedModern.add(bestMatch.fragment.startLine);
      }
    }
    
    return mappings;
  }

  private determineMappingType(
    legacy: CodeFragment[],
    modern: CodeFragment[]
  ): SemanticMapping['mappingType'] {
    if (legacy.length === 1 && modern.length === 1) return 'direct';
    if (legacy.length === 1 && modern.length > 1) return 'split';
    if (legacy.length > 1 && modern.length === 1) return 'merged';
    return 'refactored';
  }

  private calculateMappingConfidence(
    legacy: CodeFragment,
    modern: CodeFragment
  ): number {
    // Simple similarity-based confidence
    return this.calculateSimilarity(legacy.content, modern.content);
  }

  private calculateSimilarity(text1: string, text2: string): number {
    // Normalize texts
    const normalize = (t: string) => t.toLowerCase()
      .replace(/[^a-z0-9]/g, ' ')
      .replace(/\s+/g, ' ')
      .trim()
      .split(' ')
      .filter(w => w.length > 2);
    
    const words1 = new Set(normalize(text1));
    const words2 = new Set(normalize(text2));
    
    if (words1.size === 0 || words2.size === 0) return 0;
    
    let intersection = 0;
    for (const word of words1) {
      if (words2.has(word)) intersection++;
    }
    
    return intersection / Math.max(words1.size, words2.size);
  }

  private findUnmappedFragments(
    fragments: CodeFragment[],
    mappedLines: Set<number>
  ): CodeFragment[] {
    return fragments.filter(f => {
      for (let i = f.startLine; i <= f.endLine; i++) {
        if (mappedLines.has(i)) return false;
      }
      return true;
    });
  }

  private identifyRiskAreas(
    mappings: SemanticMapping[],
    unmappedLegacy: CodeFragment[],
    businessRules: BusinessRule[]
  ): RiskArea[] {
    const risks: RiskArea[] = [];
    
    // Low confidence mappings
    for (const mapping of mappings) {
      if (mapping.confidence < 0.5) {
        risks.push({
          legacyLocation: {
            file: mapping.legacyCode.filename,
            startLine: mapping.legacyCode.startLine,
            endLine: mapping.legacyCode.endLine,
          },
          modernLocation: {
            file: mapping.modernCode.filename,
            startLine: mapping.modernCode.startLine,
            endLine: mapping.modernCode.endLine,
          },
          riskLevel: mapping.confidence < 0.3 ? 'high' : 'medium',
          reason: `Low confidence mapping (${Math.round(mapping.confidence * 100)}%)`,
          recommendation: 'Manual review recommended to verify business logic preservation',
        });
      }
    }
    
    // Unmapped legacy code with business rules
    for (const fragment of unmappedLegacy) {
      const affectedRules = businessRules.filter(r =>
        fragment.startLine <= r.sourceLines[1] && fragment.endLine >= r.sourceLines[0]
      );
      
      if (affectedRules.length > 0) {
        risks.push({
          legacyLocation: {
            file: fragment.filename,
            startLine: fragment.startLine,
            endLine: fragment.endLine,
          },
          riskLevel: 'critical',
          reason: `Unmapped legacy code contains ${affectedRules.length} business rule(s)`,
          recommendation: 'Ensure business rules are migrated: ' + 
            affectedRules.map(r => r.name).join(', '),
        });
      }
    }
    
    return risks;
  }

  private calculateSummary(
    mappings: SemanticMapping[],
    legacyFragments: CodeFragment[],
    _modernFragments: CodeFragment[]
  ): SemanticDiffResult['summary'] {
    const direct = mappings.filter(m => m.mappingType === 'direct').length;
    const refactored = mappings.filter(m => m.mappingType === 'refactored').length;
    const split = mappings.filter(m => m.mappingType === 'split').length;
    const merged = mappings.filter(m => m.mappingType === 'merged').length;
    const newCode = mappings.filter(m => m.mappingType === 'new').length;
    const removed = mappings.filter(m => m.mappingType === 'removed').length;
    
    const totalLegacyLines = legacyFragments.reduce(
      (sum, f) => sum + (f.endLine - f.startLine + 1), 0
    );
    const mappedLegacyLines = mappings.reduce(
      (sum, m) => sum + (m.legacyCode.endLine - m.legacyCode.startLine + 1), 0
    );
    
    const avgConfidence = mappings.length > 0
      ? mappings.reduce((sum, m) => sum + m.confidence, 0) / mappings.length
      : 0;
    
    return {
      totalMappings: mappings.length,
      directMappings: direct,
      refactoredMappings: refactored,
      splitMappings: split,
      mergedMappings: merged,
      newCode,
      removedCode: removed,
      averageConfidence: avgConfidence,
      coveragePercentage: totalLegacyLines > 0
        ? Math.round((mappedLegacyLines / totalLegacyLines) * 100)
        : 0,
    };
  }

  private formatCodeFragment(fragment: CodeFragment): string {
    const lines = fragment.content.split('\n');
    return '<pre>' + lines.map((line, i) => 
      `<span class="line-number">${fragment.startLine + i}</span>${this.escapeHtml(line)}`
    ).join('\n') + '</pre>';
  }

  private escapeHtml(text: string): string {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }

  private getConfidenceClass(confidence: number): string {
    if (confidence >= 0.8) return 'high';
    if (confidence >= 0.5) return 'medium';
    return 'low';
  }
}
