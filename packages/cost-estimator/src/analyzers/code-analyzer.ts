/**
 * Code Analyzer
 * Analyzes code samples to extract metrics for cost estimation
 */

import type { SourceLanguage } from '@migrationpilot/core';
import type { CodeMetrics, CodeSample, ComplexityScore, ComplexityLevel, ComplexityFactor, ComplexityBreakdown } from '../types/index.js';
import { LanguageDetector } from './language-detector.js';

interface LanguageConfig {
  commentPatterns: RegExp[];
  procedurePatterns: RegExp[];
  dataStructurePatterns: RegExp[];
  externalCallPatterns: RegExp[];
  controlFlowPatterns: RegExp[];
}

const LANGUAGE_CONFIGS: Record<SourceLanguage, LanguageConfig> = {
  cobol: {
    commentPatterns: [/^\s{6}[*]/m, /^\s*\*>.*/m],
    procedurePatterns: [/^\s+\d{2}\s+\w+\s+SECTION\./gim, /^\s+\w+\.\s*$/gm, /PERFORM\s+\w+/gi],
    dataStructurePatterns: [/^\s+01\s+\w+/gm, /^\s+\d{2}\s+\w+.*PIC/gim],
    externalCallPatterns: [/CALL\s+['"]?\w+['"]?/gi, /EXEC\s+SQL/gi, /EXEC\s+CICS/gi],
    controlFlowPatterns: [/\bIF\b/gi, /\bEVALUATE\b/gi, /\bPERFORM\s+\w+\s+UNTIL\b/gi, /\bPERFORM\s+\w+\s+VARYING\b/gi],
  },
  fortran: {
    commentPatterns: [/^[cC*!]/m, /^\s*!/m],
    procedurePatterns: [/\bSUBROUTINE\s+\w+/gi, /\bFUNCTION\s+\w+/gi, /\bPROGRAM\s+\w+/gi],
    dataStructurePatterns: [/\bCOMMON\s+\/\w+\//gi, /\bTYPE\s*::\s*\w+/gi],
    externalCallPatterns: [/\bCALL\s+\w+/gi, /\bEXTERNAL\s+\w+/gi],
    controlFlowPatterns: [/\bIF\s*\(/gi, /\bDO\s+\d+/gi, /\bDO\s+WHILE/gi, /\bSELECT\s+CASE/gi],
  },
  vb6: {
    commentPatterns: [/^\s*'/m, /^\s*Rem\s/im],
    procedurePatterns: [/\b(Private|Public)\s+(Sub|Function)\s+\w+/gi],
    dataStructurePatterns: [/\bType\s+\w+/gi, /\bClass\s+\w+/gi],
    externalCallPatterns: [/\bDeclare\s+(Sub|Function)/gi, /\bCreateObject\s*\(/gi],
    controlFlowPatterns: [/\bIf\b/gi, /\bSelect\s+Case\b/gi, /\bFor\s+\w+\s*=/gi, /\bDo\s+(While|Until)/gi],
  },
  vba: {
    commentPatterns: [/^\s*'/m],
    procedurePatterns: [/\b(Private|Public)?\s*(Sub|Function)\s+\w+/gi],
    dataStructurePatterns: [/\bType\s+\w+/gi],
    externalCallPatterns: [/\bCreateObject\s*\(/gi, /\bApplication\.\w+/gi],
    controlFlowPatterns: [/\bIf\b/gi, /\bSelect\s+Case\b/gi, /\bFor\s+Each\b/gi, /\bDo\s+(While|Until)/gi],
  },
  'java-legacy': {
    commentPatterns: [/\/\/.*/g, /\/\*[\s\S]*?\*\//g],
    procedurePatterns: [/\b(public|private|protected)\s+\w+\s+\w+\s*\(/gi],
    dataStructurePatterns: [/\bclass\s+\w+/gi, /\binterface\s+\w+/gi],
    externalCallPatterns: [/\bnew\s+\w+\s*\(/gi, /\.\w+\s*\(/g],
    controlFlowPatterns: [/\bif\s*\(/gi, /\bfor\s*\(/gi, /\bwhile\s*\(/gi, /\bswitch\s*\(/gi, /\btry\s*\{/gi],
  },
};

export class CodeAnalyzer {
  private languageDetector: LanguageDetector;

  constructor() {
    this.languageDetector = new LanguageDetector();
  }

  /**
   * Analyze a code sample and extract metrics
   */
  analyze(sample: CodeSample): CodeMetrics {
    const code = sample.content;
    const language = sample.language || this.languageDetector.detect(code).language;
    const config = LANGUAGE_CONFIGS[language];

    const lines = code.split('\n');
    const totalLines = lines.length;

    // Count line types
    let commentLines = 0;
    let blankLines = 0;

    for (const line of lines) {
      const trimmed = line.trim();
      if (trimmed === '') {
        blankLines++;
      } else if (this.isCommentLine(trimmed, config.commentPatterns)) {
        commentLines++;
      }
    }

    const codeLines = totalLines - commentLines - blankLines;

    // Count procedures
    let procedures = 0;
    for (const pattern of config.procedurePatterns) {
      const matches = code.match(pattern);
      procedures += matches?.length || 0;
    }
    procedures = Math.max(1, procedures); // At least 1 procedure

    // Count data structures
    let dataStructures = 0;
    for (const pattern of config.dataStructurePatterns) {
      const matches = code.match(pattern);
      dataStructures += matches?.length || 0;
    }

    // Count external calls
    let externalCalls = 0;
    for (const pattern of config.externalCallPatterns) {
      const matches = code.match(pattern);
      externalCalls += matches?.length || 0;
    }

    // Calculate cyclomatic complexity
    let controlFlowPoints = 0;
    for (const pattern of config.controlFlowPatterns) {
      const matches = code.match(pattern);
      controlFlowPoints += matches?.length || 0;
    }
    const cyclomaticComplexity = controlFlowPoints + 1;

    // Calculate max nesting depth
    const maxNestingDepth = this.calculateNestingDepth(code, language);

    // Count unique operations (simplified)
    const uniqueOperations = this.countUniqueOperations(code);

    return {
      totalLines,
      codeLines,
      commentLines,
      blankLines,
      procedures,
      dataStructures,
      externalCalls,
      cyclomaticComplexity,
      maxNestingDepth,
      uniqueOperations,
    };
  }

  /**
   * Calculate complexity score from metrics
   */
  calculateComplexity(metrics: CodeMetrics, language: SourceLanguage): ComplexityScore {
    const factors: ComplexityFactor[] = [];
    const breakdown: ComplexityBreakdown = {
      codeVolume: 0,
      logicComplexity: 0,
      dataComplexity: 0,
      integrationComplexity: 0,
      languageComplexity: 0,
    };

    // Code volume complexity (0-25)
    breakdown.codeVolume = Math.min(25, Math.log10(metrics.codeLines + 1) * 8);
    if (metrics.codeLines > 5000) {
      factors.push({
        name: 'Large Codebase',
        description: `${metrics.codeLines.toLocaleString()} lines of code require significant analysis`,
        impact: 'high',
        score: 15,
        recommendation: 'Consider breaking into smaller modules for incremental migration',
      });
    } else if (metrics.codeLines > 1000) {
      factors.push({
        name: 'Medium Codebase',
        description: `${metrics.codeLines.toLocaleString()} lines of code`,
        impact: 'medium',
        score: 8,
      });
    }

    // Logic complexity (0-30)
    const ccNormalized = Math.min(30, metrics.cyclomaticComplexity * 0.5);
    breakdown.logicComplexity = ccNormalized;
    if (metrics.cyclomaticComplexity > 50) {
      factors.push({
        name: 'High Cyclomatic Complexity',
        description: `Complexity score of ${metrics.cyclomaticComplexity} indicates highly branched logic`,
        impact: 'high',
        score: 20,
        recommendation: 'Requires careful analysis of all code paths and edge cases',
      });
    } else if (metrics.cyclomaticComplexity > 20) {
      factors.push({
        name: 'Moderate Complexity',
        description: `Complexity score of ${metrics.cyclomaticComplexity}`,
        impact: 'medium',
        score: 10,
      });
    }

    // Nesting depth factor
    if (metrics.maxNestingDepth > 5) {
      factors.push({
        name: 'Deep Nesting',
        description: `Maximum nesting depth of ${metrics.maxNestingDepth} levels`,
        impact: 'medium',
        score: 8,
        recommendation: 'May require refactoring for maintainability',
      });
    }

    // Data complexity (0-20)
    breakdown.dataComplexity = Math.min(20, metrics.dataStructures * 2);
    if (metrics.dataStructures > 20) {
      factors.push({
        name: 'Complex Data Structures',
        description: `${metrics.dataStructures} data structures require careful mapping`,
        impact: 'medium',
        score: 10,
      });
    }

    // Integration complexity (0-15)
    breakdown.integrationComplexity = Math.min(15, metrics.externalCalls * 1.5);
    if (metrics.externalCalls > 10) {
      factors.push({
        name: 'External Dependencies',
        description: `${metrics.externalCalls} external calls require interface adaptation`,
        impact: 'high',
        score: 12,
        recommendation: 'Integration testing will be critical',
      });
    }

    // Language complexity (0-10)
    const languageMultipliers: Record<SourceLanguage, number> = {
      cobol: 10,
      fortran: 8,
      vb6: 6,
      vba: 5,
      'java-legacy': 4,
    };
    breakdown.languageComplexity = languageMultipliers[language] || 5;

    if (language === 'cobol') {
      factors.push({
        name: 'COBOL Migration',
        description: 'COBOL requires specialized knowledge for accurate conversion',
        impact: 'medium',
        score: 8,
      });
    }

    // Calculate overall score
    const overall = Math.min(100, Math.round(
      breakdown.codeVolume +
      breakdown.logicComplexity +
      breakdown.dataComplexity +
      breakdown.integrationComplexity +
      breakdown.languageComplexity
    ));

    // Determine level
    let level: ComplexityLevel;
    if (overall <= 15) level = 'trivial';
    else if (overall <= 30) level = 'low';
    else if (overall <= 50) level = 'medium';
    else if (overall <= 70) level = 'high';
    else if (overall <= 85) level = 'very_high';
    else level = 'extreme';

    return {
      overall,
      level,
      breakdown,
      factors,
    };
  }

  private isCommentLine(line: string, patterns: RegExp[]): boolean {
    for (const pattern of patterns) {
      if (pattern.test(line)) {
        return true;
      }
    }
    return false;
  }

  private calculateNestingDepth(code: string, language: SourceLanguage): number {
    let maxDepth = 0;
    let currentDepth = 0;

    const lines = code.split('\n');

    // Language-specific nesting indicators
    const openPatterns: RegExp[] = [];
    const closePatterns: RegExp[] = [];

    switch (language) {
      case 'cobol':
        openPatterns.push(/\bIF\b/i, /\bEVALUATE\b/i, /\bPERFORM\b.*\bUNTIL\b/i);
        closePatterns.push(/\bEND-IF\b/i, /\bEND-EVALUATE\b/i, /\bEND-PERFORM\b/i);
        break;
      case 'fortran':
        openPatterns.push(/\bIF\s*\(.+\)\s*THEN/i, /\bDO\b/i);
        closePatterns.push(/\bEND\s*IF\b/i, /\bEND\s*DO\b/i);
        break;
      case 'vb6':
      case 'vba':
        openPatterns.push(/\bIf\b.*\bThen\b/i, /\bFor\b/i, /\bDo\b/i, /\bSelect\s+Case\b/i);
        closePatterns.push(/\bEnd\s+If\b/i, /\bNext\b/i, /\bLoop\b/i, /\bEnd\s+Select\b/i);
        break;
      case 'java-legacy':
        openPatterns.push(/\{/);
        closePatterns.push(/\}/);
        break;
    }

    for (const line of lines) {
      for (const pattern of openPatterns) {
        const matches = line.match(pattern);
        if (matches) {
          currentDepth += matches.length;
        }
      }
      for (const pattern of closePatterns) {
        const matches = line.match(pattern);
        if (matches) {
          currentDepth = Math.max(0, currentDepth - matches.length);
        }
      }
      maxDepth = Math.max(maxDepth, currentDepth);
    }

    return maxDepth;
  }

  private countUniqueOperations(code: string): number {
    // Count unique operators and function calls
    const operators = new Set<string>();

    // Arithmetic
    const opMatches = code.match(/[+\-*/%=<>!&|^~]+/g) || [];
    for (const op of opMatches) {
      operators.add(op);
    }

    // Function/procedure calls (simplified)
    const callMatches = code.match(/\b\w+\s*\(/g) || [];
    for (const call of callMatches) {
      operators.add(call.trim());
    }

    return operators.size;
  }
}
