/**
 * Language Detector
 * Automatically detects legacy programming language from code samples
 */

import type { SourceLanguage } from '@migrationpilot/core';
import type { LanguageDetectionResult } from '../types/index.js';

interface LanguagePattern {
  language: SourceLanguage;
  patterns: RegExp[];
  keywords: string[];
  weight: number;
}

const LANGUAGE_PATTERNS: LanguagePattern[] = [
  {
    language: 'cobol',
    patterns: [
      /^\s{6,7}[*\/]/m,                          // COBOL comment column
      /IDENTIFICATION\s+DIVISION/i,
      /PROCEDURE\s+DIVISION/i,
      /DATA\s+DIVISION/i,
      /WORKING-STORAGE\s+SECTION/i,
      /\bPIC\s+[X9]+/i,
      /\bPERFORM\s+\w+/i,
      /\bMOVE\s+\w+\s+TO\s+\w+/i,
      /\bCOPY\s+\w+/i,
      /\b01\s+\w+\./,
      /\bEVALUATE\s+/i,
      /\bSTOP\s+RUN/i,
    ],
    keywords: [
      'IDENTIFICATION', 'DIVISION', 'SECTION', 'PERFORM', 'MOVE', 'COMPUTE',
      'IF', 'ELSE', 'END-IF', 'EVALUATE', 'WHEN', 'DISPLAY', 'ACCEPT',
      'OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE', 'DELETE', 'START',
      'COPY', 'REPLACING', 'PIC', 'PICTURE', 'COMP', 'COMP-3', 'VALUE',
    ],
    weight: 1.0,
  },
  {
    language: 'fortran',
    patterns: [
      /^\s*PROGRAM\s+\w+/im,
      /^\s*SUBROUTINE\s+\w+/im,
      /^\s*FUNCTION\s+\w+/im,
      /^\s*INTEGER|REAL|DOUBLE\s+PRECISION|CHARACTER|LOGICAL/im,
      /^\s*DIMENSION\s+/im,
      /^\s*COMMON\s+\/\w+\//im,
      /^\s*DO\s+\d+\s+/im,
      /^\s*CONTINUE$/im,
      /^\s*FORMAT\s*\(/im,
      /^\s*WRITE\s*\(\s*\*?\s*,/im,
      /^\s*READ\s*\(\s*\*?\s*,/im,
      /^\s*END\s+(PROGRAM|SUBROUTINE|FUNCTION)/im,
    ],
    keywords: [
      'PROGRAM', 'SUBROUTINE', 'FUNCTION', 'END', 'INTEGER', 'REAL',
      'DOUBLE', 'PRECISION', 'CHARACTER', 'LOGICAL', 'COMPLEX',
      'DIMENSION', 'COMMON', 'EQUIVALENCE', 'DATA', 'PARAMETER',
      'DO', 'CONTINUE', 'IF', 'THEN', 'ELSE', 'ENDIF', 'GOTO',
      'CALL', 'RETURN', 'WRITE', 'READ', 'FORMAT', 'OPEN', 'CLOSE',
    ],
    weight: 1.0,
  },
  {
    language: 'vb6',
    patterns: [
      /^\s*Option\s+Explicit/im,
      /^\s*Private\s+Sub\s+\w+/im,
      /^\s*Public\s+Sub\s+\w+/im,
      /^\s*Private\s+Function\s+\w+/im,
      /^\s*Public\s+Function\s+\w+/im,
      /^\s*Dim\s+\w+\s+As\s+/im,
      /^\s*Set\s+\w+\s*=\s*New\s+/im,
      /^\s*On\s+Error\s+(GoTo|Resume)/im,
      /^\s*End\s+Sub$/im,
      /^\s*End\s+Function$/im,
      /\bMsgBox\s*\(/i,
      /\bDoEvents\b/i,
    ],
    keywords: [
      'Option', 'Explicit', 'Private', 'Public', 'Sub', 'Function', 'End',
      'Dim', 'As', 'Integer', 'Long', 'String', 'Boolean', 'Variant',
      'Set', 'New', 'Nothing', 'If', 'Then', 'Else', 'ElseIf', 'End If',
      'For', 'Next', 'Do', 'Loop', 'While', 'Wend', 'Select', 'Case',
      'On', 'Error', 'GoTo', 'Resume', 'Exit', 'MsgBox', 'InputBox',
    ],
    weight: 1.0,
  },
  {
    language: 'vba',
    patterns: [
      /^\s*Option\s+Explicit/im,
      /^\s*Sub\s+\w+\(\)/im,
      /^\s*Function\s+\w+\(/im,
      /\bWorksheet[s]?\b/i,
      /\bRange\s*\(\s*"/i,
      /\bCells\s*\(/i,
      /\bActiveSheet\b/i,
      /\bActiveWorkbook\b/i,
      /\bApplication\.Workbooks/i,
      /\bThisWorkbook\b/i,
      /\.Value\b/,
    ],
    keywords: [
      'Worksheet', 'Workbook', 'Range', 'Cells', 'ActiveSheet', 'ActiveWorkbook',
      'ThisWorkbook', 'Application', 'Selection', 'Sheets', 'Value',
    ],
    weight: 1.2, // Higher weight when Excel-specific patterns found
  },
  {
    language: 'java-legacy',
    patterns: [
      /^\s*import\s+javax?\.ejb\./im,
      /^\s*import\s+javax\.servlet\./im,
      /^\s*import\s+org\.apache\.struts\./im,
      /\b@Stateless\b/,
      /\b@Stateful\b/,
      /\b@Entity\b/,
      /\bEntityBean\b/,
      /\bSessionBean\b/,
      /\bHttpServlet\b/,
      /\bActionForm\b/,
      /\bAction\s+extends\s+/,
      /\bejbCreate\b/,
      /\bejbRemove\b/,
    ],
    keywords: [
      'ejb', 'EntityBean', 'SessionBean', 'EJBObject', 'EJBHome',
      'ActionForm', 'ActionForward', 'ActionMapping', 'DispatchAction',
      'HttpServlet', 'doGet', 'doPost', 'ejbCreate', 'ejbRemove',
      'ejbActivate', 'ejbPassivate', 'setSessionContext', 'setEntityContext',
    ],
    weight: 1.0,
  },
];

export class LanguageDetector {
  /**
   * Detect the programming language of a code sample
   */
  detect(code: string): LanguageDetectionResult {
    const scores = new Map<SourceLanguage, { score: number; indicators: string[] }>();

    // Initialize scores
    for (const pattern of LANGUAGE_PATTERNS) {
      scores.set(pattern.language, { score: 0, indicators: [] });
    }

    // Check patterns
    for (const langPattern of LANGUAGE_PATTERNS) {
      const entry = scores.get(langPattern.language)!;

      for (const pattern of langPattern.patterns) {
        if (pattern.test(code)) {
          entry.score += 10 * langPattern.weight;
          entry.indicators.push(`Pattern: ${pattern.source.substring(0, 30)}...`);
        }
      }

      // Check keywords (case-insensitive)
      const upperCode = code.toUpperCase();
      for (const keyword of langPattern.keywords) {
        const regex = new RegExp(`\\b${keyword}\\b`, 'i');
        if (regex.test(upperCode)) {
          entry.score += 2 * langPattern.weight;
          if (entry.indicators.length < 10) {
            entry.indicators.push(`Keyword: ${keyword}`);
          }
        }
      }
    }

    // Find the best match
    let bestLanguage: SourceLanguage = 'cobol';
    let bestScore = 0;
    let totalScore = 0;

    for (const [language, entry] of scores) {
      totalScore += entry.score;
      if (entry.score > bestScore) {
        bestScore = entry.score;
        bestLanguage = language;
      }
    }

    // Calculate confidence (0-1)
    const confidence = totalScore > 0
      ? Math.min(0.99, bestScore / Math.max(totalScore, 50))
      : 0;

    const bestEntry = scores.get(bestLanguage)!;

    return {
      language: bestLanguage,
      confidence,
      indicators: bestEntry.indicators.slice(0, 5),
    };
  }

  /**
   * Validate if the detected language matches expected patterns
   */
  validate(code: string, expectedLanguage: SourceLanguage): boolean {
    const result = this.detect(code);
    return result.language === expectedLanguage && result.confidence > 0.5;
  }
}
