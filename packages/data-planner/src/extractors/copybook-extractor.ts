/**
 * COBOL COPYBOOK Schema Extractor
 * 
 * Parses COBOL COPYBOOK definitions to extract record structures
 * for data migration planning.
 */

import type {
  CopybookDefinition,
  CopybookField,
  RedefineGroup,
} from '../types.js';

export interface COPYBOOKExtractorConfig {
  dialect?: 'ibm' | 'microfocus' | 'gnucobol';
  normalizeNames?: boolean;
}

interface ParseState {
  currentPosition: number;
  fields: CopybookField[];
  redefines: RedefineGroup[];
  groupStack: { level: number; name: string; startPos: number }[];
}

export class COPYBOOKSchemaExtractor {
  private config: COPYBOOKExtractorConfig;

  constructor(config: COPYBOOKExtractorConfig = {}) {
    this.config = {
      dialect: 'ibm',
      normalizeNames: true,
      ...config,
    };
  }

  /**
   * Extract schema from COPYBOOK content
   */
  extract(content: string, name?: string): CopybookDefinition {
    const lines = this.preprocessCopybook(content);
    const state: ParseState = {
      currentPosition: 1,
      fields: [],
      redefines: [],
      groupStack: [],
    };

    for (const line of lines) {
      this.parseLine(line, state);
    }

    // Calculate record length
    const recordLength = state.fields.reduce((max, field) => {
      return Math.max(max, field.startPosition + field.length - 1);
    }, 0);

    return {
      name: name || this.extractCopybookName(content) || 'UNKNOWN',
      content,
      fields: state.fields,
      recordLength,
      redefines: state.redefines,
    };
  }

  private preprocessCopybook(content: string): string[] {
    return content
      .split('\n')
      .map(line => {
        // Handle fixed-format COBOL (columns 7-72)
        if (line.length > 6 && /^\d{6}/.test(line)) {
          line = line.substring(6);
        }
        // Skip comment lines
        if (line.startsWith('*') || line.trim().startsWith('*')) {
          return '';
        }
        return line.trim();
      })
      .filter(line => line.length > 0)
      .join(' ')
      .split('.')
      .map(s => s.trim())
      .filter(s => s.length > 0);
  }

  private parseLine(line: string, state: ParseState): void {
    const levelMatch = line.match(/^(\d{2})\s+(\S+)/);
    if (!levelMatch) return;

    const level = parseInt(levelMatch[1] || '0', 10);
    const name = (levelMatch[2] || '').toUpperCase();

    // Skip 88-level conditions for field extraction (but include for reference)
    if (level === 88) {
      state.fields.push({
        level: 88,
        name,
        startPosition: state.currentPosition,
        length: 0,
        mappedType: 'string',
        value: this.extractValue(line),
      });
      return;
    }

    // Pop groups from stack that are at same or higher level
    while (state.groupStack.length > 0) {
      const top = state.groupStack[state.groupStack.length - 1];
      if (top && top.level >= level) {
        state.groupStack.pop();
      } else {
        break;
      }
    }

    const field = this.parseField(line, level, name, state);
    state.fields.push(field);

    // Handle REDEFINES
    const redefinesMatch = line.match(/REDEFINES\s+(\S+)/i);
    if (redefinesMatch) {
      field.redefines = (redefinesMatch[1] || '').toUpperCase();
      // Find the redefined field to get position
      const redefinedField = state.fields.find(f => f.name === field.redefines);
      if (redefinedField) {
        field.startPosition = redefinedField.startPosition;
      }
    } else if (field.mappedType !== 'group') {
      // Advance position for non-redefines, non-group fields
      state.currentPosition += field.length;
    }

    // Track groups for nested structures
    if (field.mappedType === 'group') {
      state.groupStack.push({ 
        level, 
        name, 
        startPos: field.startPosition 
      });
    }
  }

  private parseField(
    line: string, 
    level: number, 
    name: string, 
    state: ParseState
  ): CopybookField {
    const picture = this.extractPicture(line);
    const usage = this.extractUsage(line);
    const occurs = this.extractOccurs(line);
    const value = this.extractValue(line);

    const { mappedType, length } = this.analyzePicture(picture, usage);

    // Calculate actual length for arrays
    const totalLength = occurs ? length * occurs.max : length;

    const field: CopybookField = {
      level,
      name: this.config.normalizeNames ? this.normalizeName(name) : name,
      picture,
      usage,
      occurs,
      value,
      startPosition: state.currentPosition,
      length: totalLength,
      mappedType,
    };

    return field;
  }

  private extractPicture(line: string): string | undefined {
    const picMatch = line.match(/PIC(?:TURE)?\s+IS\s+(\S+)/i) ||
                    line.match(/PIC(?:TURE)?\s+(\S+)/i);
    return picMatch && picMatch[1] ? picMatch[1].toUpperCase() : undefined;
  }

  private extractUsage(line: string): string | undefined {
    const usagePatterns = [
      /USAGE\s+IS\s+(\S+)/i,
      /USAGE\s+(\S+)/i,
      /\b(COMP(?:-[0-9])?|COMPUTATIONAL(?:-[0-9])?|BINARY|PACKED-DECIMAL|DISPLAY)\b/i,
    ];

    for (const pattern of usagePatterns) {
      const match = line.match(pattern);
      if (match && match[1]) return match[1].toUpperCase();
    }
    return undefined;
  }

  private extractOccurs(line: string): { min: number; max: number; dependingOn?: string } | undefined {
    const occursMatch = line.match(/OCCURS\s+(\d+)(?:\s+TO\s+(\d+))?\s*(?:TIMES)?(?:\s+DEPENDING\s+ON\s+(\S+))?/i);
    if (!occursMatch) return undefined;

    const minVal = occursMatch[1] || '1';
    const maxVal = occursMatch[2] || minVal;
    return {
      min: parseInt(minVal, 10),
      max: parseInt(maxVal, 10),
      dependingOn: occursMatch[3],
    };
  }

  private extractValue(line: string): string | undefined {
    const valueMatch = line.match(/VALUE\s+(?:IS\s+)?(?:'([^']*)'|"([^"]*)"|(\S+))/i);
    return valueMatch ? (valueMatch[1] || valueMatch[2] || valueMatch[3]) : undefined;
  }

  private analyzePicture(picture?: string, usage?: string): { mappedType: CopybookField['mappedType']; length: number } {
    if (!picture) {
      return { mappedType: 'group', length: 0 };
    }

    const expandedPic = this.expandPicture(picture);
    
    // Determine type based on picture and usage
    if (/^[9S]+V[9]+$/.test(expandedPic) || /^[9S]+V9+$/.test(expandedPic)) {
      // Decimal number
      const intPart = (expandedPic.match(/[9S]+(?=V)/)?.[0] || '').length;
      const decPart = (expandedPic.match(/V([9]+)/)?.[1] || '').length;
      const length = this.calculateNumericLength(intPart + decPart, usage);
      return { mappedType: 'decimal', length };
    }
    
    if (/^[9S]+$/.test(expandedPic)) {
      // Integer
      const digits = expandedPic.length;
      const length = this.calculateNumericLength(digits, usage);
      return { mappedType: 'integer', length };
    }

    if (/^[AX]+$/.test(expandedPic)) {
      // String
      return { mappedType: 'string', length: expandedPic.length };
    }

    // Default to string
    return { mappedType: 'string', length: expandedPic.length };
  }

  private expandPicture(picture: string): string {
    // Expand repeated characters: 9(5) -> 99999
    let expanded = picture;
    let match;
    
    while ((match = expanded.match(/([9AXSV])\((\d+)\)/)) !== null) {
      const char = match[1] || '';
      const countStr = match[2] || '1';
      const count = parseInt(countStr, 10);
      expanded = expanded.replace(match[0], char.repeat(count));
    }
    
    return expanded;
  }

  private calculateNumericLength(digits: number, usage?: string): number {
    if (!usage) {
      return digits; // DISPLAY (one byte per digit)
    }

    const upperUsage = usage.toUpperCase();
    
    if (upperUsage.includes('COMP-3') || upperUsage.includes('PACKED')) {
      // Packed decimal: (digits + 1) / 2
      return Math.ceil((digits + 1) / 2);
    }
    
    if (upperUsage.includes('COMP') || upperUsage.includes('BINARY')) {
      // Binary: depends on digit count
      if (digits <= 4) return 2;
      if (digits <= 9) return 4;
      return 8;
    }

    return digits; // DISPLAY
  }

  private extractCopybookName(content: string): string | undefined {
    const match = content.match(/01\s+(\S+)/);
    return match && match[1] ? match[1].replace(/\.$/, '').toUpperCase() : undefined;
  }

  private normalizeName(name: string): string {
    return name.toLowerCase().replace(/-/g, '_');
  }

  /**
   * Convert multiple copybooks to a schema
   */
  extractMultiple(copybooks: Map<string, string>): Map<string, CopybookDefinition> {
    const result = new Map<string, CopybookDefinition>();
    
    for (const [name, content] of copybooks) {
      result.set(name, this.extract(content, name));
    }
    
    return result;
  }
}
