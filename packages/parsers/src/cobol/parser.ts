/**
 * COBOL Parser
 * 
 * Parses COBOL source code into an AST representation
 * Supports COBOL-85, COBOL-2002, IBM Enterprise COBOL, and Micro Focus COBOL
 */

import type { ASTNode, DataStructure, Procedure, DataType } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParserOptions, type ParseError, type ParseWarning } from '../common/base-parser.js';

export interface CobolParserOptions extends ParserOptions {
  dialect?: 'cobol85' | 'cobol2002' | 'ibm-enterprise' | 'micro-focus' | 'gnucobol';
  sourceFormat?: 'fixed' | 'free';
  copybookPaths?: string[];
}

export interface CobolProgram {
  name: string;
  identificationDivision: IdentificationDivision;
  environmentDivision?: EnvironmentDivision;
  dataDivision?: DataDivision;
  procedureDivision?: ProcedureDivision;
}

export interface IdentificationDivision {
  programId: string;
  author?: string;
  installation?: string;
  dateWritten?: string;
  dateCompiled?: string;
  security?: string;
}

export interface EnvironmentDivision {
  configurationSection?: ConfigurationSection;
  inputOutputSection?: InputOutputSection;
}

export interface ConfigurationSection {
  sourceComputer?: string;
  objectComputer?: string;
  specialNames?: Record<string, string>;
}

export interface InputOutputSection {
  fileControl: FileControlEntry[];
}

export interface FileControlEntry {
  name: string;
  assignTo: string;
  organization?: 'sequential' | 'indexed' | 'relative';
  accessMode?: 'sequential' | 'random' | 'dynamic';
  recordKey?: string;
}

export interface DataDivision {
  fileSection: FileSectionEntry[];
  workingStorage: DataStructure[];
  localStorage?: DataStructure[];
  linkageSection?: DataStructure[];
}

export interface FileSectionEntry {
  fdName: string;
  label?: string;
  blockContains?: number;
  recordContains?: { min: number; max: number };
  records: DataStructure[];
}

export interface ProcedureDivision {
  using?: string[];
  returning?: string;
  sections: CobolSection[];
  paragraphs: CobolParagraph[];
}

export interface CobolSection {
  name: string;
  paragraphs: CobolParagraph[];
  startLine: number;
  endLine: number;
}

export interface CobolParagraph {
  name: string;
  statements: CobolStatement[];
  startLine: number;
  endLine: number;
}

export interface CobolStatement {
  type: string;
  startLine: number;
  endLine: number;
  raw: string;
  parsed?: Record<string, unknown>;
}

const COBOL_DIALECTS = ['cobol85', 'cobol2002', 'ibm-enterprise', 'microfocus', 'micro-focus', 'gnucobol'];

export class CobolParser extends BaseParser {
  private lines: string[] = [];
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: CobolParserOptions = {}) {
    super({
      ...options,
      dialect: options.dialect || 'cobol85',
    });
  }

  getSupportedDialects(): string[] {
    return COBOL_DIALECTS;
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.lines = source.split('\n');
    this.errors = [];
    this.warnings = [];

    try {
      // Preprocess: remove sequence numbers and handle continuations
      const preprocessed = this.preprocess(source);
      
      // Parse divisions
      const program = this.parseProgram(preprocessed, filename);
      
      // Build AST
      const ast = this.buildAST(program, filename);
      
      // Extract data structures
      const dataStructures = this.extractDataStructures(program);
      
      // Extract procedures
      const procedures = this.extractProcedures(program, filename);

      return {
        success: this.errors.filter(e => e.severity === 'fatal').length === 0,
        ast,
        dataStructures,
        procedures,
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'cobol',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: this.lines.length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    } catch (error) {
      this.errors.push(
        this.createError(
          error instanceof Error ? error.message : 'Unknown parse error',
          this.createLocation(filename, 1, this.lines.length),
          'PARSE_ERROR',
          'fatal'
        )
      );

      return {
        success: false,
        dataStructures: [],
        procedures: [],
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'cobol',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: this.lines.length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  /**
   * Preprocess COBOL source
   */
  private preprocess(source: string): string {
    const lines = source.split('\n');
    const processed: string[] = [];
    const isFixedFormat = this.options.dialect !== 'gnucobol' || 
      (this.options as CobolParserOptions).sourceFormat !== 'free';

    for (let i = 0; i < lines.length; i++) {
      let line = lines[i]!;
      
      if (isFixedFormat && line.length >= 7) {
        // Fixed format: columns 1-6 are sequence numbers, column 7 is indicator
        const indicator = line[6] || ' ';
        
        // Skip comment lines (indicator is * or /)
        if (indicator === '*' || indicator === '/') {
          continue;
        }
        
        // Handle continuation lines (indicator is -)
        if (indicator === '-' && processed.length > 0) {
          const prevLine = processed.pop()!;
          line = prevLine + line.slice(11).trimStart();
        } else {
          // Extract code from columns 8-72
          line = line.slice(7, 72).trimEnd();
        }
      }

      processed.push(line);
    }

    return processed.join('\n');
  }

  /**
   * Parse the COBOL program structure
   */
  private parseProgram(source: string, filename: string): CobolProgram {
    const upperSource = source.toUpperCase();
    
    // Extract divisions
    const idDivStart = upperSource.indexOf('IDENTIFICATION DIVISION');
    const envDivStart = upperSource.indexOf('ENVIRONMENT DIVISION');
    const dataDivStart = upperSource.indexOf('DATA DIVISION');
    const procDivStart = upperSource.indexOf('PROCEDURE DIVISION');

    // Validate: COBOL must have at least IDENTIFICATION DIVISION
    if (idDivStart < 0) {
      this.errors.push(
        this.createError(
          'Missing required IDENTIFICATION DIVISION',
          this.createLocation(filename, 1, 1),
          'MISSING_ID_DIVISION',
          'error'
        )
      );
    }

    // Validate: Check for PROGRAM-ID
    if (!upperSource.includes('PROGRAM-ID')) {
      this.errors.push(
        this.createError(
          'Missing required PROGRAM-ID in IDENTIFICATION DIVISION',
          this.createLocation(filename, 1, 1),
          'MISSING_PROGRAM_ID',
          'error'
        )
      );
    }

    // Parse identification division
    const identificationDivision = this.parseIdentificationDivision(
      source.slice(idDivStart, envDivStart > 0 ? envDivStart : dataDivStart > 0 ? dataDivStart : procDivStart)
    );

    // Parse environment division
    let environmentDivision: EnvironmentDivision | undefined;
    if (envDivStart > 0) {
      environmentDivision = this.parseEnvironmentDivision(
        source.slice(envDivStart, dataDivStart > 0 ? dataDivStart : procDivStart)
      );
    }

    // Parse data division
    let dataDivision: DataDivision | undefined;
    if (dataDivStart > 0) {
      dataDivision = this.parseDataDivision(
        source.slice(dataDivStart, procDivStart > 0 ? procDivStart : undefined)
      );
    }

    // Parse procedure division
    let procedureDivision: ProcedureDivision | undefined;
    if (procDivStart > 0) {
      procedureDivision = this.parseProcedureDivision(source.slice(procDivStart));
    }

    return {
      name: identificationDivision.programId,
      identificationDivision,
      environmentDivision,
      dataDivision,
      procedureDivision,
    };
  }

  /**
   * Parse the Identification Division
   */
  private parseIdentificationDivision(source: string): IdentificationDivision {
    const programIdMatch = source.match(/PROGRAM-ID\.\s*(\S+)/i);
    const authorMatch = source.match(/AUTHOR\.\s*(.+?)(?:\.|$)/im);
    
    return {
      programId: programIdMatch?.[1]?.replace(/\.$/, '') || 'UNKNOWN',
      author: authorMatch?.[1]?.trim(),
    };
  }

  /**
   * Parse the Environment Division
   */
  private parseEnvironmentDivision(source: string): EnvironmentDivision {
    const result: EnvironmentDivision = {};
    
    // Parse file control entries
    const fileControlMatch = source.match(/FILE-CONTROL\.([\s\S]*?)(?=INPUT-OUTPUT|DATA DIVISION|PROCEDURE DIVISION|$)/i);
    if (fileControlMatch) {
      result.inputOutputSection = {
        fileControl: this.parseFileControl(fileControlMatch[1]!),
      };
    }

    return result;
  }

  /**
   * Parse FILE-CONTROL entries
   */
  private parseFileControl(source: string): FileControlEntry[] {
    const entries: FileControlEntry[] = [];
    const selectStatements = source.matchAll(/SELECT\s+(\S+)\s+ASSIGN\s+TO\s+(\S+)/gi);
    
    for (const match of selectStatements) {
      entries.push({
        name: match[1]!,
        assignTo: match[2]!.replace(/['"]/g, ''),
      });
    }

    return entries;
  }

  /**
   * Parse the Data Division
   */
  private parseDataDivision(source: string): DataDivision {
    const result: DataDivision = {
      fileSection: [],
      workingStorage: [],
    };

    // Find WORKING-STORAGE SECTION
    const wsStart = source.toUpperCase().indexOf('WORKING-STORAGE SECTION');
    const linkageStart = source.toUpperCase().indexOf('LINKAGE SECTION');
    
    if (wsStart > 0) {
      const wsEnd = linkageStart > 0 ? linkageStart : source.length;
      const wsSource = source.slice(wsStart, wsEnd);
      result.workingStorage = this.parseDataItems(wsSource);
    }

    if (linkageStart > 0) {
      result.linkageSection = this.parseDataItems(source.slice(linkageStart));
    }

    return result;
  }

  /**
   * Parse data items from a section
   */
  private parseDataItems(source: string): DataStructure[] {
    const items: DataStructure[] = [];
    const lines = source.split('\n');
    const itemStack: { level: number; item: DataStructure }[] = [];

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      
      // Match level number and name with optional PIC clause
      // Format: LL NAME [PIC|PICTURE [IS]] pattern.
      const match = line.match(/^(\d{2})\s+([\w-]+)(?:\s+PIC(?:TURE)?(?:\s+IS)?\s+([^\s.]+))?/i);
      if (!match) continue;

      const level = parseInt(match[1]!, 10);
      const name = match[2]!;
      const picture = match[3]?.replace(/\.$/, ''); // Remove trailing period if present

      const item: DataStructure = {
        name,
        type: this.picToDataType(picture),
        level,
        picture,
        location: this.createLocation('', i + 1, i + 1),
      };

      // Handle hierarchy
      while (itemStack.length > 0 && itemStack[itemStack.length - 1]!.level >= level) {
        itemStack.pop();
      }

      if (itemStack.length > 0) {
        const parent = itemStack[itemStack.length - 1]!.item;
        if (!parent.children) {
          parent.children = [];
        }
        parent.children.push(item);
      } else {
        items.push(item);
      }

      if (level < 77) { // 77 level items don't have children
        itemStack.push({ level, item });
      }
    }

    return items;
  }

  /**
   * Convert PIC clause to DataType
   */
  private picToDataType(picture?: string): DataType {
    if (!picture) return 'group';
    
    const pic = picture.toUpperCase();
    
    // Decimal: contains V (implied decimal point)
    if (/V/.test(pic)) {
      return 'decimal';
    }
    // Integer: all 9s or 9(n) format, optionally signed
    if (/^S?9+$/.test(pic) || /^S?9\(\d+\)$/.test(pic)) {
      return 'integer';
    }
    // String: X or X(n) format
    if (/^X+$/.test(pic) || /^X\(\d+\)$/.test(pic)) {
      return 'string';
    }
    // Alphabetic: A or A(n) format
    if (/^A+$/.test(pic) || /^A\(\d+\)$/.test(pic)) {
      return 'string';
    }

    return 'unknown';
  }

  /**
   * Parse the Procedure Division
   */
  private parseProcedureDivision(source: string): ProcedureDivision {
    const lines = source.split('\n');
    const paragraphs: CobolParagraph[] = [];
    let currentParagraph: CobolParagraph | null = null;
    let currentStatements: CobolStatement[] = [];
    let orphanStatements: CobolStatement[] = []; // Statements before first paragraph

    // Reserved words that should not be treated as paragraph names
    const reservedWords = new Set([
      'END-IF', 'END-EVALUATE', 'END-PERFORM', 'END-READ', 'END-WRITE',
      'END-CALL', 'END-COMPUTE', 'END-STRING', 'END-UNSTRING', 'END-SEARCH',
      'ELSE', 'WHEN', 'OTHER', 'NOT', 'THEN', 'PROCEDURE'
    ]);

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      if (!line || line.startsWith('*')) continue;
      
      // Skip PROCEDURE DIVISION header
      if (/^PROCEDURE\s+DIVISION/i.test(line)) continue;

      // Check for paragraph name (word ending with period at start of line)
      // Exclude reserved words like END-IF, END-PERFORM, etc.
      const paragraphMatch = line.match(/^([\w-]+)\.\s*$/);
      if (paragraphMatch && !reservedWords.has(paragraphMatch[1]!.toUpperCase())) {
        if (currentParagraph) {
          currentParagraph.statements = currentStatements;
          currentParagraph.endLine = i;
          paragraphs.push(currentParagraph);
        }
        
        currentParagraph = {
          name: paragraphMatch[1]!,
          statements: [],
          startLine: i + 1,
          endLine: i + 1,
        };
        currentStatements = [];
        continue;
      }

      // Parse statement
      const statement = this.parseStatement(line, i + 1);
      if (statement) {
        if (currentParagraph) {
          currentStatements.push(statement);
        } else {
          orphanStatements.push(statement);
        }
      }
    }

    // Don't forget the last paragraph
    if (currentParagraph) {
      currentParagraph.statements = currentStatements;
      currentParagraph.endLine = lines.length;
      paragraphs.push(currentParagraph);
    }

    // If there are orphan statements (no named paragraphs), create a synthetic MAIN paragraph
    if (orphanStatements.length > 0 && paragraphs.length === 0) {
      paragraphs.push({
        name: 'MAIN',
        statements: orphanStatements,
        startLine: 1,
        endLine: lines.length,
      });
    }

    return {
      sections: [],
      paragraphs,
    };
  }

  /**
   * Parse a single statement
   */
  private parseStatement(line: string, lineNumber: number): CobolStatement | null {
    const verbs = ['MOVE', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      'IF', 'EVALUATE', 'PERFORM', 'CALL', 'READ', 'WRITE', 'OPEN', 'CLOSE',
      'DISPLAY', 'ACCEPT', 'STOP', 'GO', 'EXIT', 'STRING', 'UNSTRING', 'INSPECT'];

    for (const verb of verbs) {
      if (line.toUpperCase().startsWith(verb)) {
        return {
          type: verb,
          startLine: lineNumber,
          endLine: lineNumber,
          raw: line,
        };
      }
    }

    return null;
  }

  /**
   * Build AST from parsed program
   */
  private buildAST(program: CobolProgram, filename: string): ASTNode {
    return {
      type: 'Program',
      name: program.name,
      location: this.createLocation(filename, 1, this.lines.length),
      children: [
        {
          type: 'IdentificationDivision',
          location: this.createLocation(filename, 1, 10),
          children: [],
          metadata: program.identificationDivision as unknown as Record<string, unknown>,
        },
        ...(program.dataDivision ? [{
          type: 'DataDivision',
          location: this.createLocation(filename, 11, 100),
          children: [],
          metadata: { itemCount: program.dataDivision.workingStorage.length },
        }] : []),
        ...(program.procedureDivision ? [{
          type: 'ProcedureDivision',
          location: this.createLocation(filename, 101, this.lines.length),
          children: program.procedureDivision.paragraphs.map(p => ({
            type: 'Paragraph',
            name: p.name,
            location: this.createLocation(filename, p.startLine, p.endLine),
            children: [],
          })),
        }] : []),
      ],
    };
  }

  /**
   * Extract data structures from program
   */
  private extractDataStructures(program: CobolProgram): DataStructure[] {
    return program.dataDivision?.workingStorage || [];
  }

  /**
   * Extract procedures from program
   */
  private extractProcedures(program: CobolProgram, filename: string): Procedure[] {
    if (!program.procedureDivision) return [];

    return program.procedureDivision.paragraphs.map(p => ({
      name: p.name,
      type: 'paragraph' as const,
      parameters: [],
      localVariables: [],
      calledProcedures: p.statements
        .filter(s => s.type === 'PERFORM' || s.type === 'CALL')
        .map(s => s.raw.split(/\s+/)[1] || '')
        .filter(Boolean),
      location: this.createLocation(filename, p.startLine, p.endLine),
      complexity: this.calculateComplexity(p.statements),
    }));
  }

  /**
   * Calculate cyclomatic complexity
   */
  private calculateComplexity(statements: CobolStatement[]): number {
    let complexity = 1;
    
    for (const stmt of statements) {
      if (['IF', 'EVALUATE', 'PERFORM'].includes(stmt.type)) {
        complexity++;
      }
    }

    return complexity;
  }
}
