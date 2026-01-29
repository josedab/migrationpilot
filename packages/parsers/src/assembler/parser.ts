/**
 * IBM Mainframe Assembler Parser
 * 
 * Parser for IBM High Level Assembler (HLASM) used on z/OS mainframes.
 * Supports basic assembler instructions, macros, and system macros.
 */

import type { ASTNode, DataStructure, Procedure } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParseError, type ParseWarning, type ParserOptions } from '../common/base-parser';

export interface AssemblerParserOptions extends ParserOptions {
  dialect?: 'hlasm' | 'balam' | 'assist';
}

interface AssemblerLine {
  lineNum: number;
  label: string;
  operation: string;
  operands: string;
  comment: string;
  isContinuation: boolean;
}

export class AssemblerParser extends BaseParser {
  private lines: AssemblerLine[] = [];
  private filename = '';
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: AssemblerParserOptions = {}) {
    super({ ...options, dialect: options.dialect || 'hlasm' });
  }

  getSupportedDialects(): string[] {
    return ['hlasm', 'balam', 'assist'];
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.filename = filename;
    this.errors = [];
    this.warnings = [];

    try {
      this.lines = this.parseLines(source);
      const ast = this.buildAST();
      const dataStructures = this.extractDataStructures(ast);
      const procedures = this.extractProcedures(ast);

      return {
        success: this.errors.filter(e => e.severity === 'fatal').length === 0,
        ast,
        dataStructures,
        procedures,
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'assembler',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: source.split('\n').length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown parse error';
      this.errors.push(this.createError(
        message,
        this.createLocation(filename, 1, 1),
        'ASM001',
        'fatal'
      ));

      return {
        success: false,
        dataStructures: [],
        procedures: [],
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'assembler',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: source.split('\n').length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private parseLines(source: string): AssemblerLine[] {
    const rawLines = source.split('\n');
    const parsed: AssemblerLine[] = [];

    for (let i = 0; i < rawLines.length; i++) {
      const line = rawLines[i];
      
      // Skip empty lines
      if (!line!.trim()) continue;

      // Full line comment (starts with * in column 1)
      if (line![0] === '*') {
        parsed.push({
          lineNum: i + 1,
          label: '',
          operation: '*',
          operands: '',
          comment: line!.substring(1).trim(),
          isContinuation: false,
        });
        continue;
      }

      // Parse fixed-format assembler line
      // Columns: 1-8 label, 10-14 operation, 16-71 operands, 72 continuation, 73-80 sequence
      const parsedLine = this.parseAssemblerLine(line!, i + 1);
      if (parsedLine) {
        parsed.push(parsedLine);
      }
    }

    return parsed;
  }

  private parseAssemblerLine(line: string, lineNum: number): AssemblerLine | null {
    // Pad line to 72 characters for consistent parsing
    const padded = line.padEnd(72);
    
    let label = '';
    let operation = '';
    let operands = '';
    let comment = '';
    let isContinuation = false;

    // Check continuation character in column 72
    if (padded.length >= 72 && padded[71] !== ' ') {
      isContinuation = true;
    }

    // Label (columns 1-8, must start in column 1)
    if (padded[0] !== ' ') {
      let endLabel = 0;
      while (endLabel < 8 && padded[endLabel] !== ' ') {
        endLabel++;
      }
      label = padded.substring(0, endLabel).trim();
    }

    // Find operation (starts after column 9, first non-blank)
    let opStart = 9;
    while (opStart < padded.length && padded[opStart] === ' ') {
      opStart++;
    }
    
    if (opStart < padded.length) {
      let opEnd = opStart;
      while (opEnd < padded.length && padded[opEnd] !== ' ') {
        opEnd++;
      }
      operation = padded.substring(opStart, opEnd).toUpperCase();

      // Find operands (after operation)
      let operandStart = opEnd;
      while (operandStart < padded.length && padded[operandStart] === ' ') {
        operandStart++;
      }
      
      if (operandStart < 71) {
        // Find end of operands (before comment)
        let inQuote = false;
        let operandEnd = operandStart;
        for (let i = operandStart; i < 71; i++) {
          if (padded[i] === "'") {
            inQuote = !inQuote;
          }
          if (!inQuote && padded[i] === ' ') {
            // Check if this is start of comment
            let j = i;
            while (j < 71 && padded[j] === ' ') j++;
            if (j < 71) {
              operandEnd = i;
              comment = padded.substring(j, 71).trim();
              break;
            }
          }
          operandEnd = i + 1;
        }
        operands = padded.substring(operandStart, operandEnd).trim();
      }
    }

    return {
      lineNum,
      label,
      operation,
      operands,
      comment,
      isContinuation,
    };
  }

  private buildAST(): ASTNode {
    const children: ASTNode[] = [];
    let currentCSECT: ASTNode | null = null;
    let currentDSECT: ASTNode | null = null;

    const controlOps = new Set([
      'CSECT', 'DSECT', 'START', 'END', 'ENTRY', 'EXTRN', 'WXTRN',
      'USING', 'DROP', 'PUSH', 'POP', 'PRINT', 'TITLE', 'EJECT', 'SPACE',
      'COPY', 'MACRO', 'MEND', 'MEXIT', 'AIF', 'AGO', 'ANOP', 'ACTR',
      'GBLA', 'GBLB', 'GBLC', 'LCLA', 'LCLB', 'LCLC', 'SETA', 'SETB', 'SETC',
    ]);

    const dataOps = new Set([
      'DC', 'DS', 'EQU', 'ORG', 'LTORG', 'CNOP',
    ]);

    const branchOps = new Set([
      'B', 'BR', 'BE', 'BNE', 'BH', 'BL', 'BNH', 'BNL', 'BO', 'BNO', 'BM', 'BP', 'BZ', 'BNZ',
      'BC', 'BCR', 'BCT', 'BCTR', 'BXH', 'BXLE', 'BAL', 'BALR', 'BAS', 'BASR',
    ]);

    for (let i = 0; i < this.lines.length; i++) {
      const line = this.lines[i];
      
      // Skip comments
      if (line!.operation === '*') {
        children.push({
          type: 'Comment',
          name: '*',
          location: this.createLocation(this.filename, line!.lineNum, line!.lineNum),
          children: [],
          metadata: { text: line!.comment },
        });
        continue;
      }

      // Handle CSECT
      if (line!.operation === 'CSECT') {
        if (currentCSECT) {
          currentCSECT.location = this.createLocation(
            this.filename,
            currentCSECT.location.startLine,
            line!.lineNum - 1
          );
          children.push(currentCSECT);
        }
        currentCSECT = {
          type: 'ControlSection',
          name: line!.label || 'UNNAMED',
          location: this.createLocation(this.filename, line!.lineNum, line!.lineNum),
          children: [],
          metadata: {},
        };
        continue;
      }

      // Handle DSECT
      if (line!.operation === 'DSECT') {
        if (currentDSECT) {
          currentDSECT.location = this.createLocation(
            this.filename,
            currentDSECT.location.startLine,
            line!.lineNum - 1
          );
          children.push(currentDSECT);
        }
        currentDSECT = {
          type: 'DummySection',
          name: line!.label || 'UNNAMED',
          location: this.createLocation(this.filename, line!.lineNum, line!.lineNum),
          children: [],
          metadata: {},
        };
        continue;
      }

      // Handle END
      if (line!.operation === 'END') {
        if (currentCSECT) {
          currentCSECT.location = this.createLocation(
            this.filename,
            currentCSECT.location.startLine,
            line!.lineNum
          );
          children.push(currentCSECT);
          currentCSECT = null;
        }
        if (currentDSECT) {
          currentDSECT.location = this.createLocation(
            this.filename,
            currentDSECT.location.startLine,
            line!.lineNum
          );
          children.push(currentDSECT);
          currentDSECT = null;
        }
        children.push({
          type: 'EndDirective',
          name: 'END',
          location: this.createLocation(this.filename, line!.lineNum, line!.lineNum),
          children: [],
          metadata: { entryPoint: line!.operands },
        });
        continue;
      }

      // Create instruction node
      const node: ASTNode = {
        type: controlOps.has(line!.operation) ? 'Directive' :
              dataOps.has(line!.operation) ? 'DataDefinition' :
              branchOps.has(line!.operation) ? 'Branch' :
              'Instruction',
        name: line!.label || line!.operation,
        location: this.createLocation(this.filename, line!.lineNum, line!.lineNum),
        children: [],
        metadata: {
          operation: line!.operation,
          operands: line!.operands,
          comment: line!.comment,
          parsedOperands: this.parseOperands(line!.operands),
        },
      };

      // Add to current section or top level
      if (currentDSECT) {
        currentDSECT.children.push(node);
      } else if (currentCSECT) {
        currentCSECT.children.push(node);
      } else {
        children.push(node);
      }
    }

    // Close any open sections
    if (currentCSECT) {
      children.push(currentCSECT);
    }
    if (currentDSECT) {
      children.push(currentDSECT);
    }

    return {
      type: 'Program',
      name: 'ASSEMBLER_PROGRAM',
      location: this.createLocation(
        this.filename,
        1,
        this.lines.length > 0 ? this.lines[this.lines.length - 1]!.lineNum : 1
      ),
      children,
      metadata: {},
    };
  }

  private parseOperands(operands: string): string[] {
    if (!operands) return [];
    
    const result: string[] = [];
    let current = '';
    let parenDepth = 0;
    let inQuote = false;

    for (const char of operands) {
      if (char === "'" && !inQuote) {
        inQuote = true;
        current += char;
      } else if (char === "'" && inQuote) {
        inQuote = false;
        current += char;
      } else if (char === '(' && !inQuote) {
        parenDepth++;
        current += char;
      } else if (char === ')' && !inQuote) {
        parenDepth--;
        current += char;
      } else if (char === ',' && !inQuote && parenDepth === 0) {
        result.push(current.trim());
        current = '';
      } else {
        current += char;
      }
    }
    
    if (current.trim()) {
      result.push(current.trim());
    }

    return result;
  }

  private extractDataStructures(ast: ASTNode): DataStructure[] {
    const structures: DataStructure[] = [];
    
    const visit = (node: ASTNode) => {
      if (node.type === 'DummySection' && node.name) {
        structures.push({
          name: node.name,
          type: 'group',
          location: node.location,
        });
      }
      node.children?.forEach(visit);
    };
    
    visit(ast);
    return structures;
  }

  private extractProcedures(ast: ASTNode): Procedure[] {
    const procedures: Procedure[] = [];
    
    const visit = (node: ASTNode) => {
      if (node.type === 'ControlSection' && node.name) {
        procedures.push({
          name: node.name,
          type: 'section',
          parameters: [],
          localVariables: [],
          calledProcedures: this.findCalledProcedures(node),
          complexity: this.calculateComplexity(node),
          location: node.location,
        });
      }
      node.children?.forEach(visit);
    };
    
    visit(ast);
    return procedures;
  }

  private findCalledProcedures(node: ASTNode): string[] {
    const calls: string[] = [];
    
    const visit = (n: ASTNode) => {
      if (n.type === 'Instruction') {
        const op = (n.metadata?.operation as string)?.toUpperCase();
        if (['CALL', 'BAL', 'BALR', 'BAS', 'BASR'].includes(op)) {
          const operands = n.metadata?.parsedOperands as string[];
          if (operands && operands.length > 0) {
            // Extract target from operands
            const target = operands[operands.length - 1]?.split('(')[0];
            if (target && !target.match(/^\d/)) {
              calls.push(target);
            }
          }
        }
      }
      n.children?.forEach(visit);
    };
    
    visit(node);
    return [...new Set(calls)];
  }

  private calculateComplexity(node: ASTNode): number {
    let complexity = 1;
    
    const visit = (n: ASTNode) => {
      if (n.type === 'Branch') {
        complexity++;
      }
      if (n.type === 'Instruction') {
        const op = (n.metadata?.operation as string)?.toUpperCase();
        if (['BCT', 'BCTR', 'BXH', 'BXLE'].includes(op)) {
          complexity++; // Loop constructs
        }
      }
      n.children?.forEach(visit);
    };
    
    visit(node);
    return complexity;
  }
}

export { AssemblerParser as default };
