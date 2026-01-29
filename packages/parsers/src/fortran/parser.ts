/**
 * Fortran Parser
 * 
 * Parses Fortran source code (F77, F90, F95)
 */

import type { ASTNode, DataStructure, Procedure, DataType } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParserOptions, type ParseError, type ParseWarning } from '../common/base-parser.js';

export interface FortranParserOptions extends ParserOptions {
  dialect?: 'f77' | 'f90' | 'f95';
  freeFormat?: boolean;
}

export interface FortranProgram {
  name: string;
  units: ProgramUnit[];
}

export interface ProgramUnit {
  type: 'program' | 'subroutine' | 'function' | 'module' | 'block_data';
  name: string;
  parameters?: string[];
  returnType?: string;
  declarations: FortranDeclaration[];
  statements: FortranStatement[];
  startLine: number;
  endLine: number;
}

export interface FortranDeclaration {
  type: string;
  variables: string[];
  dimension?: string;
  intent?: 'in' | 'out' | 'inout';
  line: number;
}

export interface FortranStatement {
  type: string;
  line: number;
  raw: string;
  label?: string;
}

const FORTRAN_DIALECTS = ['f77', 'f90', 'f95'];

export class FortranParser extends BaseParser {
  private lines: string[] = [];
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: FortranParserOptions = {}) {
    super({
      ...options,
      dialect: options.dialect || 'f90',
    });
  }

  getSupportedDialects(): string[] {
    return FORTRAN_DIALECTS;
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.lines = source.split('\n');
    this.errors = [];
    this.warnings = [];

    try {
      const preprocessed = this.preprocess(source);
      const program = this.parseProgram(preprocessed, filename);
      const ast = this.buildAST(program, filename);
      const dataStructures = this.extractDataStructures(program);
      const procedures = this.extractProcedures(program, filename);

      return {
        success: this.errors.filter(e => e.severity === 'fatal').length === 0,
        ast,
        dataStructures,
        procedures,
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'fortran',
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
          language: 'fortran',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: this.lines.length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private preprocess(source: string): string {
    const lines = source.split('\n');
    const processed: string[] = [];
    const isF77 = this.options.dialect === 'f77';

    for (let i = 0; i < lines.length; i++) {
      let line = lines[i]!;
      
      // Handle F77 fixed format
      if (isF77 && line.length > 0) {
        const firstChar = line[0]!.toUpperCase();
        
        // Skip comment lines
        if (firstChar === 'C' || firstChar === '*' || firstChar === '!') {
          continue;
        }
        
        // Handle continuation lines (column 6 is non-blank)
        if (line.length >= 6 && line[5] !== ' ' && processed.length > 0) {
          const prevLine = processed.pop()!;
          line = prevLine + line.slice(6).trimStart();
        } else {
          line = line.slice(6);
        }
      } else {
        // F90+ free format: handle ! comments
        const commentIdx = line.indexOf('!');
        if (commentIdx >= 0) {
          line = line.slice(0, commentIdx);
        }
      }

      processed.push(line.trim());
    }

    return processed.join('\n');
  }

  private parseProgram(source: string, _filename: string): FortranProgram {
    const units: ProgramUnit[] = [];
    const lines = source.split('\n');
    let currentUnit: Partial<ProgramUnit> | null = null;
    let declarations: FortranDeclaration[] = [];
    let statements: FortranStatement[] = [];
    let unitStartLine = 0;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim().toUpperCase();
      if (!line) continue;

      // Check for program unit start
      const programMatch = line.match(/^PROGRAM\s+(\w+)/);
      const subroutineMatch = line.match(/^SUBROUTINE\s+(\w+)\s*\(([^)]*)\)?/);
      const functionMatch = line.match(/^(?:(\w+)\s+)?FUNCTION\s+(\w+)\s*\(([^)]*)\)?/);
      const moduleMatch = line.match(/^MODULE\s+(\w+)/);

      if (programMatch || subroutineMatch || functionMatch || moduleMatch) {
        // Save previous unit
        if (currentUnit) {
          currentUnit.declarations = declarations;
          currentUnit.statements = statements;
          currentUnit.endLine = i;
          units.push(currentUnit as ProgramUnit);
        }

        declarations = [];
        statements = [];
        unitStartLine = i + 1;

        if (programMatch) {
          currentUnit = { type: 'program', name: programMatch[1]!, startLine: unitStartLine };
        } else if (subroutineMatch) {
          currentUnit = {
            type: 'subroutine',
            name: subroutineMatch[1]!,
            parameters: subroutineMatch[2]?.split(',').map(p => p.trim()).filter(Boolean),
            startLine: unitStartLine,
          };
        } else if (functionMatch) {
          currentUnit = {
            type: 'function',
            name: functionMatch[2]!,
            returnType: functionMatch[1],
            parameters: functionMatch[3]?.split(',').map(p => p.trim()).filter(Boolean),
            startLine: unitStartLine,
          };
        } else if (moduleMatch) {
          currentUnit = { type: 'module', name: moduleMatch[1]!, startLine: unitStartLine };
        }
        continue;
      }

      // Check for end of unit
      if (line.startsWith('END')) {
        if (currentUnit) {
          currentUnit.declarations = declarations;
          currentUnit.statements = statements;
          currentUnit.endLine = i + 1;
          units.push(currentUnit as ProgramUnit);
          currentUnit = null;
          declarations = [];
          statements = [];
        }
        continue;
      }

      // Parse declarations
      const declMatch = line.match(/^(INTEGER|REAL|DOUBLE\s+PRECISION|COMPLEX|LOGICAL|CHARACTER)(?:\*\d+)?\s+(.+)/);
      if (declMatch) {
        declarations.push({
          type: declMatch[1]!.toLowerCase().replace(/\s+/g, '_'),
          variables: declMatch[2]!.split(',').map(v => v.trim().split('(')[0]!.trim()),
          line: i + 1,
        });
        continue;
      }

      // Parse COMMON blocks
      const commonMatch = line.match(/^COMMON\s*\/(\w*)\/\s*(.+)/);
      if (commonMatch) {
        declarations.push({
          type: 'common',
          variables: commonMatch[2]!.split(',').map(v => v.trim()),
          line: i + 1,
        });
        continue;
      }

      // Other statements
      if (currentUnit) {
        statements.push({
          type: this.getStatementType(line),
          line: i + 1,
          raw: lines[i]!.trim(),
        });
      }
    }

    // Handle case where file doesn't have explicit END
    if (currentUnit) {
      currentUnit.declarations = declarations;
      currentUnit.statements = statements;
      currentUnit.endLine = lines.length;
      units.push(currentUnit as ProgramUnit);
    }

    return {
      name: units[0]?.name || 'UNNAMED',
      units,
    };
  }

  private getStatementType(line: string): string {
    const keywords = ['IF', 'DO', 'CALL', 'RETURN', 'WRITE', 'READ', 'PRINT', 'FORMAT', 'GOTO', 'STOP'];
    
    for (const kw of keywords) {
      if (line.startsWith(kw)) return kw;
    }
    
    if (line.includes('=')) return 'ASSIGNMENT';
    return 'OTHER';
  }

  private buildAST(program: FortranProgram, filename: string): ASTNode {
    return {
      type: 'Program',
      name: program.name,
      location: this.createLocation(filename, 1, this.lines.length),
      children: program.units.map(unit => ({
        type: unit.type.charAt(0).toUpperCase() + unit.type.slice(1),
        name: unit.name,
        location: this.createLocation(filename, unit.startLine, unit.endLine),
        children: [],
        metadata: {
          parameters: unit.parameters,
          returnType: unit.returnType,
        },
      })),
    };
  }

  private extractDataStructures(program: FortranProgram): DataStructure[] {
    const structures: DataStructure[] = [];

    for (const unit of program.units) {
      for (const decl of unit.declarations) {
        for (const varName of decl.variables) {
          structures.push({
            name: varName,
            type: this.fortranTypeToDataType(decl.type),
            location: this.createLocation('', decl.line, decl.line),
          });
        }
      }
    }

    return structures;
  }

  private fortranTypeToDataType(type: string): DataType {
    const typeMap: Record<string, DataType> = {
      integer: 'integer',
      real: 'decimal',
      'double_precision': 'decimal',
      complex: 'unknown',
      logical: 'boolean',
      character: 'string',
    };

    return typeMap[type] || 'unknown';
  }

  private extractProcedures(program: FortranProgram, filename: string): Procedure[] {
    // Include program, subroutine, and function units as procedures
    return program.units
      .filter(u => u.type === 'program' || u.type === 'subroutine' || u.type === 'function')
      .map(unit => ({
        name: unit.name,
        type: unit.type === 'function' ? 'function' : 'method' as 'function' | 'method',
        parameters: (unit.parameters || []).map(p => ({
          name: p,
          type: 'unknown' as DataType,
          direction: 'inout' as const,
        })),
        returnType: unit.returnType ? this.fortranTypeToDataType(unit.returnType) : undefined,
        localVariables: unit.declarations
          .filter(d => d.type !== 'common')
          .flatMap(d => d.variables.map(v => ({
            name: v,
            type: this.fortranTypeToDataType(d.type),
            location: this.createLocation(filename, d.line, d.line),
          }))),
        calledProcedures: unit.statements
          .filter(s => s.type === 'CALL')
          .map(s => s.raw.match(/CALL\s+(\w+)/i)?.[1] || '')
          .filter(Boolean),
        location: this.createLocation(filename, unit.startLine, unit.endLine),
        complexity: this.calculateComplexity(unit.statements),
      }));
  }

  private calculateComplexity(statements: FortranStatement[]): number {
    let complexity = 1;
    
    for (const stmt of statements) {
      if (['IF', 'DO', 'CALL'].includes(stmt.type)) {
        complexity++;
      }
    }

    return complexity;
  }
}
