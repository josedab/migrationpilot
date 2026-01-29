/**
 * Visual Basic 6 Parser
 * 
 * Parses VB6 and VBA source code
 */

import type { ASTNode, DataStructure, Procedure, DataType } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParserOptions, type ParseError, type ParseWarning } from '../common/base-parser.js';

export interface VB6ParserOptions extends ParserOptions {
  dialect?: 'vb6' | 'vba-excel' | 'vba-access' | 'vba-word';
}

export interface VB6Module {
  type: 'module' | 'class' | 'form' | 'usercontrol';
  name: string;
  attributes: Record<string, string>;
  declarations: VB6Declaration[];
  procedures: VB6Procedure[];
}

export interface VB6Declaration {
  scope: 'public' | 'private' | 'dim';
  name: string;
  type: string;
  isArray: boolean;
  isConstant: boolean;
  value?: string;
  line: number;
}

export interface VB6Procedure {
  type: 'sub' | 'function' | 'property_get' | 'property_let' | 'property_set';
  scope: 'public' | 'private' | 'friend';
  name: string;
  parameters: VB6Parameter[];
  returnType?: string;
  statements: VB6Statement[];
  startLine: number;
  endLine: number;
}

export interface VB6Parameter {
  name: string;
  type: string;
  byRef: boolean;
  optional: boolean;
  defaultValue?: string;
}

export interface VB6Statement {
  type: string;
  line: number;
  raw: string;
}

const VB6_DIALECTS = ['vb6', 'vba', 'vba-excel', 'vba-access', 'vba-word'];

export class VB6Parser extends BaseParser {
  private lines: string[] = [];
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: VB6ParserOptions = {}) {
    super({
      ...options,
      dialect: options.dialect || 'vb6',
    });
  }

  getSupportedDialects(): string[] {
    return VB6_DIALECTS;
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.lines = source.split('\n');
    this.errors = [];
    this.warnings = [];

    try {
      const preprocessed = this.preprocess(source);
      const module = this.parseModule(preprocessed, filename);
      const ast = this.buildAST(module, filename);
      const dataStructures = this.extractDataStructures(module);
      const procedures = this.extractProcedures(module, filename);

      return {
        success: this.errors.filter(e => e.severity === 'fatal').length === 0,
        ast,
        dataStructures,
        procedures,
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'vb6',
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
          language: 'vb6',
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

    for (let i = 0; i < lines.length; i++) {
      let line = lines[i]!;
      
      // Handle line continuation with underscore
      while (line.trimEnd().endsWith('_') && i + 1 < lines.length) {
        line = line.trimEnd().slice(0, -1) + ' ' + lines[++i]!.trim();
      }

      // Remove comments
      const commentIdx = line.indexOf("'");
      if (commentIdx >= 0) {
        line = line.slice(0, commentIdx);
      }

      processed.push(line);
    }

    return processed.join('\n');
  }

  private parseModule(source: string, filename: string): VB6Module {
    const lines = source.split('\n');
    const module: VB6Module = {
      type: this.detectModuleType(source),
      name: this.extractModuleName(source, filename),
      attributes: {},
      declarations: [],
      procedures: [],
    };

    let currentProcedure: Partial<VB6Procedure> | null = null;
    let statements: VB6Statement[] = [];
    let procStartLine = 0;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      if (!line) continue;

      // Parse attributes
      const attrMatch = line.match(/^Attribute\s+(\w+)\s*=\s*(.+)/i);
      if (attrMatch) {
        module.attributes[attrMatch[1]!] = attrMatch[2]!.replace(/^"|"$/g, '');
        continue;
      }

      // Parse procedure start - must check BEFORE declarations
      const procMatch = line.match(/^(Public|Private|Friend)?\s*(Sub|Function|Property\s+Get|Property\s+Let|Property\s+Set)\s+(\w+)\s*\(([^)]*)\)(?:\s+As\s+(\w+))?/i);
      if (procMatch) {
        if (currentProcedure) {
          currentProcedure.statements = statements;
          currentProcedure.endLine = i;
          module.procedures.push(currentProcedure as VB6Procedure);
        }

        statements = [];
        procStartLine = i + 1;

        const procType = procMatch[2]!.toLowerCase().replace(/\s+/g, '_') as VB6Procedure['type'];
        
        currentProcedure = {
          type: procType,
          scope: (procMatch[1]?.toLowerCase() || 'public') as 'public' | 'private' | 'friend',
          name: procMatch[3]!,
          parameters: this.parseParameters(procMatch[4] || ''),
          returnType: procMatch[5],
          startLine: procStartLine,
        };
        continue;
      }

      // Parse module-level declarations (after procedure check to avoid matching "Public Sub" as declaration)
      const declMatch = line.match(/^(Public|Private|Dim|Const)\s+(?!Sub\b|Function\b|Property\b)(\w+)(?:\s+As\s+(\w+))?/i);
      if (declMatch && !currentProcedure) {
        module.declarations.push({
          scope: declMatch[1]!.toLowerCase() as 'public' | 'private' | 'dim',
          name: declMatch[2]!,
          type: declMatch[3] || 'Variant',
          isArray: line.includes('()'),
          isConstant: declMatch[1]!.toLowerCase() === 'const',
          line: i + 1,
        });
        continue;
      }

      // Parse procedure end
      if (/^End\s+(Sub|Function|Property)/i.test(line)) {
        if (currentProcedure) {
          currentProcedure.statements = statements;
          currentProcedure.endLine = i + 1;
          module.procedures.push(currentProcedure as VB6Procedure);
          currentProcedure = null;
          statements = [];
        }
        continue;
      }

      // Collect statements
      if (currentProcedure) {
        statements.push({
          type: this.getStatementType(line),
          line: i + 1,
          raw: lines[i]!.trim(),
        });
      }
    }

    return module;
  }

  private detectModuleType(source: string): VB6Module['type'] {
    if (/VERSION\s+[\d.]+\s+CLASS/i.test(source)) return 'class';
    if (/Begin\s+VB\.Form/i.test(source)) return 'form';
    if (/Begin\s+VB\.UserControl/i.test(source)) return 'usercontrol';
    return 'module';
  }

  private extractModuleName(source: string, filename: string): string {
    const nameMatch = source.match(/Attribute\s+VB_Name\s*=\s*"([^"]+)"/i);
    if (nameMatch) return nameMatch[1]!;
    
    return filename.replace(/\.(bas|cls|frm)$/i, '');
  }

  private parseParameters(paramStr: string): VB6Parameter[] {
    if (!paramStr.trim()) return [];

    return paramStr.split(',').map(p => {
      const trimmed = p.trim();
      const optional = /^Optional/i.test(trimmed);
      const byRef = !/^ByVal/i.test(trimmed);
      
      const match = trimmed.match(/(?:Optional\s+)?(?:ByVal\s+|ByRef\s+)?(\w+)(?:\s+As\s+(\w+))?(?:\s*=\s*(.+))?/i);
      
      return {
        name: match?.[1] || 'unknown',
        type: match?.[2] || 'Variant',
        byRef,
        optional,
        defaultValue: match?.[3],
      };
    });
  }

  private getStatementType(line: string): string {
    const upper = line.toUpperCase();
    const keywords = ['IF', 'FOR', 'DO', 'WHILE', 'SELECT', 'WITH', 'CALL', 'SET', 'LET', 'DIM', 'REDIM', 'EXIT', 'GOTO', 'ON ERROR'];
    
    for (const kw of keywords) {
      if (upper.startsWith(kw)) return kw;
    }
    
    if (line.includes('=') && !upper.startsWith('IF')) return 'ASSIGNMENT';
    return 'OTHER';
  }

  private buildAST(module: VB6Module, filename: string): ASTNode {
    return {
      type: module.type.charAt(0).toUpperCase() + module.type.slice(1),
      name: module.name,
      location: this.createLocation(filename, 1, this.lines.length),
      children: module.procedures.map(proc => ({
        type: proc.type.charAt(0).toUpperCase() + proc.type.slice(1).replace(/_/g, ''),
        name: proc.name,
        location: this.createLocation(filename, proc.startLine, proc.endLine),
        children: [],
        metadata: {
          scope: proc.scope,
          returnType: proc.returnType,
        },
      })),
      metadata: module.attributes,
    };
  }

  private extractDataStructures(module: VB6Module): DataStructure[] {
    return module.declarations.map(decl => ({
      name: decl.name,
      type: this.vbTypeToDataType(decl.type),
      location: this.createLocation('', decl.line, decl.line),
    }));
  }

  private vbTypeToDataType(type: string): DataType {
    const typeMap: Record<string, DataType> = {
      string: 'string',
      integer: 'integer',
      long: 'integer',
      single: 'decimal',
      double: 'decimal',
      currency: 'decimal',
      date: 'date',
      boolean: 'boolean',
      byte: 'integer',
      variant: 'unknown',
    };

    return typeMap[type.toLowerCase()] || 'unknown';
  }

  private extractProcedures(module: VB6Module, filename: string): Procedure[] {
    return module.procedures.map(proc => ({
      name: proc.name,
      type: proc.type === 'function' ? 'function' : 'method',
      parameters: proc.parameters.map(p => ({
        name: p.name,
        type: this.vbTypeToDataType(p.type),
        direction: p.byRef ? 'inout' : 'in',
      })),
      returnType: proc.returnType ? this.vbTypeToDataType(proc.returnType) : undefined,
      localVariables: [],
      calledProcedures: proc.statements
        .filter(s => s.type === 'CALL' || s.raw.includes('.'))
        .map(s => {
          const callMatch = s.raw.match(/Call\s+(\w+)/i);
          if (callMatch) return callMatch[1]!;
          const dotMatch = s.raw.match(/(\w+)\./);
          if (dotMatch) return dotMatch[1]!;
          return '';
        })
        .filter(Boolean),
      location: this.createLocation(filename, proc.startLine, proc.endLine),
      complexity: this.calculateComplexity(proc.statements),
    }));
  }

  private calculateComplexity(statements: VB6Statement[]): number {
    let complexity = 1;
    
    for (const stmt of statements) {
      if (['IF', 'FOR', 'DO', 'WHILE', 'SELECT', 'CALL'].includes(stmt.type)) {
        complexity++;
      }
    }

    return complexity;
  }
}
