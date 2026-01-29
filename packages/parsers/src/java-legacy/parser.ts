/**
 * Legacy Java Parser
 * 
 * Parses J2EE and EJB 2.x code
 */

import type { ASTNode, DataStructure, Procedure, DataType } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParserOptions, type ParseError, type ParseWarning } from '../common/base-parser.js';

export interface JavaLegacyParserOptions extends ParserOptions {
  dialect?: 'j2ee' | 'ejb2' | 'struts1' | 'spring2';
}

export interface JavaClass {
  name: string;
  packageName: string;
  type: 'class' | 'interface' | 'enum' | 'annotation';
  modifiers: string[];
  extends?: string;
  implements: string[];
  annotations: JavaAnnotation[];
  fields: JavaField[];
  methods: JavaMethod[];
  innerClasses: JavaClass[];
  startLine: number;
  endLine: number;
}

export interface JavaAnnotation {
  name: string;
  values: Record<string, string>;
  line: number;
}

export interface JavaField {
  name: string;
  type: string;
  modifiers: string[];
  annotations: JavaAnnotation[];
  initialValue?: string;
  line: number;
}

export interface JavaMethod {
  name: string;
  returnType: string;
  modifiers: string[];
  parameters: JavaParameter[];
  annotations: JavaAnnotation[];
  throws: string[];
  body: string;
  startLine: number;
  endLine: number;
}

export interface JavaParameter {
  name: string;
  type: string;
  annotations: JavaAnnotation[];
}

export interface EJBInfo {
  type: 'session' | 'entity' | 'message-driven';
  beanClass: string;
  homeInterface?: string;
  remoteInterface?: string;
  localHomeInterface?: string;
  localInterface?: string;
  persistenceType?: 'bean' | 'container';
  transactionType?: 'bean' | 'container';
}

const JAVA_DIALECTS = ['j2ee', 'ejb2', 'struts1', 'spring2'];

export class JavaLegacyParser extends BaseParser {
  private lines: string[] = [];
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: JavaLegacyParserOptions = {}) {
    super({
      ...options,
      dialect: options.dialect || 'j2ee',
    });
  }

  getSupportedDialects(): string[] {
    return JAVA_DIALECTS;
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.lines = source.split('\n');
    this.errors = [];
    this.warnings = [];

    try {
      const javaClass = this.parseClass(source, filename);
      const ast = this.buildAST(javaClass, filename);
      const dataStructures = this.extractDataStructures(javaClass);
      const procedures = this.extractProcedures(javaClass, filename);

      // Detect EJB patterns
      const ejbInfo = this.detectEJBPatterns(javaClass);
      if (ejbInfo) {
        this.warnings.push(
          this.createWarning(
            `Detected EJB 2.x ${ejbInfo.type} bean`,
            this.createLocation(filename, 1, 1),
            'EJB_DETECTED'
          )
        );
      }

      return {
        success: this.errors.filter(e => e.severity === 'fatal').length === 0,
        ast,
        dataStructures,
        procedures,
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'java-legacy',
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
          language: 'java-legacy',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: this.lines.length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private parseClass(source: string, _filename: string): JavaClass {
    const lines = source.split('\n');
    
    // Extract package
    const packageMatch = source.match(/package\s+([\w.]+)\s*;/);
    const packageName = packageMatch?.[1] || '';

    // Extract class declaration
    const classMatch = source.match(
      /(?:@\w+(?:\([^)]*\))?\s*)*\s*(public|private|protected)?\s*(abstract|final)?\s*(class|interface|enum)\s+(\w+)(?:\s+extends\s+([\w.]+))?(?:\s+implements\s+([\w.,\s]+))?/
    );

    if (!classMatch) {
      throw new Error('No class declaration found');
    }

    const className = classMatch[4]!;
    const classType = classMatch[3] as 'class' | 'interface' | 'enum';
    const extendsClass = classMatch[5];
    const implementsList = classMatch[6]?.split(',').map(s => s.trim()).filter(Boolean) || [];

    // Parse annotations - include the class match text since the regex consumes annotations
    const classMatchStart = source.indexOf(classMatch[0]);
    const classMatchEnd = classMatchStart + classMatch[0].length;
    const classAnnotations = this.parseAnnotations(source, classMatchStart, classMatchEnd);

    // Parse fields
    const fields = this.parseFields(source);

    // Parse methods
    const methods = this.parseMethods(source);

    return {
      name: className,
      packageName,
      type: classType,
      modifiers: [classMatch[1], classMatch[2]].filter(Boolean) as string[],
      extends: extendsClass,
      implements: implementsList,
      annotations: classAnnotations,
      fields,
      methods,
      innerClasses: [],
      startLine: 1,
      endLine: lines.length,
    };
  }

  private parseAnnotations(source: string, startIndex: number, endIndex: number): JavaAnnotation[] {
    const annotations: JavaAnnotation[] = [];
    const section = source.slice(startIndex, endIndex);
    const annotationMatches = section.matchAll(/@(\w+)(?:\(([^)]*)\))?/g);

    for (const match of annotationMatches) {
      const values: Record<string, string> = {};
      if (match[2]) {
        const valueMatches = match[2].matchAll(/(\w+)\s*=\s*(?:"([^"]*)"|(\w+))/g);
        for (const vm of valueMatches) {
          values[vm[1]!] = vm[2] || vm[3] || '';
        }
        if (Object.keys(values).length === 0 && match[2]) {
          values['value'] = match[2].replace(/"/g, '');
        }
      }

      annotations.push({
        name: match[1]!,
        values,
        line: source.slice(0, startIndex + match.index!).split('\n').length,
      });
    }

    return annotations;
  }

  private parseFields(source: string): JavaField[] {
    const fields: JavaField[] = [];
    
    const fieldPattern = /(?:@\w+(?:\([^)]*\))?\s*)*\s*(private|protected|public)\s+(static\s+)?(final\s+)?([\w<>,\s\[\]]+)\s+(\w+)\s*(?:=\s*([^;]+))?\s*;/g;
    
    let match;
    while ((match = fieldPattern.exec(source)) !== null) {
      if (match[0].includes('(') && match[0].includes(')') && !match[0].includes('new')) continue;
      
      const lineNumber = source.slice(0, match.index).split('\n').length;
      const annotations = this.parseAnnotations(source, Math.max(0, match.index - 300), match.index);

      fields.push({
        name: match[5]!,
        type: match[4]!.trim(),
        modifiers: [match[1], match[2]?.trim(), match[3]?.trim()].filter(Boolean) as string[],
        annotations,
        initialValue: match[6]?.trim(),
        line: lineNumber,
      });
    }

    return fields;
  }

  private parseMethods(source: string): JavaMethod[] {
    const methods: JavaMethod[] = [];
    
    const methodPattern = /(?:@\w+(?:\([^)]*\))?\s*)*\s*(public|private|protected)\s+(static\s+)?(synchronized\s+)?([\w<>,\[\]\s]+)\s+(\w+)\s*\(([^)]*)\)\s*(?:throws\s+([\w,\s]+))?\s*\{/g;
    
    let match;
    while ((match = methodPattern.exec(source)) !== null) {
      const startLine = source.slice(0, match.index).split('\n').length;
      
      let braceCount = 1;
      let endIndex = match.index + match[0].length;
      while (braceCount > 0 && endIndex < source.length) {
        if (source[endIndex] === '{') braceCount++;
        if (source[endIndex] === '}') braceCount--;
        endIndex++;
      }
      
      const endLine = source.slice(0, endIndex).split('\n').length;
      const body = source.slice(match.index + match[0].length, endIndex - 1);
      
      const parameters: JavaParameter[] = [];
      if (match[6]) {
        const paramParts = match[6].split(',');
        for (const part of paramParts) {
          const trimmed = part.trim();
          if (!trimmed) continue;
          
          const paramMatch = trimmed.match(/([\w<>,\[\]\s]+)\s+(\w+)/);
          if (paramMatch) {
            parameters.push({
              name: paramMatch[2]!,
              type: paramMatch[1]!.trim(),
              annotations: [],
            });
          }
        }
      }

      const annotations = this.parseAnnotations(source, Math.max(0, match.index - 300), match.index);
      const throwsList = match[7]?.split(',').map(s => s.trim()).filter(Boolean) || [];

      methods.push({
        name: match[5]!,
        returnType: match[4]!.trim(),
        modifiers: [match[1], match[2]?.trim(), match[3]?.trim()].filter(Boolean) as string[],
        parameters,
        annotations,
        throws: throwsList,
        body,
        startLine,
        endLine,
      });
    }

    return methods;
  }

  private detectEJBPatterns(javaClass: JavaClass): EJBInfo | null {
    const ejbAnnotations = ['Stateless', 'Stateful', 'Singleton', 'MessageDriven', 'Entity'];
    for (const ann of javaClass.annotations) {
      if (ejbAnnotations.includes(ann.name)) {
        return {
          type: ann.name === 'MessageDriven' ? 'message-driven' : 
                ann.name === 'Entity' ? 'entity' : 'session',
          beanClass: javaClass.name,
        };
      }
    }

    const ejbInterfaces = ['SessionBean', 'EntityBean', 'MessageDrivenBean'];
    for (const impl of javaClass.implements) {
      if (ejbInterfaces.some(e => impl.includes(e))) {
        return {
          type: impl.includes('Entity') ? 'entity' : 
                impl.includes('MessageDriven') ? 'message-driven' : 'session',
          beanClass: javaClass.name,
          persistenceType: 'bean',
          transactionType: 'container',
        };
      }
    }

    return null;
  }

  private buildAST(javaClass: JavaClass, filename: string): ASTNode {
    return {
      type: javaClass.type.charAt(0).toUpperCase() + javaClass.type.slice(1),
      name: javaClass.name,
      location: this.createLocation(filename, javaClass.startLine, javaClass.endLine),
      children: [
        ...javaClass.fields.map(f => ({
          type: 'Field',
          name: f.name,
          location: this.createLocation(filename, f.line, f.line),
          children: [],
          metadata: { type: f.type, modifiers: f.modifiers },
        })),
        ...javaClass.methods.map(m => ({
          type: 'Method',
          name: m.name,
          location: this.createLocation(filename, m.startLine, m.endLine),
          children: [],
          metadata: { 
            returnType: m.returnType, 
            parameters: m.parameters,
            modifiers: m.modifiers,
          },
        })),
      ],
      metadata: {
        package: javaClass.packageName,
        extends: javaClass.extends,
        implements: javaClass.implements,
        annotations: javaClass.annotations,
      },
    };
  }

  private extractDataStructures(javaClass: JavaClass): DataStructure[] {
    return javaClass.fields.map(f => ({
      name: f.name,
      type: this.javaTypeToDataType(f.type),
      location: this.createLocation('', f.line, f.line),
    }));
  }

  private javaTypeToDataType(type: string): DataType {
    const normalized = type.toLowerCase().replace(/\s/g, '');
    
    if (['string', 'char', 'character'].some(t => normalized.includes(t))) return 'string';
    if (['int', 'integer', 'long', 'short', 'byte'].some(t => normalized === t)) return 'integer';
    if (['double', 'float', 'bigdecimal'].some(t => normalized.includes(t))) return 'decimal';
    if (normalized === 'boolean') return 'boolean';
    if (['date', 'timestamp', 'localdate'].some(t => normalized.includes(t))) return 'date';
    if (normalized.includes('list') || normalized.includes('[]')) return 'array';

    return 'unknown';
  }

  private extractProcedures(javaClass: JavaClass, filename: string): Procedure[] {
    return javaClass.methods.map(m => ({
      name: m.name,
      type: 'method' as const,
      parameters: m.parameters.map(p => ({
        name: p.name,
        type: this.javaTypeToDataType(p.type),
        direction: 'in' as const,
      })),
      returnType: this.javaTypeToDataType(m.returnType),
      localVariables: [],
      calledProcedures: this.extractMethodCalls(m.body),
      location: this.createLocation(filename, m.startLine, m.endLine),
      complexity: this.calculateComplexity(m.body),
    }));
  }

  private extractMethodCalls(body: string): string[] {
    const calls: string[] = [];
    const callPattern = /(\w+)\s*\(/g;
    
    let match;
    while ((match = callPattern.exec(body)) !== null) {
      const name = match[1]!;
      if (!['if', 'while', 'for', 'switch', 'catch', 'synchronized', 'new'].includes(name)) {
        calls.push(name);
      }
    }

    return [...new Set(calls)];
  }

  private calculateComplexity(body: string): number {
    let complexity = 1;
    const patterns = [/\bif\s*\(/g, /\bfor\s*\(/g, /\bwhile\s*\(/g, /\bcase\s+/g, /\bcatch\s*\(/g, /&&/g, /\|\|/g];
    for (const pattern of patterns) {
      const matches = body.match(pattern);
      if (matches) complexity += matches.length;
    }
    return complexity;
  }
}
