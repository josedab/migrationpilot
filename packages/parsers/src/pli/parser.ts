/**
 * PL/I Parser
 * 
 * Parser for IBM PL/I programming language used in mainframe environments.
 * Supports PL/I for MVS, PL/I for OS/390, and Enterprise PL/I dialects.
 */

import type { ASTNode, DataStructure, Procedure } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParseError, type ParseWarning, type ParserOptions } from '../common/base-parser';

export interface PLIParserOptions extends ParserOptions {
  dialect?: 'mvs' | 'os390' | 'enterprise' | 'optimizing';
  enableMacros?: boolean;
  includeSearchPaths?: string[];
}

interface PLIToken {
  type: string;
  value: string;
  line: number;
  column: number;
}

export class PLIParser extends BaseParser {
  private tokens: PLIToken[] = [];
  private currentIndex = 0;
  private filename = '';
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: PLIParserOptions = {}) {
    super({ ...options, dialect: options.dialect || 'enterprise' });
  }

  getSupportedDialects(): string[] {
    return ['mvs', 'os390', 'enterprise', 'optimizing'];
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.filename = filename;
    this.errors = [];
    this.warnings = [];
    this.currentIndex = 0;

    try {
      this.tokens = this.tokenize(source);
      const ast = this.parseProgram();
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
          language: 'pli',
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
        'PLI001',
        'fatal'
      ));

      return {
        success: false,
        dataStructures: [],
        procedures: [],
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'pli',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: source.split('\n').length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private tokenize(source: string): PLIToken[] {
    const tokens: PLIToken[] = [];
    const lines = source.split('\n');

    // PL/I keywords
    const keywords = new Set([
      'PROCEDURE', 'PROC', 'END', 'DECLARE', 'DCL', 'BEGIN', 'DO', 'IF', 'THEN',
      'ELSE', 'CALL', 'RETURN', 'GO', 'TO', 'GOTO', 'SELECT', 'WHEN', 'OTHERWISE',
      'ALLOCATE', 'FREE', 'OPEN', 'CLOSE', 'READ', 'WRITE', 'PUT', 'GET', 'DISPLAY',
      'ENTRY', 'RETURNS', 'RECURSIVE', 'REENTRANT', 'OPTIONS', 'MAIN', 'EXTERNAL',
      'INTERNAL', 'STATIC', 'AUTOMATIC', 'CONTROLLED', 'BASED', 'DEFINED', 'LIKE',
      'FIXED', 'FLOAT', 'BINARY', 'DECIMAL', 'CHARACTER', 'CHAR', 'BIT', 'VARYING',
      'VAR', 'PICTURE', 'PIC', 'POINTER', 'PTR', 'OFFSET', 'FILE', 'LABEL', 'FORMAT',
      'CONDITION', 'AREA', 'EVENT', 'TASK', 'STRUCTURE', 'UNION', 'DIMENSION', 'DIM',
      'INITIAL', 'INIT', 'VALUE', 'BUILTIN', 'GENERIC', 'FETCH', 'RELEASE',
      'ON', 'SIGNAL', 'REVERT', 'WHILE', 'UNTIL', 'REPEAT', 'LEAVE', 'ITERATE',
      'BY', 'NAME', 'LIST', 'SKIP', 'PAGE', 'LINE', 'COLUMN', 'COL', 'DATA',
      'EDIT', 'STREAM', 'RECORD', 'INPUT', 'OUTPUT', 'UPDATE', 'PRINT', 'SEQUENTIAL',
      'DIRECT', 'KEYED', 'ENVIRONMENT', 'ENV', 'TITLE', 'LINESIZE', 'PAGESIZE',
    ]);

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum]!; // Safe: iterating within bounds
      let col = 0;

      while (col < line.length) {
        const char = line[col]!!; // Safe: col < line.length
        // Skip whitespace
        if (/\s/.test(char)) {
          col++;
          continue;
        }

        // Comments (/* ... */)
        if (line.substring(col, col + 2) === '/*') {
          let endCol = line.indexOf('*/', col + 2);
          if (endCol === -1) {
            // Multi-line comment - simplified handling
            endCol = line.length;
          } else {
            endCol += 2;
          }
          tokens.push({
            type: 'COMMENT',
            value: line.substring(col, endCol),
            line: lineNum + 1,
            column: col + 1,
          });
          col = endCol;
          continue;
        }

        // String literals
        if (line[col]! === "'" || line[col]! === '"') {
          const quote = line[col]!;
          let endCol = col + 1;
          while (endCol < line.length && line[endCol]! !== quote) {
            if (line[endCol]! === quote && line[endCol + 1] === quote) {
              endCol += 2; // Escaped quote
            } else {
              endCol++;
            }
          }
          endCol++; // Include closing quote
          tokens.push({
            type: 'STRING',
            value: line.substring(col, endCol),
            line: lineNum + 1,
            column: col + 1,
          });
          col = endCol;
          continue;
        }

        // Numbers
        if (/\d/.test(line[col]!)) {
          let endCol = col;
          while (endCol < line.length && /[\d.eEbB]/.test(line[endCol]!)) {
            endCol++;
          }
          tokens.push({
            type: 'NUMBER',
            value: line.substring(col, endCol),
            line: lineNum + 1,
            column: col + 1,
          });
          col = endCol;
          continue;
        }

        // Identifiers and keywords
        if (/[a-zA-Z_$#@]/.test(line[col]!)) {
          let endCol = col;
          while (endCol < line.length && /[a-zA-Z0-9_$#@]/.test(line[endCol]!)) {
            endCol++;
          }
          const value = line.substring(col, endCol);
          const upperValue = value.toUpperCase();
          tokens.push({
            type: keywords.has(upperValue) ? upperValue : 'IDENTIFIER',
            value,
            line: lineNum + 1,
            column: col + 1,
          });
          col = endCol;
          continue;
        }

        // Operators and punctuation
        const twoChar = line.substring(col, col + 2);
        const operators2 = ['**', '->', '||', '&&', '<=', '>=', '¬=', '^=', '!=', ':='];
        if (operators2.includes(twoChar)) {
          tokens.push({
            type: 'OPERATOR',
            value: twoChar,
            line: lineNum + 1,
            column: col + 1,
          });
          col += 2;
          continue;
        }

        // Single character
        tokens.push({
          type: line[col]! === ';' ? 'SEMICOLON' : 
                line[col]! === '(' ? 'LPAREN' :
                line[col]! === ')' ? 'RPAREN' :
                line[col]! === ',' ? 'COMMA' :
                line[col]! === ':' ? 'COLON' :
                'OPERATOR',
          value: line[col]!,
          line: lineNum + 1,
          column: col + 1,
        });
        col++;
      }
    }

    return tokens;
  }

  private parseProgram(): ASTNode {
    const children: ASTNode[] = [];
    const startLine = this.tokens[0]?.line || 1;

    while (this.currentIndex < this.tokens.length) {
      const token = this.peek();
      if (!token) break;

      if (token.type === 'PROCEDURE' || token.type === 'PROC') {
        children.push(this.parseProcedure());
      } else if (token.type === 'DECLARE' || token.type === 'DCL') {
        children.push(this.parseDeclaration());
      } else if (token.type === 'COMMENT') {
        this.advance();
      } else {
        children.push(this.parseStatement());
      }
    }

    return {
      type: 'Program',
      name: 'PLI_PROGRAM',
      location: this.createLocation(this.filename, startLine, this.tokens[this.tokens.length - 1]?.line || startLine),
      children,
      metadata: {},
    };
  }

  private parseProcedure(): ASTNode {
    const startToken = this.advance()!; // PROCEDURE or PROC
    const nameToken = this.advance();
    const name = nameToken?.type === 'IDENTIFIER' ? nameToken.value : 'UNNAMED';

    // Parse options like OPTIONS(MAIN)
    const options: string[] = [];
    if (this.peek()?.type === 'OPTIONS') {
      this.advance();
      if (this.peek()?.type === 'LPAREN') {
        this.advance();
        while (this.peek() && this.peek()?.type !== 'RPAREN') {
          const opt = this.advance();
          if (opt?.type === 'IDENTIFIER' || opt?.type === 'MAIN') {
            options.push(opt.value);
          }
        }
        if (this.peek()?.type === 'RPAREN') this.advance();
      }
    }

    // Parse parameters
    const parameters: ASTNode[] = [];
    if (this.peek()?.type === 'LPAREN') {
      this.advance();
      while (this.peek() && this.peek()?.type !== 'RPAREN') {
        const param = this.advance();
        if (param?.type === 'IDENTIFIER') {
          parameters.push({
            type: 'Parameter',
            name: param.value,
            location: this.createLocation(this.filename, param.line, param.line),
            children: [],
            metadata: {},
          });
        }
        if (this.peek()?.type === 'COMMA') this.advance();
      }
      if (this.peek()?.type === 'RPAREN') this.advance();
    }

    // Skip to semicolon
    while (this.peek() && this.peek()?.type !== 'SEMICOLON') {
      this.advance();
    }
    if (this.peek()?.type === 'SEMICOLON') this.advance();

    // Parse procedure body
    const body: ASTNode[] = [];
    while (this.peek() && this.peek()?.type !== 'END') {
      if (this.peek()?.type === 'DECLARE' || this.peek()?.type === 'DCL') {
        body.push(this.parseDeclaration());
      } else if (this.peek()?.type === 'COMMENT') {
        this.advance();
      } else {
        body.push(this.parseStatement());
      }
    }

    // Consume END
    let endLine = startToken.line;
    if (this.peek()?.type === 'END') {
      const endToken = this.advance()!;
      endLine = endToken.line;
      // Skip procedure name after END if present
      if (this.peek()?.type === 'IDENTIFIER') this.advance();
      if (this.peek()?.type === 'SEMICOLON') this.advance();
    }

    return {
      type: 'Procedure',
      name,
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: [...parameters, ...body],
      metadata: { options, parameters: parameters.map(p => p.name) },
    };
  }

  private parseDeclaration(): ASTNode {
    const startToken = this.advance()!; // DECLARE or DCL
    const declarations: ASTNode[] = [];

    // Parse declaration list until semicolon
    while (this.peek() && this.peek()?.type !== 'SEMICOLON') {
      const nameToken = this.peek();
      if (nameToken?.type === 'IDENTIFIER') {
        this.advance();
        
        // Parse attributes
        const attributes: string[] = [];
        let dataType = 'UNKNOWN';
        let dimension: number[] = [];
        
        while (this.peek() && 
               this.peek()?.type !== 'SEMICOLON' && 
               this.peek()?.type !== 'COMMA') {
          const attr = this.advance();
          if (!attr) break;
          
          if (['FIXED', 'FLOAT', 'BINARY', 'DECIMAL', 'CHARACTER', 'CHAR', 
               'BIT', 'POINTER', 'PTR', 'FILE', 'LABEL', 'STRUCTURE', 
               'PICTURE', 'PIC', 'VARYING', 'VAR'].includes(attr.type)) {
            dataType = attr.type;
            attributes.push(attr.value);
          } else if (attr.type === 'DIMENSION' || attr.type === 'DIM') {
            if (this.peek()?.type === 'LPAREN') {
              this.advance();
              while (this.peek() && this.peek()?.type !== 'RPAREN') {
                const dim = this.advance();
                if (dim?.type === 'NUMBER') {
                  dimension.push(parseInt(dim.value, 10));
                }
                if (this.peek()?.type === 'COMMA') this.advance();
              }
              if (this.peek()?.type === 'RPAREN') this.advance();
            }
          } else if (attr.type === 'LPAREN') {
            // Skip nested parentheses
            let depth = 1;
            while (depth > 0 && this.peek()) {
              const t = this.advance();
              if (t?.type === 'LPAREN') depth++;
              if (t?.type === 'RPAREN') depth--;
            }
          } else if (attr.type !== 'COMMENT') {
            attributes.push(attr.value);
          }
        }

        declarations.push({
          type: 'Declaration',
          name: nameToken.value,
          location: this.createLocation(this.filename, nameToken.line, nameToken.line),
          children: [],
          metadata: { dataType, attributes, dimension },
        });
      }

      if (this.peek()?.type === 'COMMA') {
        this.advance();
      }
    }

    if (this.peek()?.type === 'SEMICOLON') this.advance();

    return {
      type: 'DeclarationBlock',
      name: 'DECLARATIONS',
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: declarations,
      metadata: {},
    };
  }

  private parseStatement(): ASTNode {
    const token = this.peek();
    if (!token) {
      return {
        type: 'Empty',
        name: '',
        location: this.createLocation(this.filename, 1, 1),
        children: [],
        metadata: {},
      };
    }

    switch (token.type) {
      case 'IF':
        return this.parseIfStatement();
      case 'DO':
        return this.parseDoStatement();
      case 'CALL':
        return this.parseCallStatement();
      case 'SELECT':
        return this.parseSelectStatement();
      case 'RETURN':
        return this.parseReturnStatement();
      case 'PUT':
      case 'GET':
        return this.parseIOStatement();
      default:
        return this.parseAssignmentOrExpression();
    }
  }

  private parseIfStatement(): ASTNode {
    const startToken = this.advance()!; // IF
    
    // Parse condition
    const condition: PLIToken[] = [];
    while (this.peek() && this.peek()?.type !== 'THEN') {
      condition.push(this.advance()!);
    }
    if (this.peek()?.type === 'THEN') this.advance();

    // Parse then branch
    const thenBranch = this.parseStatement();

    // Parse else branch if present
    let elseBranch: ASTNode | undefined;
    if (this.peek()?.type === 'ELSE') {
      this.advance();
      elseBranch = this.parseStatement();
    }

    return {
      type: 'IfStatement',
      name: 'IF',
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: elseBranch ? [thenBranch, elseBranch] : [thenBranch],
      metadata: { condition: condition.map(t => t.value).join(' ') },
    };
  }

  private parseDoStatement(): ASTNode {
    const startToken = this.advance()!; // DO
    const properties: Record<string, unknown> = {};

    // Parse DO options (WHILE, UNTIL, iteration)
    if (this.peek()?.type === 'WHILE') {
      this.advance();
      const condition: string[] = [];
      if (this.peek()?.type === 'LPAREN') {
        this.advance();
        while (this.peek() && this.peek()?.type !== 'RPAREN') {
          condition.push(this.advance()!.value);
        }
        if (this.peek()?.type === 'RPAREN') this.advance();
      }
      properties['while'] = condition.join(' ');
    }

    if (this.peek()?.type === 'SEMICOLON') this.advance();

    // Parse body
    const body: ASTNode[] = [];
    while (this.peek() && this.peek()?.type !== 'END') {
      body.push(this.parseStatement());
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END') {
      endLine = this.advance()!.line;
      if (this.peek()?.type === 'SEMICOLON') this.advance();
    }

    return {
      type: 'DoLoop',
      name: 'DO',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: body,
      metadata: properties,
    };
  }

  private parseCallStatement(): ASTNode {
    const startToken = this.advance()!; // CALL
    const nameToken = this.advance();
    const name = nameToken?.type === 'IDENTIFIER' ? nameToken.value : 'UNKNOWN';

    // Parse arguments
    const args: string[] = [];
    if (this.peek()?.type === 'LPAREN') {
      this.advance();
      while (this.peek() && this.peek()?.type !== 'RPAREN') {
        const arg = this.advance();
        if (arg && arg.type !== 'COMMA') {
          args.push(arg.value);
        }
      }
      if (this.peek()?.type === 'RPAREN') this.advance();
    }

    if (this.peek()?.type === 'SEMICOLON') this.advance();

    return {
      type: 'CallStatement',
      name,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { arguments: args },
    };
  }

  private parseSelectStatement(): ASTNode {
    const startToken = this.advance()!; // SELECT
    if (this.peek()?.type === 'SEMICOLON') this.advance();

    const cases: ASTNode[] = [];

    while (this.peek() && 
           (this.peek()?.type === 'WHEN' || this.peek()?.type === 'OTHERWISE')) {
      if (this.peek()?.type === 'WHEN') {
        this.advance();
        const condition: string[] = [];
        if (this.peek()?.type === 'LPAREN') {
          this.advance();
          while (this.peek() && this.peek()?.type !== 'RPAREN') {
            condition.push(this.advance()!.value);
          }
          if (this.peek()?.type === 'RPAREN') this.advance();
        }
        
        const body = this.parseStatement();
        cases.push({
          type: 'WhenClause',
          name: 'WHEN',
          location: body.location,
          children: [body],
          metadata: { condition: condition.join(' ') },
        });
      } else if (this.peek()?.type === 'OTHERWISE') {
        this.advance();
        const body = this.parseStatement();
        cases.push({
          type: 'OtherwiseClause',
          name: 'OTHERWISE',
          location: body.location,
          children: [body],
          metadata: {},
        });
      }
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END') {
      endLine = this.advance()!.line;
      if (this.peek()?.type === 'SEMICOLON') this.advance();
    }

    return {
      type: 'SelectStatement',
      name: 'SELECT',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: cases,
      metadata: {},
    };
  }

  private parseReturnStatement(): ASTNode {
    const startToken = this.advance()!; // RETURN
    let value: string | undefined;

    if (this.peek()?.type === 'LPAREN') {
      this.advance();
      const expr: string[] = [];
      while (this.peek() && this.peek()?.type !== 'RPAREN') {
        expr.push(this.advance()!.value);
      }
      if (this.peek()?.type === 'RPAREN') this.advance();
      value = expr.join(' ');
    }

    if (this.peek()?.type === 'SEMICOLON') this.advance();

    return {
      type: 'ReturnStatement',
      name: 'RETURN',
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { value },
    };
  }

  private parseIOStatement(): ASTNode {
    const startToken = this.advance()!; // PUT or GET
    const type = startToken.type;

    // Skip to semicolon
    const parts: string[] = [];
    while (this.peek() && this.peek()?.type !== 'SEMICOLON') {
      parts.push(this.advance()!.value);
    }
    if (this.peek()?.type === 'SEMICOLON') this.advance();

    return {
      type: `${type}Statement`,
      name: type,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { expression: parts.join(' ') },
    };
  }

  private parseAssignmentOrExpression(): ASTNode {
    const startToken = this.advance()!;
    
    // Skip to semicolon
    const tokens: PLIToken[] = [startToken];
    while (this.peek() && this.peek()?.type !== 'SEMICOLON') {
      tokens.push(this.advance()!);
    }
    if (this.peek()?.type === 'SEMICOLON') this.advance();

    // Determine if assignment or expression
    const hasAssignment = tokens.some(t => t.value === '=' || t.value === ':=');
    
    return {
      type: hasAssignment ? 'Assignment' : 'Expression',
      name: startToken.value,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { expression: tokens.map(t => t.value).join(' ') },
    };
  }

  private extractDataStructures(ast: ASTNode): DataStructure[] {
    const structures: DataStructure[] = [];
    
    const visit = (node: ASTNode) => {
      if (node.type === 'Declaration' && node.name) {
        structures.push({
          name: node.name,
          type: 'unknown',
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
      if (node.type === 'Procedure' && node.name) {
        const params = (node.metadata?.parameters as string[]) || [];
        procedures.push({
          name: node.name,
          type: 'function',
          parameters: params.map(p => ({
            name: p,
            type: 'unknown',
            direction: 'in' as const,
          })),
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
      if (n.type === 'CallStatement' && n.name) {
        calls.push(n.name);
      }
      n.children?.forEach(visit);
    };
    
    visit(node);
    return [...new Set(calls)];
  }

  private calculateComplexity(node: ASTNode): number {
    let complexity = 1;
    
    const visit = (n: ASTNode) => {
      if (['IfStatement', 'DoLoop', 'WhenClause', 'SelectStatement'].includes(n.type)) {
        complexity++;
      }
      n.children?.forEach(visit);
    };
    
    visit(node);
    return complexity;
  }

  private peek(): PLIToken | undefined {
    return this.tokens[this.currentIndex];
  }

  private advance(): PLIToken | undefined {
    return this.tokens[this.currentIndex++];
  }
}

export { PLIParser as default };
