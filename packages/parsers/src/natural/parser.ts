/**
 * Natural/ADABAS Parser
 * 
 * Parser for Software AG Natural programming language used with ADABAS database.
 * Supports Natural for Mainframes and Natural for Unix/Linux/Windows.
 */

import type { ASTNode, DataStructure, Procedure } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParseError, type ParseWarning, type ParserOptions } from '../common/base-parser';

export interface NaturalParserOptions extends ParserOptions {
  dialect?: 'mainframe' | 'unix' | 'windows';
  version?: string;
}

interface NaturalToken {
  type: string;
  value: string;
  line: number;
  column: number;
}

export class NaturalParser extends BaseParser {
  private tokens: NaturalToken[] = [];
  private currentIndex = 0;
  private filename = '';
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: NaturalParserOptions = {}) {
    super({ ...options, dialect: options.dialect || 'mainframe' });
  }

  getSupportedDialects(): string[] {
    return ['mainframe', 'unix', 'windows'];
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
          language: 'natural',
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
        'NAT001',
        'fatal'
      ));

      return {
        success: false,
        dataStructures: [],
        procedures: [],
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'natural',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: source.split('\n').length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private tokenize(source: string): NaturalToken[] {
    const tokens: NaturalToken[] = [];
    const lines = source.split('\n');

    const keywords = new Set([
      'DEFINE', 'DATA', 'LOCAL', 'GLOBAL', 'PARAMETER', 'END-DEFINE',
      'IF', 'THEN', 'ELSE', 'END-IF', 'DECIDE', 'FOR', 'VALUE', 'VALUES',
      'WHEN', 'NONE', 'ANY', 'ALL', 'END-DECIDE',
      'FOR', 'END-FOR', 'REPEAT', 'UNTIL', 'END-REPEAT',
      'WHILE', 'END-WHILE', 'LOOP', 'END-LOOP',
      'PERFORM', 'END-PERFORM', 'SUBROUTINE', 'END-SUBROUTINE',
      'CALLNAT', 'CALL', 'FETCH', 'RETURN', 'ESCAPE',
      'READ', 'FIND', 'GET', 'STORE', 'UPDATE', 'DELETE', 'END-READ', 'END-FIND',
      'HISTOGRAM', 'END-HISTOGRAM',
      'DISPLAY', 'WRITE', 'PRINT', 'INPUT', 'ACCEPT', 'REJECT',
      'MOVE', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      'COMPRESS', 'EXAMINE', 'SEPARATE', 'RESET', 'ASSIGN',
      'AT', 'BREAK', 'END-BREAK', 'ON', 'ERROR', 'END-ERROR',
      'DEFINE', 'WINDOW', 'END-WINDOW', 'MAP', 'END-MAP',
      'INCLUDE', 'STACK', 'TOP', 'BOTTOM', 'FORMATTED',
      'USING', 'WITH', 'WHERE', 'SORTED', 'BY', 'DESCENDING', 'ASCENDING',
      'GIVING', 'INTO', 'FROM', 'TO', 'THRU',
      'TRUE', 'FALSE', 'NOT', 'AND', 'OR', 'EQ', 'NE', 'LT', 'GT', 'LE', 'GE',
      'MASK', 'SCAN', 'SUBSTRING', 'VAL',
      'INIT', 'CONST', 'VIEW', 'REDEFINE', 'FILLER',
    ]);

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum]!;
      let col = 0;

      while (col < line.length) {
        // Skip whitespace
        if (/\s/.test(line[col]!)) {
          col++;
          continue;
        }

        // Comments (start with * in column 1 or /*)
        if ((col === 0 && line[col] === '*') || line.substring(col, col + 2) === '/*') {
          tokens.push({
            type: 'COMMENT',
            value: line.substring(col),
            line: lineNum + 1,
            column: col + 1,
          });
          break; // Rest of line is comment
        }

        // String literals
        if (line[col] === "'" || line[col] === '"') {
          const quote = line[col]!;
          let endCol = col + 1;
          while (endCol < line.length && line[endCol] !== quote) {
            endCol++;
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
          while (endCol < line.length && /[\d.]/.test(line[endCol]!)) {
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

        // Identifiers and keywords (including # prefix for variables)
        if (/[a-zA-Z_#@]/.test(line[col]!)) {
          let endCol = col;
          while (endCol < line.length && /[a-zA-Z0-9_#@-]/.test(line[endCol]!)) {
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
        if ([':=', '<=', '>=', '<>', '/*'].includes(twoChar)) {
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
        const char = line[col]!;
        tokens.push({
          type: char === '(' ? 'LPAREN' :
                char === ')' ? 'RPAREN' :
                char === ',' ? 'COMMA' :
                char === '.' ? 'DOT' :
                'OPERATOR',
          value: char,
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

      if (token.type === 'DEFINE' && this.peekNext()?.type === 'DATA') {
        children.push(this.parseDefineData());
      } else if (token.type === 'DEFINE' && this.peekNext()?.type === 'SUBROUTINE') {
        children.push(this.parseSubroutine());
      } else if (token.type === 'COMMENT') {
        this.advance();
      } else {
        children.push(this.parseStatement());
      }
    }

    return {
      type: 'Program',
      name: 'NATURAL_PROGRAM',
      location: this.createLocation(this.filename, startLine, this.tokens[this.tokens.length - 1]?.line || startLine),
      children,
      metadata: {},
    };
  }

  private parseDefineData(): ASTNode {
    const startToken = this.advance()!; // DEFINE
    this.advance(); // DATA
    
    const scope = this.peek()?.type;
    if (['LOCAL', 'GLOBAL', 'PARAMETER'].includes(scope || '')) {
      this.advance();
    }

    const declarations: ASTNode[] = [];

    while (this.peek() && this.peek()?.type !== 'END-DEFINE') {
      const token = this.peek();
      if (!token) break;

      if (token.type === 'COMMENT') {
        this.advance();
        continue;
      }

      // Parse level number and field definition
      if (token.type === 'NUMBER') {
        const level = parseInt(token.value, 10);
        this.advance();

        const nameToken = this.peek();
        if (nameToken?.type === 'IDENTIFIER') {
          this.advance();

          // Parse type definition
          let dataType = 'UNKNOWN';
          let format = '';
          
          if (this.peek()?.type === 'LPAREN') {
            this.advance();
            const typeTokens: string[] = [];
            while (this.peek() && this.peek()?.type !== 'RPAREN') {
              typeTokens.push(this.advance()!.value);
            }
            if (this.peek()?.type === 'RPAREN') this.advance();
            format = typeTokens.join('');
            dataType = this.parseNaturalFormat(format);
          }

          declarations.push({
            type: 'FieldDefinition',
            name: nameToken.value,
            location: this.createLocation(this.filename, nameToken.line, nameToken.line),
            children: [],
            metadata: { level, dataType, format },
          });
        }
      } else {
        this.advance();
      }
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END-DEFINE') {
      endLine = this.advance()!.line;
    }

    return {
      type: 'DefineData',
      name: 'DATA',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: declarations,
      metadata: { scope },
    };
  }

  private parseNaturalFormat(format: string): string {
    const firstChar = format[0]?.toUpperCase();
    switch (firstChar) {
      case 'A': return 'ALPHANUMERIC';
      case 'B': return 'BINARY';
      case 'C': return 'CONTROL';
      case 'D': return 'DATE';
      case 'F': return 'FLOAT';
      case 'I': return 'INTEGER';
      case 'L': return 'LOGICAL';
      case 'N': return 'NUMERIC';
      case 'P': return 'PACKED';
      case 'T': return 'TIME';
      case 'U': return 'UNICODE';
      default: return 'UNKNOWN';
    }
  }

  private parseSubroutine(): ASTNode {
    const startToken = this.advance()!; // DEFINE
    this.advance(); // SUBROUTINE
    
    const nameToken = this.advance();
    const name = nameToken?.type === 'IDENTIFIER' ? nameToken.value : 'UNNAMED';

    const body: ASTNode[] = [];

    while (this.peek() && this.peek()?.type !== 'END-SUBROUTINE') {
      if (this.peek()?.type === 'COMMENT') {
        this.advance();
        continue;
      }
      body.push(this.parseStatement());
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END-SUBROUTINE') {
      endLine = this.advance()!.line;
    }

    return {
      type: 'Subroutine',
      name,
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: body,
      metadata: {},
    };
  }

  private parseStatement(): ASTNode {
    const token = this.peek();
    if (!token) {
      return this.createEmptyNode();
    }

    switch (token.type) {
      case 'IF':
        return this.parseIfStatement();
      case 'DECIDE':
        return this.parseDecideStatement();
      case 'FOR':
        return this.parseForStatement();
      case 'REPEAT':
        return this.parseRepeatStatement();
      case 'READ':
      case 'FIND':
        return this.parseReadStatement();
      case 'PERFORM':
        return this.parsePerformStatement();
      case 'CALLNAT':
        return this.parseCallnatStatement();
      case 'MOVE':
      case 'COMPUTE':
      case 'ADD':
      case 'SUBTRACT':
      case 'MULTIPLY':
      case 'DIVIDE':
        return this.parseArithmeticStatement();
      case 'DISPLAY':
      case 'WRITE':
      case 'PRINT':
        return this.parseOutputStatement();
      case 'INPUT':
        return this.parseInputStatement();
      case 'ESCAPE':
      case 'RETURN':
        return this.parseControlStatement();
      default:
        return this.parseGenericStatement();
    }
  }

  private createEmptyNode(): ASTNode {
    return {
      type: 'Empty',
      name: '',
      location: this.createLocation(this.filename, 1, 1),
      children: [],
      metadata: {},
    };
  }

  private parseIfStatement(): ASTNode {
    const startToken = this.advance()!; // IF
    
    // Parse condition until THEN
    const condition: string[] = [];
    while (this.peek() && this.peek()?.type !== 'THEN' && this.peek()?.type !== 'END-IF') {
      condition.push(this.advance()!.value);
    }
    if (this.peek()?.type === 'THEN') this.advance();

    const body: ASTNode[] = [];
    let elseBody: ASTNode[] = [];

    while (this.peek() && this.peek()?.type !== 'ELSE' && this.peek()?.type !== 'END-IF') {
      if (this.peek()?.type === 'COMMENT') {
        this.advance();
        continue;
      }
      body.push(this.parseStatement());
    }

    if (this.peek()?.type === 'ELSE') {
      this.advance();
      while (this.peek() && this.peek()?.type !== 'END-IF') {
        if (this.peek()?.type === 'COMMENT') {
          this.advance();
          continue;
        }
        elseBody.push(this.parseStatement());
      }
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END-IF') {
      endLine = this.advance()!.line;
    }

    return {
      type: 'IfStatement',
      name: 'IF',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: [...body, ...elseBody],
      metadata: { condition: condition.join(' '), hasElse: elseBody.length > 0 },
    };
  }

  private parseDecideStatement(): ASTNode {
    const startToken = this.advance()!; // DECIDE
    this.advance(); // FOR or ON
    
    const expression: string[] = [];
    while (this.peek() && this.peek()?.type !== 'WHEN' && this.peek()?.type !== 'END-DECIDE') {
      expression.push(this.advance()!.value);
    }

    const cases: ASTNode[] = [];

    while (this.peek() && (this.peek()?.type === 'WHEN' || this.peek()?.type === 'NONE')) {
      if (this.peek()?.type === 'WHEN') {
        const whenToken = this.advance()!;
        const values: string[] = [];
        while (this.peek() && 
               this.peek()?.type !== 'WHEN' && 
               this.peek()?.type !== 'NONE' && 
               this.peek()?.type !== 'END-DECIDE') {
          if (this.peek()?.type === 'COMMENT') {
            this.advance();
            continue;
          }
          // Check if this is a value or start of statement
          const next = this.peek();
          if (next && ['IF', 'FOR', 'MOVE', 'COMPUTE', 'DISPLAY', 'CALLNAT'].includes(next.type)) {
            break;
          }
          values.push(this.advance()!.value);
        }
        
        const body: ASTNode[] = [];
        while (this.peek() && 
               this.peek()?.type !== 'WHEN' && 
               this.peek()?.type !== 'NONE' && 
               this.peek()?.type !== 'END-DECIDE') {
          if (this.peek()?.type === 'COMMENT') {
            this.advance();
            continue;
          }
          body.push(this.parseStatement());
        }

        cases.push({
          type: 'WhenClause',
          name: 'WHEN',
          location: this.createLocation(this.filename, whenToken.line, whenToken.line),
          children: body,
          metadata: { values: values.join(' ') },
        });
      } else if (this.peek()?.type === 'NONE') {
        const noneToken = this.advance()!;
        const body: ASTNode[] = [];
        while (this.peek() && this.peek()?.type !== 'END-DECIDE') {
          if (this.peek()?.type === 'COMMENT') {
            this.advance();
            continue;
          }
          body.push(this.parseStatement());
        }

        cases.push({
          type: 'NoneClause',
          name: 'NONE',
          location: this.createLocation(this.filename, noneToken.line, noneToken.line),
          children: body,
          metadata: {},
        });
      }
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END-DECIDE') {
      endLine = this.advance()!.line;
    }

    return {
      type: 'DecideStatement',
      name: 'DECIDE',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: cases,
      metadata: { expression: expression.join(' ') },
    };
  }

  private parseForStatement(): ASTNode {
    const startToken = this.advance()!; // FOR
    
    const loopVar = this.advance();
    const loopInfo: string[] = loopVar ? [loopVar.value] : [];
    while (this.peek() && !['IF', 'FOR', 'MOVE', 'COMPUTE', 'DISPLAY', 'END-FOR'].includes(this.peek()!.type)) {
      loopInfo.push(this.advance()!.value);
    }

    const body: ASTNode[] = [];
    while (this.peek() && this.peek()?.type !== 'END-FOR') {
      if (this.peek()?.type === 'COMMENT') {
        this.advance();
        continue;
      }
      body.push(this.parseStatement());
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END-FOR') {
      endLine = this.advance()!.line;
    }

    return {
      type: 'ForStatement',
      name: 'FOR',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: body,
      metadata: { loopInfo: loopInfo.join(' ') },
    };
  }

  private parseRepeatStatement(): ASTNode {
    const startToken = this.advance()!; // REPEAT

    const body: ASTNode[] = [];
    let condition = '';

    while (this.peek() && this.peek()?.type !== 'END-REPEAT' && this.peek()?.type !== 'UNTIL') {
      if (this.peek()?.type === 'COMMENT') {
        this.advance();
        continue;
      }
      body.push(this.parseStatement());
    }

    if (this.peek()?.type === 'UNTIL') {
      this.advance();
      const condTokens: string[] = [];
      while (this.peek() && this.peek()?.type !== 'END-REPEAT') {
        condTokens.push(this.advance()!.value);
      }
      condition = condTokens.join(' ');
    }

    let endLine = startToken.line;
    if (this.peek()?.type === 'END-REPEAT') {
      endLine = this.advance()!.line;
    }

    return {
      type: 'RepeatStatement',
      name: 'REPEAT',
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: body,
      metadata: { condition },
    };
  }

  private parseReadStatement(): ASTNode {
    const startToken = this.advance()!; // READ or FIND
    const type = startToken.type;
    
    const parts: string[] = [];
    while (this.peek() && !['IF', 'FOR', 'MOVE', 'DISPLAY', `END-${type}`].includes(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    const body: ASTNode[] = [];
    const endType = `END-${type}`;
    while (this.peek() && this.peek()?.type !== endType) {
      if (this.peek()?.type === 'COMMENT') {
        this.advance();
        continue;
      }
      body.push(this.parseStatement());
    }

    let endLine = startToken.line;
    if (this.peek()?.type === endType) {
      endLine = this.advance()!.line;
    }

    return {
      type: `${type}Statement`,
      name: type,
      location: this.createLocation(this.filename, startToken.line, endLine),
      children: body,
      metadata: { expression: parts.join(' ') },
    };
  }

  private parsePerformStatement(): ASTNode {
    const startToken = this.advance()!; // PERFORM
    const nameToken = this.advance();
    const name = nameToken?.type === 'IDENTIFIER' ? nameToken.value : 'UNNAMED';

    return {
      type: 'PerformStatement',
      name,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: {},
    };
  }

  private parseCallnatStatement(): ASTNode {
    const startToken = this.advance()!; // CALLNAT
    const parts: string[] = [];
    
    while (this.peek() && !this.isStatementStart(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    const name = parts[0] || 'UNKNOWN';

    return {
      type: 'CallnatStatement',
      name: name.replace(/['"]/g, ''),
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { arguments: parts.slice(1).join(' ') },
    };
  }

  private parseArithmeticStatement(): ASTNode {
    const startToken = this.advance()!;
    const type = startToken.type;
    
    const parts: string[] = [];
    while (this.peek() && !this.isStatementStart(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    return {
      type: `${type}Statement`,
      name: type,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { expression: parts.join(' ') },
    };
  }

  private parseOutputStatement(): ASTNode {
    const startToken = this.advance()!;
    const type = startToken.type;
    
    const parts: string[] = [];
    while (this.peek() && !this.isStatementStart(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    return {
      type: `${type}Statement`,
      name: type,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { expression: parts.join(' ') },
    };
  }

  private parseInputStatement(): ASTNode {
    const startToken = this.advance()!; // INPUT
    
    const parts: string[] = [];
    while (this.peek() && !this.isStatementStart(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    return {
      type: 'InputStatement',
      name: 'INPUT',
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { fields: parts.join(' ') },
    };
  }

  private parseControlStatement(): ASTNode {
    const startToken = this.advance()!;
    const type = startToken.type;
    
    const parts: string[] = [];
    while (this.peek() && !this.isStatementStart(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    return {
      type: `${type}Statement`,
      name: type,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { value: parts.join(' ') },
    };
  }

  private parseGenericStatement(): ASTNode {
    const startToken = this.advance()!;
    
    const parts: string[] = [startToken.value];
    while (this.peek() && !this.isStatementStart(this.peek()!.type)) {
      parts.push(this.advance()!.value);
    }

    return {
      type: 'Statement',
      name: startToken.value,
      location: this.createLocation(this.filename, startToken.line, startToken.line),
      children: [],
      metadata: { code: parts.join(' ') },
    };
  }

  private isStatementStart(type: string): boolean {
    const starters = [
      'IF', 'DECIDE', 'FOR', 'REPEAT', 'READ', 'FIND', 'PERFORM', 'CALLNAT',
      'MOVE', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      'DISPLAY', 'WRITE', 'PRINT', 'INPUT', 'ESCAPE', 'RETURN',
      'END-IF', 'END-DECIDE', 'END-FOR', 'END-REPEAT', 'END-READ', 'END-FIND',
      'END-SUBROUTINE', 'END-DEFINE', 'ELSE', 'WHEN', 'NONE',
    ];
    return starters.includes(type);
  }

  private extractDataStructures(ast: ASTNode): DataStructure[] {
    const structures: DataStructure[] = [];
    
    const visit = (node: ASTNode) => {
      if (node.type === 'FieldDefinition' && (node.metadata?.level as number) === 1 && node.name) {
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
      if (node.type === 'Subroutine' && node.name) {
        procedures.push({
          name: node.name,
          type: 'subroutine',
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
      if ((n.type === 'PerformStatement' || n.type === 'CallnatStatement') && n.name) {
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
      if (['IfStatement', 'DecideStatement', 'ForStatement', 'RepeatStatement', 
           'READStatement', 'FINDStatement', 'WhenClause'].includes(n.type)) {
        complexity++;
      }
      n.children?.forEach(visit);
    };
    
    visit(node);
    return complexity;
  }

  private peek(): NaturalToken | undefined {
    return this.tokens[this.currentIndex];
  }

  private peekNext(): NaturalToken | undefined {
    return this.tokens[this.currentIndex + 1];
  }

  private advance(): NaturalToken | undefined {
    return this.tokens[this.currentIndex++];
  }
}

export { NaturalParser as default };
