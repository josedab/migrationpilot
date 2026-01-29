/**
 * CICS Transaction Parser
 * 
 * Parser for IBM CICS command-level programs. Extracts CICS commands from
 * COBOL, PL/I, or Assembler source code to understand transaction flow.
 */

import type { ASTNode, DataStructure, Procedure } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParseError, type ParseWarning, type ParserOptions } from '../common/base-parser';

export interface CICSParserOptions extends ParserOptions {
  hostLanguage?: 'cobol' | 'pli' | 'assembler';
  cicsVersion?: string;
}

interface CICSCommand {
  command: string;
  options: Map<string, string>;
  line: number;
  endLine: number;
}

export class CICSParser extends BaseParser {
  private commands: CICSCommand[] = [];
  private source = '';
  private filename = '';
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: CICSParserOptions = {}) {
    super({ ...options, dialect: options.hostLanguage || 'cobol' });
  }

  getSupportedDialects(): string[] {
    return ['cobol', 'pli', 'assembler'];
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.source = source;
    this.filename = filename;
    this.errors = [];
    this.warnings = [];
    this.commands = [];

    try {
      this.extractCICSCommands(source);
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
          language: 'cics',
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
        'CICS001',
        'fatal'
      ));

      return {
        success: false,
        dataStructures: [],
        procedures: [],
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'cics',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: source.split('\n').length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private extractCICSCommands(source: string): void {
    const lines = source.split('\n');
    
    // EXEC CICS ... END-EXEC pattern
    const execPattern = /EXEC\s+CICS\s+/gi;
    const endExecPattern = /END-EXEC/gi;
    
    let i = 0;
    while (i < lines.length) {
      const line = lines[i];
      const execMatch = execPattern.exec(line!);
      
      if (execMatch) {
        const startLine = i + 1;
        let commandText = line!.substring(execMatch.index + execMatch[0].length);
        let endLine = startLine;
        
        // Check if END-EXEC is on same line
        let endMatch = endExecPattern.exec(commandText);
        if (!endMatch) {
          // Continue to next lines until END-EXEC
          i++;
          while (i < lines.length) {
            const nextLine = lines[i]!;
            commandText += ' ' + nextLine;
            endMatch = endExecPattern.exec(nextLine!);
            if (endMatch) {
              endLine = i + 1;
              commandText = commandText.substring(0, commandText.lastIndexOf('END-EXEC'));
              break;
            }
            i++;
          }
        } else {
          commandText = commandText.substring(0, endMatch.index);
        }
        
        const command = this.parseCICSCommand(commandText.trim(), startLine, endLine);
        if (command) {
          this.commands.push(command);
        }
        
        execPattern.lastIndex = 0;
        endExecPattern.lastIndex = 0;
      }
      i++;
    }
  }

  private parseCICSCommand(text: string, startLine: number, endLine: number): CICSCommand | null {
    // Clean up the text
    text = text.replace(/\s+/g, ' ').trim();
    
    // First word is the command
    const parts = text.split(' ');
    const command = parts[0]?.toUpperCase() || 'UNKNOWN';
    
    // Parse options
    const options = new Map<string, string>();
    let currentOption = '';
    let currentValue = '';
    let inParen = false;
    
    for (let i = 1; i < parts.length; i++) {
      const part = parts[i]!;
      
      if (part.includes('(')) {
        const openParen = part.indexOf('(');
        currentOption = part.substring(0, openParen).toUpperCase();
        
        if (part.includes(')')) {
          // Option and value on same token
          const closeParen = part.indexOf(')');
          currentValue = part.substring(openParen + 1, closeParen);
          options.set(currentOption, currentValue);
          currentOption = '';
          currentValue = '';
        } else {
          // Value continues on next tokens
          currentValue = part.substring(openParen + 1);
          inParen = true;
        }
      } else if (inParen) {
        if (part.includes(')')) {
          const closeParen = part.indexOf(')');
          currentValue += ' ' + part.substring(0, closeParen);
          options.set(currentOption, currentValue.trim());
          currentOption = '';
          currentValue = '';
          inParen = false;
        } else {
          currentValue += ' ' + part;
        }
      } else {
        // Simple keyword without value
        options.set(part.toUpperCase(), '');
      }
    }
    
    return { command, options, line: startLine, endLine };
  }

  private buildAST(): ASTNode {
    const children: ASTNode[] = [];
    
    // Group commands by type
    const commandGroups: Map<string, CICSCommand[]> = new Map();
    
    for (const cmd of this.commands) {
      const group = this.getCommandGroup(cmd.command);
      if (!commandGroups.has(group)) {
        commandGroups.set(group, []);
      }
      commandGroups.get(group)!.push(cmd);
    }
    
    // Create AST nodes for each command
    for (const cmd of this.commands) {
      const node = this.createCommandNode(cmd);
      children.push(node);
    }
    
    return {
      type: 'CICSProgram',
      name: 'CICS_PROGRAM',
      location: this.createLocation(
        this.filename,
        1,
        this.source.split('\n').length
      ),
      children,
      metadata: {
        commandCount: this.commands.length,
        commandTypes: Array.from(new Set(this.commands.map(c => c.command))),
      },
    };
  }

  private getCommandGroup(command: string): string {
    const groups: Record<string, string[]> = {
      'terminal': ['SEND', 'RECEIVE', 'CONVERSE', 'ISSUE'],
      'file': ['READ', 'WRITE', 'REWRITE', 'DELETE', 'UNLOCK', 'STARTBR', 'READNEXT', 'READPREV', 'ENDBR', 'RESETBR'],
      'queue': ['WRITEQ', 'READQ', 'DELETEQ'],
      'program': ['LINK', 'XCTL', 'RETURN', 'LOAD', 'RELEASE'],
      'task': ['START', 'RETRIEVE', 'CANCEL'],
      'sync': ['SYNCPOINT'],
      'interval': ['ASKTIME', 'FORMATTIME', 'DELAY', 'POST', 'WAIT'],
      'storage': ['GETMAIN', 'FREEMAIN'],
      'exception': ['HANDLE', 'IGNORE', 'PUSH', 'POP', 'ABEND'],
      'journal': ['WRITE JOURNALNUM'],
      'trace': ['ENTER', 'TRACE'],
    };
    
    for (const [group, commands] of Object.entries(groups)) {
      if (commands.some(c => command.startsWith(c))) {
        return group;
      }
    }
    return 'other';
  }

  private createCommandNode(cmd: CICSCommand): ASTNode {
    const options: Record<string, string> = {};
    for (const [key, value] of cmd.options) {
      options[key] = value;
    }
    
    return {
      type: 'CICSCommand',
      name: cmd.command,
      location: this.createLocation(this.filename, cmd.line, cmd.endLine),
      children: [],
      metadata: {
        command: cmd.command,
        group: this.getCommandGroup(cmd.command),
        options,
        hasResponse: cmd.options.has('RESP') || cmd.options.has('RESP2'),
        isAsync: cmd.command === 'START',
      },
    };
  }

  private extractDataStructures(ast: ASTNode): DataStructure[] {
    const structures: DataStructure[] = [];
    const commAreas = new Set<string>();
    const channels = new Set<string>();
    
    // Find COMMAREA and CHANNEL references
    for (const node of ast.children || []) {
      const options = node.metadata?.options as Record<string, string>;
      if (options) {
        if (options['COMMAREA']) {
          commAreas.add(options['COMMAREA']);
        }
        if (options['CHANNEL']) {
          channels.add(options['CHANNEL']);
        }
      }
    }
    
    // Create data structures for COMMAReas
    for (const name of commAreas) {
      structures.push({
        name,
        type: 'group',
        location: this.createLocation(this.filename, 1, 1),
      });
    }
    
    return structures;
  }

  private extractProcedures(ast: ASTNode): Procedure[] {
    const procedures: Procedure[] = [];
    
    // Extract LINK and XCTL targets as called procedures
    const linkedPrograms = new Set<string>();
    const xctlPrograms = new Set<string>();
    
    for (const node of ast.children || []) {
      const cmd = node.metadata?.command as string;
      const options = node.metadata?.options as Record<string, string>;
      
      if (cmd === 'LINK' && options?.['PROGRAM']) {
        linkedPrograms.add(options['PROGRAM'].replace(/'/g, ''));
      }
      if (cmd === 'XCTL' && options?.['PROGRAM']) {
        xctlPrograms.add(options['PROGRAM'].replace(/'/g, ''));
      }
    }
    
    // Create main transaction procedure
    procedures.push({
      name: 'MAIN',
      type: 'program',
      parameters: [],
      localVariables: [],
      calledProcedures: [...linkedPrograms, ...xctlPrograms],
      complexity: this.calculateComplexity(ast),
      location: ast.location,
    });
    
    return procedures;
  }

  private calculateComplexity(ast: ASTNode): number {
    let complexity = 1;
    
    // Add complexity for each command
    for (const node of ast.children || []) {
      const cmd = node.metadata?.command as string;
      
      // Branch/control commands add complexity
      if (['LINK', 'XCTL', 'RETURN', 'HANDLE', 'ABEND'].includes(cmd)) {
        complexity++;
      }
      
      // File operations add complexity
      if (['READ', 'WRITE', 'REWRITE', 'DELETE'].includes(cmd)) {
        complexity++;
      }
    }
    
    return complexity;
  }
}

export { CICSParser as default };
