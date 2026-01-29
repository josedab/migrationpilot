/**
 * RPG Parser
 * 
 * Parser for IBM RPG (Report Program Generator) used on AS/400 and IBM i systems.
 * Supports RPG III, RPG IV (ILE RPG), and RPG Free-form dialects.
 */

import type { ASTNode, DataStructure, Procedure } from '@migrationpilot/core';
import { BaseParser, type ParseResult, type ParseError, type ParseWarning, type ParserOptions } from '../common/base-parser';

export interface RPGParserOptions extends ParserOptions {
  dialect?: 'rpg3' | 'rpg4' | 'rpgfree' | 'sqlrpg';
  fixedFormat?: boolean;
}

interface RPGSpec {
  type: 'H' | 'F' | 'D' | 'I' | 'C' | 'O' | 'P' | '**';
  line: number;
  content: string;
  fields: Record<string, string>;
}

export class RPGParser extends BaseParser {
  private specs: RPGSpec[] = [];
  private filename = '';
  private errors: ParseError[] = [];
  private warnings: ParseWarning[] = [];

  constructor(options: RPGParserOptions = {}) {
    super({ ...options, dialect: options.dialect || 'rpg4' });
  }

  getSupportedDialects(): string[] {
    return ['rpg3', 'rpg4', 'rpgfree', 'sqlrpg'];
  }

  parse(source: string, filename: string): ParseResult {
    const startTime = Date.now();
    this.filename = filename;
    this.errors = [];
    this.warnings = [];

    try {
      // Determine if free-form or fixed format
      const isFreeForm = this.detectFreeForm(source);
      
      if (isFreeForm) {
        return this.parseFreeForm(source, filename, startTime);
      } else {
        return this.parseFixedFormat(source, filename, startTime);
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : 'Unknown parse error';
      this.errors.push(this.createError(
        message,
        this.createLocation(filename, 1, 1),
        'RPG001',
        'fatal'
      ));

      return {
        success: false,
        dataStructures: [],
        procedures: [],
        errors: this.errors,
        warnings: this.warnings,
        metadata: {
          language: 'rpg',
          dialect: this.options.dialect,
          sourceFile: filename,
          lines: source.split('\n').length,
          parseTime: Date.now() - startTime,
          version: '1.0.0',
        },
      };
    }
  }

  private detectFreeForm(source: string): boolean {
    const lines = source.split('\n');
    // Check for **FREE directive or lack of column-based specifications
    return lines.some(line => 
      line.trim().toUpperCase().startsWith('**FREE') ||
      line.trim().toUpperCase().startsWith('CTL-OPT') ||
      line.trim().toUpperCase().startsWith('DCL-')
    );
  }

  private parseFixedFormat(source: string, filename: string, startTime: number): ParseResult {
    const lines = source.split('\n');
    this.specs = [];

    // Parse fixed-format specifications
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!;
      if (line.length < 6) continue;

      // Skip comments
      if (line[6] === '*') continue;

      const specType = line[5]?.toUpperCase() as RPGSpec['type'];
      if (!['H', 'F', 'D', 'I', 'C', 'O', 'P'].includes(specType)) continue;

      const spec = this.parseFixedSpec(specType, line!, i + 1);
      if (spec) {
        this.specs.push(spec);
      }
    }

    const ast = this.buildASTFromSpecs();
    const dataStructures = this.extractDataStructuresFromSpecs();
    const procedures = this.extractProceduresFromSpecs();

    return {
      success: this.errors.filter(e => e.severity === 'fatal').length === 0,
      ast,
      dataStructures,
      procedures,
      errors: this.errors,
      warnings: this.warnings,
      metadata: {
        language: 'rpg',
        dialect: this.options.dialect,
        sourceFile: filename,
        lines: lines.length,
        parseTime: Date.now() - startTime,
        version: '1.0.0',
      },
    };
  }

  private parseFixedSpec(type: RPGSpec['type'], line: string, lineNum: number): RPGSpec | null {
    const fields: Record<string, string> = {};

    switch (type) {
      case 'H': // Control (Header) specifications
        fields['keywords'] = line.substring(6).trim();
        break;

      case 'F': // File specifications
        fields['filename'] = line.substring(6, 16).trim();
        fields['fileType'] = line[16] || '';
        fields['fileDesignation'] = line[17] || '';
        fields['endOfFile'] = line[18] || '';
        fields['fileAddition'] = line[19] || '';
        fields['sequence'] = line[20] || '';
        fields['fileFormat'] = line[21] || '';
        fields['recordLength'] = line.substring(22, 27).trim();
        fields['limits'] = line[27] || '';
        fields['keyLength'] = line.substring(28, 33).trim();
        fields['recordAddress'] = line[33] || '';
        fields['fileOrg'] = line[34] || '';
        fields['device'] = line.substring(35, 42).trim();
        fields['keywords'] = line.substring(43).trim();
        break;

      case 'D': // Definition specifications
        fields['name'] = line.substring(6, 21).trim();
        fields['externalType'] = line[21] || '';
        fields['defType'] = line.substring(23, 25).trim();
        fields['fromPos'] = line.substring(25, 32).trim();
        fields['toPos'] = line.substring(32, 39).trim();
        fields['dataType'] = line[39] || '';
        fields['decimals'] = line.substring(40, 42).trim();
        fields['keywords'] = line.substring(43).trim();
        break;

      case 'C': // Calculation specifications
        fields['indicator1'] = line.substring(6, 9).trim();
        fields['indicator2'] = line.substring(9, 12).trim();
        fields['indicator3'] = line.substring(12, 15).trim();
        fields['factor1'] = line.substring(15, 29).trim();
        fields['opcode'] = line.substring(29, 39).trim();
        fields['factor2'] = line.substring(39, 53).trim();
        fields['result'] = line.substring(53, 67).trim();
        fields['length'] = line.substring(67, 72).trim();
        fields['decimals'] = line.substring(72, 74).trim();
        fields['hiLoEq'] = line.substring(74, 77).trim();
        fields['extFactor2'] = line.substring(35).trim();
        break;

      case 'I': // Input specifications
        fields['filename'] = line.substring(6, 16).trim();
        fields['sequence'] = line.substring(16, 18).trim();
        fields['option'] = line[18] || '';
        fields['recordId'] = line.substring(20, 23).trim();
        break;

      case 'O': // Output specifications
        fields['filename'] = line.substring(6, 16).trim();
        fields['type'] = line[16] || '';
        fields['addDel'] = line[17] || '';
        fields['fetchOvflow'] = line[18] || '';
        break;

      case 'P': // Procedure specifications
        fields['name'] = line.substring(6, 21).trim();
        fields['beginEnd'] = line[23] || '';
        fields['keywords'] = line.substring(43).trim();
        break;

      default:
        return null;
    }

    return { type, line: lineNum, content: line, fields };
  }

  private parseFreeForm(source: string, filename: string, startTime: number): ParseResult {
    const children: ASTNode[] = [];
    const lines = source.split('\n');
    let currentProcedure: ASTNode | null = null;
    let inProcedure = false;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i]!.trim();
      const lineNum = i + 1;

      // Skip empty lines and comments
      if (!line || line.startsWith('//')) continue;

      // Control options
      if (line.toUpperCase().startsWith('CTL-OPT')) {
        children.push({
          type: 'ControlOptions',
          name: 'CTL-OPT',
          location: this.createLocation(filename, lineNum, lineNum),
          children: [],
          metadata: { options: this.parseKeywords(line) },
        });
      }
      // File declaration
      else if (line.toUpperCase().startsWith('DCL-F')) {
        const match = line.match(/DCL-F\s+(\w+)/i);
        children.push({
          type: 'FileDeclaration',
          name: match?.[1] || 'UNKNOWN',
          location: this.createLocation(filename, lineNum, lineNum),
          children: [],
          metadata: { declaration: line },
        });
      }
      // Data structure declaration
      else if (line.toUpperCase().startsWith('DCL-DS')) {
        const match = line.match(/DCL-DS\s+(\w+)/i);
        children.push({
          type: 'DataStructure',
          name: match?.[1] || 'UNKNOWN',
          location: this.createLocation(filename, lineNum, lineNum),
          children: [],
          metadata: { declaration: line },
        });
      }
      // Variable declaration
      else if (line.toUpperCase().startsWith('DCL-S')) {
        const match = line.match(/DCL-S\s+(\w+)/i);
        children.push({
          type: 'VariableDeclaration',
          name: match?.[1] || 'UNKNOWN',
          location: this.createLocation(filename, lineNum, lineNum),
          children: [],
          metadata: { declaration: line },
        });
      }
      // Constant declaration
      else if (line.toUpperCase().startsWith('DCL-C')) {
        const match = line.match(/DCL-C\s+(\w+)/i);
        children.push({
          type: 'ConstantDeclaration',
          name: match?.[1] || 'UNKNOWN',
          location: this.createLocation(filename, lineNum, lineNum),
          children: [],
          metadata: { declaration: line },
        });
      }
      // Procedure declaration
      else if (line.toUpperCase().startsWith('DCL-PROC')) {
        const match = line.match(/DCL-PROC\s+(\w+)/i);
        inProcedure = true;
        currentProcedure = {
          type: 'Procedure',
          name: match?.[1] || 'UNKNOWN',
          location: this.createLocation(filename, lineNum, lineNum),
          children: [],
          metadata: { declaration: line },
        };
      }
      // End procedure
      else if (line.toUpperCase().startsWith('END-PROC')) {
        if (currentProcedure) {
          currentProcedure.location = this.createLocation(
            filename,
            currentProcedure.location.startLine,
            lineNum
          );
          children.push(currentProcedure);
        }
        inProcedure = false;
        currentProcedure = null;
      }
      // Statements inside procedures
      else if (inProcedure && currentProcedure) {
        const stmt = this.parseFreeFormStatement(line, lineNum, filename);
        if (stmt) {
          currentProcedure.children.push(stmt);
        }
      }
    }

    const ast: ASTNode = {
      type: 'Program',
      name: 'RPG_PROGRAM',
      location: this.createLocation(filename, 1, lines.length),
      children,
      metadata: { format: 'free' },
    };

    return {
      success: this.errors.filter(e => e.severity === 'fatal').length === 0,
      ast,
      dataStructures: this.extractDataStructures(ast),
      procedures: this.extractProcedures(ast),
      errors: this.errors,
      warnings: this.warnings,
      metadata: {
        language: 'rpg',
        dialect: 'rpgfree',
        sourceFile: filename,
        lines: lines.length,
        parseTime: Date.now() - startTime,
        version: '1.0.0',
      },
    };
  }

  private parseFreeFormStatement(line: string, lineNum: number, filename: string): ASTNode | null {
    const upper = line.toUpperCase();

    // IF statement
    if (upper.startsWith('IF ') || upper.startsWith('IF(')) {
      return {
        type: 'IfStatement',
        name: 'IF',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { condition: line.substring(2).trim() },
      };
    }
    // ELSE
    if (upper === 'ELSE' || upper.startsWith('ELSE;')) {
      return {
        type: 'ElseClause',
        name: 'ELSE',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // ENDIF
    if (upper.startsWith('ENDIF')) {
      return {
        type: 'EndIf',
        name: 'ENDIF',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // DOW (Do While)
    if (upper.startsWith('DOW ') || upper.startsWith('DOW(')) {
      return {
        type: 'DoWhile',
        name: 'DOW',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { condition: line.substring(3).trim() },
      };
    }
    // DOU (Do Until)
    if (upper.startsWith('DOU ') || upper.startsWith('DOU(')) {
      return {
        type: 'DoUntil',
        name: 'DOU',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { condition: line.substring(3).trim() },
      };
    }
    // FOR loop
    if (upper.startsWith('FOR ')) {
      return {
        type: 'ForLoop',
        name: 'FOR',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { expression: line.substring(4).trim() },
      };
    }
    // ENDDO / ENDFOR
    if (upper.startsWith('ENDDO') || upper.startsWith('ENDFOR')) {
      return {
        type: 'EndLoop',
        name: upper.startsWith('ENDDO') ? 'ENDDO' : 'ENDFOR',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // SELECT
    if (upper === 'SELECT' || upper.startsWith('SELECT;')) {
      return {
        type: 'SelectStatement',
        name: 'SELECT',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // WHEN
    if (upper.startsWith('WHEN ') || upper.startsWith('WHEN(')) {
      return {
        type: 'WhenClause',
        name: 'WHEN',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { condition: line.substring(4).trim() },
      };
    }
    // OTHER
    if (upper === 'OTHER' || upper.startsWith('OTHER;')) {
      return {
        type: 'OtherClause',
        name: 'OTHER',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // ENDSL
    if (upper.startsWith('ENDSL')) {
      return {
        type: 'EndSelect',
        name: 'ENDSL',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // RETURN
    if (upper.startsWith('RETURN')) {
      return {
        type: 'ReturnStatement',
        name: 'RETURN',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { value: line.substring(6).trim().replace(/;$/, '') },
      };
    }
    // EVAL
    if (upper.startsWith('EVAL ') || upper.startsWith('EVAL(')) {
      return {
        type: 'EvalStatement',
        name: 'EVAL',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { expression: line.substring(4).trim() },
      };
    }
    // EXSR (Execute Subroutine)
    if (upper.startsWith('EXSR ')) {
      const match = line.match(/EXSR\s+(\w+)/i);
      return {
        type: 'ExsrStatement',
        name: match?.[1] || 'UNKNOWN',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: {},
      };
    }
    // CALLP (Call Procedure)
    if (upper.startsWith('CALLP ') || upper.startsWith('CALLP(')) {
      return {
        type: 'CallStatement',
        name: 'CALLP',
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { call: line.substring(5).trim() },
      };
    }
    // Assignment or expression
    if (line.includes('=') && !line.includes('==') && !line.includes('<=') && !line.includes('>=')) {
      const parts = line.split('=');
      return {
        type: 'Assignment',
        name: parts[0]!.trim(),
        location: this.createLocation(filename, lineNum, lineNum),
        children: [],
        metadata: { value: parts.slice(1).join('=').trim().replace(/;$/, '') },
      };
    }

    // Generic statement
    return {
      type: 'Statement',
      name: 'STMT',
      location: this.createLocation(filename, lineNum, lineNum),
      children: [],
      metadata: { code: line },
    };
  }

  private parseKeywords(line: string): string[] {
    const keywords: string[] = [];
    const match = line.match(/\(([^)]+)\)/g);
    if (match) {
      keywords.push(...match.map(m => m.slice(1, -1)));
    }
    return keywords;
  }

  private buildASTFromSpecs(): ASTNode {
    const children: ASTNode[] = [];

    // Group specs by type
    const hSpecs = this.specs.filter(s => s.type === 'H');
    const fSpecs = this.specs.filter(s => s.type === 'F');
    const dSpecs = this.specs.filter(s => s.type === 'D');
    const cSpecs = this.specs.filter(s => s.type === 'C');
    const pSpecs = this.specs.filter(s => s.type === 'P');

    // Control specs
    for (const spec of hSpecs) {
      children.push({
        type: 'ControlSpec',
        name: 'H',
        location: this.createLocation(this.filename, spec.line, spec.line),
        children: [],
        metadata: spec.fields,
      });
    }

    // File specs
    for (const spec of fSpecs) {
      children.push({
        type: 'FileSpec',
        name: spec.fields['filename'] || 'UNKNOWN',
        location: this.createLocation(this.filename, spec.line, spec.line),
        children: [],
        metadata: spec.fields,
      });
    }

    // Definition specs
    for (const spec of dSpecs) {
      children.push({
        type: 'DefinitionSpec',
        name: spec.fields['name'] || 'UNKNOWN',
        location: this.createLocation(this.filename, spec.line, spec.line),
        children: [],
        metadata: spec.fields,
      });
    }

    // Calculation specs
    let currentProc: ASTNode | null = null;
    for (const spec of [...pSpecs, ...cSpecs].sort((a, b) => a.line - b.line)) {
      if (spec.type === 'P') {
        if (spec.fields['beginEnd']?.toUpperCase() === 'B') {
          currentProc = {
            type: 'Procedure',
            name: spec.fields['name'] || 'UNKNOWN',
            location: this.createLocation(this.filename, spec.line, spec.line),
            children: [],
            metadata: spec.fields,
          };
        } else if (spec.fields['beginEnd']?.toUpperCase() === 'E' && currentProc) {
          currentProc.location = this.createLocation(
            this.filename,
            currentProc.location.startLine,
            spec.line
          );
          children.push(currentProc);
          currentProc = null;
        }
      } else if (spec.type === 'C') {
        const calcNode: ASTNode = {
          type: 'CalculationSpec',
          name: spec.fields['opcode'] || 'STMT',
          location: this.createLocation(this.filename, spec.line, spec.line),
          children: [],
          metadata: spec.fields,
        };
        if (currentProc) {
          currentProc.children.push(calcNode);
        } else {
          children.push(calcNode);
        }
      }
    }

    return {
      type: 'Program',
      name: 'RPG_PROGRAM',
      location: this.createLocation(
        this.filename,
        1,
        this.specs.length > 0 ? this.specs[this.specs.length - 1]!.line : 1
      ),
      children,
      metadata: { format: 'fixed' },
    };
  }

  private extractDataStructuresFromSpecs(): DataStructure[] {
    const structures: DataStructure[] = [];
    
    const dSpecs = this.specs.filter(s => s.type === 'D');
    for (const spec of dSpecs) {
      if (spec.fields['defType'] === 'DS') {
        structures.push({
          name: spec.fields['name'] || 'UNKNOWN',
          type: 'group',
          location: this.createLocation(this.filename, spec.line, spec.line),
        });
      }
    }

    return structures;
  }

  private extractProceduresFromSpecs(): Procedure[] {
    const procedures: Procedure[] = [];
    
    const pSpecs = this.specs.filter(s => s.type === 'P' && s.fields['beginEnd']?.toUpperCase() === 'B');
    for (const spec of pSpecs) {
      procedures.push({
        name: spec.fields['name'] || 'UNKNOWN',
        type: 'subroutine',
        parameters: [],
        localVariables: [],
        calledProcedures: [],
        complexity: 1,
        location: this.createLocation(this.filename, spec.line, spec.line),
      });
    }

    return procedures;
  }

  private extractDataStructures(ast: ASTNode): DataStructure[] {
    const structures: DataStructure[] = [];
    
    const visit = (node: ASTNode) => {
      if (node.type === 'DataStructure' && node.name) {
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
      if (node.type === 'Procedure' && node.name) {
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
      if ((n.type === 'CallStatement' || n.type === 'ExsrStatement') && n.name) {
        calls.push(n.name);
      }
      if (n.type === 'CalculationSpec' && n.metadata?.['opcode']) {
        const opcode = (n.metadata['opcode'] as string).toUpperCase();
        if (['CALL', 'CALLP', 'EXSR'].includes(opcode)) {
          calls.push((n.metadata['factor2'] as string) || n.name || 'UNKNOWN');
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
      const complexOps = ['IfStatement', 'DoWhile', 'DoUntil', 'ForLoop', 'WhenClause', 'SelectStatement'];
      if (complexOps.includes(n.type)) {
        complexity++;
      }
      if (n.type === 'CalculationSpec' && n.metadata?.['opcode']) {
        const opcode = (n.metadata['opcode'] as string).toUpperCase();
        if (['IF', 'DOW', 'DOU', 'FOR', 'WHEN', 'CAS'].includes(opcode)) {
          complexity++;
        }
      }
      n.children?.forEach(visit);
    };
    
    visit(node);
    return complexity;
  }
}

export { RPGParser as default };
