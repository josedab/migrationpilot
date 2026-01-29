/**
 * Base Parser Interface
 * 
 * Common interface for all language parsers
 */

import type { ASTNode, DataStructure, Procedure, SourceLocation } from '@migrationpilot/core';

export interface ParseResult {
  success: boolean;
  ast?: ASTNode;
  dataStructures: DataStructure[];
  procedures: Procedure[];
  errors: ParseError[];
  warnings: ParseWarning[];
  metadata: ParseMetadata;
}

export interface ParseError {
  message: string;
  location: SourceLocation;
  code: string;
  severity: 'error' | 'fatal';
}

export interface ParseWarning {
  message: string;
  location: SourceLocation;
  code: string;
}

export interface ParseMetadata {
  language: string;
  dialect?: string;
  sourceFile: string;
  lines: number;
  parseTime: number;
  version: string;
}

export interface ParserOptions {
  dialect?: string;
  strictMode?: boolean;
  includeComments?: boolean;
  resolveCopybooks?: boolean;
  copybookPaths?: string[];
}

export abstract class BaseParser {
  protected options: ParserOptions;

  constructor(options: ParserOptions = {}) {
    this.options = options;
  }

  /**
   * Parse source code
   */
  abstract parse(source: string, filename?: string): ParseResult;

  /**
   * Get supported dialects
   */
  abstract getSupportedDialects(): string[];

  /**
   * Check if a dialect is supported
   */
  isDialectSupported(dialect: string): boolean {
    return this.getSupportedDialects().includes(dialect.toLowerCase());
  }

  /**
   * Create a source location
   */
  protected createLocation(
    filename: string,
    startLine: number,
    endLine: number,
    startColumn?: number,
    endColumn?: number
  ): SourceLocation {
    return {
      file: filename,
      startLine,
      endLine,
      startColumn,
      endColumn,
    };
  }

  /**
   * Create a parse error
   */
  protected createError(
    message: string,
    location: SourceLocation,
    code: string,
    severity: 'error' | 'fatal' = 'error'
  ): ParseError {
    return { message, location, code, severity };
  }

  /**
   * Create a parse warning
   */
  protected createWarning(
    message: string,
    location: SourceLocation,
    code: string
  ): ParseWarning {
    return { message, location, code };
  }
}
