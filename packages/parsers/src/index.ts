/**
 * MigrationPilot Parsers Package
 * 
 * Legacy language parsers for COBOL, Fortran, VB6, and more
 */

// Common
export * from './common/index.js';

// Language-specific parsers
export { CobolParser, type CobolParserOptions, type CobolProgram } from './cobol/index.js';
export { FortranParser, type FortranParserOptions, type FortranProgram } from './fortran/index.js';
export { VB6Parser, type VB6ParserOptions, type VB6Module } from './vb6/index.js';
export { JavaLegacyParser, type JavaLegacyParserOptions, type JavaClass } from './java-legacy/index.js';

// New polyglot parsers
export { PLIParser, type PLIParserOptions } from './pli/index.js';
export { RPGParser, type RPGParserOptions } from './rpg/index.js';
export { NaturalParser, type NaturalParserOptions } from './natural/index.js';
export { AssemblerParser, type AssemblerParserOptions } from './assembler/index.js';
export { CICSParser, type CICSParserOptions } from './cics/index.js';

// Factory function
import type { SourceLanguage } from '@migrationpilot/core';
import { CobolParser, type CobolParserOptions } from './cobol/index.js';
import { FortranParser, type FortranParserOptions } from './fortran/index.js';
import { VB6Parser, type VB6ParserOptions } from './vb6/index.js';
import { JavaLegacyParser, type JavaLegacyParserOptions } from './java-legacy/index.js';
import { PLIParser, type PLIParserOptions } from './pli/index.js';
import { RPGParser, type RPGParserOptions } from './rpg/index.js';
import { NaturalParser, type NaturalParserOptions } from './natural/index.js';
import { AssemblerParser, type AssemblerParserOptions } from './assembler/index.js';
import { CICSParser, type CICSParserOptions } from './cics/index.js';
import type { BaseParser, ParserOptions } from './common/index.js';

// Extended source language type for polyglot support
export type ExtendedSourceLanguage = SourceLanguage | 'pli' | 'rpg' | 'natural' | 'assembler' | 'cics';

/**
 * Factory function to create a parser for the specified language
 * Options are cast to the appropriate type for each parser
 */
export function createParser(language: ExtendedSourceLanguage, options?: ParserOptions): BaseParser {
  switch (language) {
    case 'cobol':
      return new CobolParser(options as CobolParserOptions);
    case 'fortran':
      return new FortranParser(options as FortranParserOptions);
    case 'vb6':
    case 'vba':
      return new VB6Parser(options as VB6ParserOptions);
    case 'java-legacy':
      return new JavaLegacyParser(options as JavaLegacyParserOptions);
    case 'pli':
      return new PLIParser(options as PLIParserOptions);
    case 'rpg':
      return new RPGParser(options as RPGParserOptions);
    case 'natural':
      return new NaturalParser(options as NaturalParserOptions);
    case 'assembler':
      return new AssemblerParser(options as AssemblerParserOptions);
    case 'cics':
      return new CICSParser(options as CICSParserOptions);
    default:
      throw new Error(`Unsupported language: ${language}`);
  }
}
