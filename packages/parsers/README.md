# @migrationpilot/parsers

Legacy language parsers for MigrationPilot.

## Overview

This package provides parsers for legacy programming languages, generating Abstract Syntax Trees (ASTs) and structured representations that can be analyzed by the AI agents.

## Supported Languages

| Language | Parser | Dialects |
|----------|--------|----------|
| COBOL | `cobol` | COBOL-85, IBM Enterprise, Micro Focus, GnuCOBOL |
| Fortran | `fortran` | F77, F90, F95 |
| Visual Basic | `vb6` | VB6, VBA |
| Legacy Java | `java-legacy` | J2EE, EJB 2.x, Struts 1.x |
| RPG | `rpg` | RPG II, RPG III, ILE RPG |
| PL/I | `pli` | Enterprise PL/I |
| Natural | `natural` | Software AG Natural |
| Assembler | `assembler` | IBM HLASM |
| CICS | `cics` | CICS command-level |

## Usage

### Basic Parsing

```typescript
import { parse, detectLanguage } from '@migrationpilot/parsers';

// Auto-detect language and parse
const result = parse(sourceCode, { filename: 'PAYROLL.cbl' });

// Or specify language explicitly
import { CobolParser } from '@migrationpilot/parsers/cobol';

const parser = new CobolParser();
const ast = parser.parse(sourceCode, {
  dialect: 'ibm-enterprise',
  copybooks: ['CUSTOMER.cpy', 'ACCOUNT.cpy'],
});
```

### COBOL Parser

```typescript
import { CobolParser } from '@migrationpilot/parsers/cobol';

const parser = new CobolParser();
const result = parser.parse(sourceCode, {
  dialect: 'cobol-85', // 'cobol-85' | 'ibm-enterprise' | 'micro-focus' | 'gnucobol'
  copybooks: [],        // Copybook contents to inline
  includeComments: true,
});

// Result includes:
// - divisions: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
// - dataStructures: WORKING-STORAGE, FILE SECTION items
// - procedures: paragraphs and sections
// - copyStatements: COPY references
```

### Fortran Parser

```typescript
import { FortranParser } from '@migrationpilot/parsers/fortran';

const parser = new FortranParser();
const result = parser.parse(sourceCode, {
  standard: 'f90', // 'f77' | 'f90' | 'f95'
  freeForm: true,
});

// Result includes:
// - modules and programs
// - subroutines and functions
// - common blocks
// - data types
```

### VB6 Parser

```typescript
import { Vb6Parser } from '@migrationpilot/parsers/vb6';

const parser = new Vb6Parser();
const result = parser.parse(sourceCode, {
  variant: 'vb6', // 'vb6' | 'vba'
});

// Result includes:
// - modules and classes
// - forms and controls
// - procedures and functions
// - COM references
```

## Parser Output

All parsers return a standardized `ParseResult`:

```typescript
interface ParseResult {
  ast: ASTNode;              // Abstract syntax tree
  dataStructures: DataStructure[];
  procedures: Procedure[];
  dependencies: Dependency[];
  errors: ParseError[];
  warnings: ParseWarning[];
  metadata: {
    language: string;
    dialect?: string;
    linesOfCode: number;
    commentLines: number;
  };
}
```

## Language Detection

```typescript
import { detectLanguage } from '@migrationpilot/parsers';

const language = detectLanguage(sourceCode, { filename: 'CALC.CBL' });
// Returns: { language: 'cobol', confidence: 0.95, dialect: 'cobol-85' }
```

Detection uses:
1. File extension
2. First-line patterns (e.g., `IDENTIFICATION DIVISION`)
3. Keyword frequency analysis
4. Structural patterns

## Error Handling

Parsers are fault-tolerant and will attempt to parse as much as possible:

```typescript
const result = parser.parse(invalidCode);

if (result.errors.length > 0) {
  for (const error of result.errors) {
    console.log(`${error.severity}: ${error.message} at line ${error.line}`);
  }
}

// Partial AST is still available
console.log(result.ast);
```

## Installation

```bash
pnpm add @migrationpilot/parsers
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Watch mode
pnpm dev
```

## Adding New Language Support

1. Create a new directory under `src/` (e.g., `src/ada/`)
2. Implement the `Parser` interface from `common/parser.ts`
3. Export from `src/index.ts`
4. Add tests in `src/__tests__/`

## License

MIT
