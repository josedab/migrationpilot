# Parsers Architecture

MigrationPilot includes custom parsers for legacy languages that produce a unified Abstract Syntax Tree (AST) representation. This enables consistent analysis across different source languages.

## Supported Languages

| Language | File Extensions | Dialects |
|----------|-----------------|----------|
| COBOL | `.cbl`, `.cob`, `.cpy` | COBOL-85, IBM Enterprise, Micro Focus, GnuCOBOL |
| Fortran | `.f`, `.for`, `.f90`, `.f95` | F77, F90, F95 |
| Visual Basic 6 | `.bas`, `.cls`, `.frm` | VB6, VBA |
| Legacy Java | `.java` | J2EE, EJB 2.x, Struts 1.x |

## Parser Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Source Code                             │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                     Lexer/Tokenizer                          │
│  Converts source text into tokens                           │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                    Language Parser                           │
│  COBOL | Fortran | VB6 | Java                               │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                    Unified AST                               │
│  Common representation across all languages                  │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                    Semantic Analyzer                         │
│  Type inference, scope resolution, flow analysis            │
└─────────────────────────────────────────────────────────────┘
```

## Unified AST

All parsers produce a common AST structure:

```typescript
interface ParseResult {
  // Metadata
  language: SourceLanguage;
  filename: string;
  
  // Structure
  dataStructures: DataStructure[];
  procedures: Procedure[];
  
  // Diagnostics
  errors: ParseError[];
  warnings: ParseWarning[];
}

interface DataStructure {
  name: string;
  description?: string;
  fields: Field[];
  sourceLocation: SourceLocation;
}

interface Procedure {
  name: string;
  type: 'function' | 'subroutine' | 'method' | 'paragraph';
  parameters: Parameter[];
  returnType?: string;
  localVariables: Variable[];
  statements: Statement[];
  complexity: number;
  sourceLocation: SourceLocation;
}
```

## COBOL Parser

Handles COBOL-specific constructs:

### Data Division

```cobol
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID      PIC 9(8).
   05 CUSTOMER-NAME    PIC X(50).
   05 ACCOUNT-BALANCE  PIC S9(10)V99.
```

Parsed as:

```json
{
  "name": "CUSTOMER-RECORD",
  "fields": [
    { "name": "CUSTOMER-ID", "type": "PIC 9(8)", "level": 5 },
    { "name": "CUSTOMER-NAME", "type": "PIC X(50)", "level": 5 },
    { "name": "ACCOUNT-BALANCE", "type": "PIC S9(10)V99", "level": 5 }
  ]
}
```

### Procedure Division

```cobol
CALCULATE-INTEREST.
    COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE / 100.
    IF WS-INTEREST > WS-MAX-INTEREST
        MOVE WS-MAX-INTEREST TO WS-INTEREST
    END-IF.
```

Parsed as:

```json
{
  "name": "CALCULATE-INTEREST",
  "type": "paragraph",
  "statements": [
    {
      "type": "compute",
      "target": "WS-INTEREST",
      "expression": "WS-PRINCIPAL * WS-RATE / 100"
    },
    {
      "type": "if",
      "condition": "WS-INTEREST > WS-MAX-INTEREST",
      "thenStatements": [
        { "type": "move", "from": "WS-MAX-INTEREST", "to": "WS-INTEREST" }
      ]
    }
  ],
  "complexity": 2
}
```

### COBOL-Specific Features

- **COPY statements** - Include file resolution
- **REDEFINES** - Overlapping memory structures
- **OCCURS** - Array/table definitions
- **88-level items** - Condition names
- **PERFORM THRU** - Multi-paragraph execution

## Fortran Parser

Handles Fortran-specific constructs:

### Program Structure

```fortran
SUBROUTINE SIMPSON(F, A, B, N, RESULT)
    IMPLICIT NONE
    REAL*8 F, A, B, RESULT
    INTEGER N
    EXTERNAL F
    
    ! Implementation
END SUBROUTINE
```

### Fortran-Specific Features

- **COMMON blocks** - Shared memory areas
- **EQUIVALENCE** - Memory aliasing
- **Implicit typing** - I-N integer rule
- **Array sections** - Fortran 90+ slicing
- **DO loops** - Various forms (labeled, DO WHILE)

## VB6 Parser

Handles Visual Basic 6 constructs:

### Class Modules

```vb
Private mValue As Long

Public Property Get Value() As Long
    Value = mValue
End Property

Public Property Let Value(v As Long)
    mValue = v
End Property

Public Function Calculate() As Double
    Calculate = mValue * 1.1
End Function
```

### VB6-Specific Features

- **Property procedures** - Get/Let/Set
- **Events** - RaiseEvent, WithEvents
- **Error handling** - On Error GoTo
- **Late binding** - CreateObject, GetObject
- **Form controls** - UI element parsing

## Legacy Java Parser

Handles J2EE-era Java patterns:

### EJB 2.x

```java
public interface AccountHome extends EJBHome {
    Account create(String id) throws CreateException;
    Account findByPrimaryKey(String id) throws FinderException;
}

public abstract class AccountBean implements EntityBean {
    public abstract String getId();
    public abstract void setId(String id);
    public abstract BigDecimal getBalance();
}
```

### Legacy Java Features

- **EJB 2.x** - Entity/Session beans, Home interfaces
- **Struts 1.x** - Actions, ActionForms
- **JNDI lookups** - Resource location
- **XML configuration** - ejb-jar.xml, struts-config.xml

## Using Parsers

### Programmatic API

```typescript
import { createParser } from '@migrationpilot/parsers';

const parser = createParser('cobol');
const result = parser.parse(sourceCode, 'PROGRAM.cbl');

console.log(`Found ${result.procedures.length} procedures`);
console.log(`Found ${result.dataStructures.length} data structures`);
```

### CLI

```bash
migrationpilot analyze ./src --language cobol --output analysis.json
```

## Error Handling

Parsers provide detailed error information:

```typescript
interface ParseError {
  message: string;
  severity: 'error' | 'warning';
  location: {
    line: number;
    column: number;
    length: number;
  };
  code: string;  // Error code for documentation lookup
}
```

Example output:

```json
{
  "errors": [
    {
      "message": "Undefined variable: WS-UNDEFINED",
      "severity": "error",
      "location": { "line": 45, "column": 12, "length": 12 },
      "code": "COBOL001"
    }
  ],
  "warnings": [
    {
      "message": "GOTO statement detected - consider restructuring",
      "severity": "warning",
      "location": { "line": 78, "column": 8, "length": 4 },
      "code": "COBOL042"
    }
  ]
}
```

## Extending Parsers

To add support for a new language:

1. **Implement the Parser interface**:

```typescript
export class NewLanguageParser implements Parser {
  parse(content: string, filename: string): ParseResult {
    // Tokenize
    const tokens = this.tokenize(content);
    
    // Parse
    const ast = this.buildAST(tokens);
    
    // Convert to unified format
    return this.toUnifiedAST(ast);
  }
}
```

2. **Register the parser**:

```typescript
registerParser('new-language', NewLanguageParser);
```

## Related Topics

- [AI Agents](./agents.md) - How agents use parsed data
- [Business Rules](../concepts/business-rules.md) - Rule extraction from AST
- [Analyze Command](../cli/analyze.md) - CLI parser usage
