# ADR-002: Parser Implementation Strategy

## Status

Accepted

## Context

Legacy languages (COBOL, Fortran, VB6) have complex grammars with many dialects:

- **COBOL**: COBOL-85, COBOL-2002, IBM Enterprise COBOL, Micro Focus, GnuCOBOL
- **Fortran**: F77, F90, F95 with vendor extensions
- **VB6/VBA**: Different hosts (Excel, Access, Word) with varying features

Options considered:

1. **ANTLR grammars**: Full parsing with grammar files
2. **Tree-sitter**: Fast incremental parsing
3. **Regex-based**: Simple pattern matching
4. **AI-assisted**: Use LLM for parsing
5. **Hybrid**: Combine structural parsing with AI

## Decision

We will use a **hybrid approach**:

1. **Structural parsing** using TypeScript-based parsers that:
   - Identify program structure (divisions, sections, procedures)
   - Extract data definitions with type information
   - Parse control flow statements
   - Handle common dialects

2. **AI-assisted semantic analysis** for:
   - Business rule extraction
   - Intent understanding
   - Edge case identification
   - Comment/documentation analysis

The parsers produce an **Abstract Syntax Tree (AST)** with a common structure across languages, enabling:
- Unified downstream processing
- Language-agnostic agent prompts
- Consistent data models

## Consequences

### Positive

- **Performance**: Structural parsing is fast and deterministic
- **Accuracy**: Known patterns are reliably extracted
- **Extensibility**: New dialects can be added incrementally
- **Debuggability**: Parse errors are clear and actionable
- **Cost efficiency**: AI is used only where it adds value

### Negative

- **Maintenance**: Parsers need updates for new dialects
- **Incomplete coverage**: Edge cases may not parse correctly
- **Two systems**: Must keep structural and AI analysis in sync

### Mitigations

- Comprehensive test suites for each parser
- Fallback to AI for unparseable sections
- Dialect detection heuristics
- User feedback loop for parser improvements
