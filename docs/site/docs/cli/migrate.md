# migrate

Start or continue a migration project using AI-powered agents.

## Usage

```bash
migrationpilot migrate [options]
```

## Options

| Option | Description | Default |
|--------|-------------|---------|
| `-p, --project <name>` | Project name (creates new if not exists) | - |
| `-s, --source <path>` | Source code path | - |
| `-t, --target <lang>` | Target language (java, python, typescript, go, csharp) | - |
| `-l, --language <lang>` | Source language (cobol, fortran, vb6, java-legacy) | - |
| `--pattern <pattern>` | Migration pattern (microservices, modular-monolith, strangler-fig) | - |
| `-o, --output <path>` | Output directory for generated code | `./migrated` |
| `--dry-run` | Preview migration without making changes | `false` |
| `-i, --interactive` | Interactive mode for rule review | `false` |

## Interactive Mode

Running `migrationpilot migrate` without options launches an interactive wizard that guides you through:

1. **Project name** - Name for the migration project
2. **Source path** - Location of legacy source code
3. **Source language** - COBOL, Fortran, VB6, or Legacy Java
4. **Target language** - Java, Python, TypeScript, Go, or C#
5. **Migration pattern** - Architecture approach for the modern code
6. **Output directory** - Where to write generated code
7. **Interactive review** - Whether to pause for human review of business rules

## Migration Patterns

| Pattern | Description |
|---------|-------------|
| `microservices` | Generate separate microservices for each bounded context |
| `modular-monolith` | Generate a modular monolith with clear boundaries |
| `strangler-fig` | Generate facade layer for incremental migration |

## Examples

### Interactive migration

```bash
migrationpilot migrate -i
```

### Full command-line migration

```bash
migrationpilot migrate \
  --project banking-modernization \
  --source ./cobol-src \
  --language cobol \
  --target java \
  --pattern microservices \
  --output ./modern-banking
```

### Dry run to preview

```bash
migrationpilot migrate \
  --project test-migration \
  --source ./legacy \
  --language cobol \
  --target python \
  --dry-run
```

## Migration Phases

The migration process runs through four phases:

### Phase 1: Analysis
- Parses legacy source code
- Extracts data structures and procedures
- Identifies business rules with confidence scores

### Phase 2: Architecture Design
- Designs modern architecture based on analysis
- Defines service boundaries
- Creates API specifications
- Generates database schemas

### Phase 3: Code Generation
- Generates idiomatic modern code
- Creates unit tests
- Produces configuration files
- Writes documentation

### Phase 4: Validation
- Generates equivalence test cases
- Validates business logic preservation
- Calculates confidence scores

## Output

```
🚀 MigrationPilot Migration
──────────────────────────────────────────────────

📋 Migration Plan
──────────────────────────────────────────────────
  Project: banking-modernization
  Source: ./cobol-src
  Source Language: cobol
  Target Language: java
  Pattern: microservices
  Output: ./modern-banking

🔄 Migration Progress
──────────────────────────────────────────────────
✓ Phase 1: Analysis complete
✓ Phase 2: Architecture design complete
✓ Phase 3: Code generation complete
✓ Phase 4: Validation complete

✅ Migration completed successfully!

  Next steps:
  1. Review generated code in ./modern-banking
  2. Run: migrationpilot validate --project banking-modernization
  3. Check business rules: migrationpilot rules list --project banking-modernization
```

## Notes

- Use `--interactive` for the first migration to understand the options
- The `--dry-run` flag is useful for validating your configuration
- Review generated business rules with `migrationpilot rules list --pending`
