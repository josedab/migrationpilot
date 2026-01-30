# analyze

Analyze legacy code to extract structure, data definitions, and business rules.

## Usage

```bash
migrationpilot analyze <path> [options]
```

## Arguments

| Argument | Description |
|----------|-------------|
| `path` | Path to file or directory to analyze |

## Options

| Option | Description | Default |
|--------|-------------|---------|
| `-l, --language <lang>` | Source language (cobol, fortran, vb6, java-legacy) | `cobol` |
| `-o, --output <file>` | Output file path | - |
| `-f, --format <format>` | Output format (json, yaml, table) | `table` |
| `-r, --rules` | Extract business rules | `false` |
| `--verbose` | Show detailed output | `false` |

## Supported Languages

| Language | File Extensions |
|----------|-----------------|
| COBOL | `.cbl`, `.cob`, `.CBL`, `.COB`, `.cpy` |
| Fortran | `.f`, `.for`, `.f90`, `.f95`, `.F`, `.FOR` |
| Visual Basic 6 | `.bas`, `.cls`, `.frm` |
| Legacy Java | `.java` |

## Examples

### Analyze a directory

```bash
migrationpilot analyze ./legacy-code --language cobol
```

### Output results to JSON

```bash
migrationpilot analyze ./src --language fortran --output analysis.json --format json
```

### Extract business rules

```bash
migrationpilot analyze ./cobol-src --language cobol --rules --verbose
```

### Analyze with detailed output

```bash
migrationpilot analyze ./banking-system --language cobol --verbose
```

## Output

The analyze command produces:

### Summary Table

```
📊 Analysis Summary
──────────────────────────────────────────────────
┌────────────────┬───────┐
│ Metric         │ Count │
├────────────────┼───────┤
│ Files          │ 42    │
│ Data Structures│ 156   │
│ Procedures     │ 287   │
│ Errors         │ 3     │
│ Warnings       │ 12    │
└────────────────┴───────┘
```

### JSON Output (with `--output`)

```json
{
  "summary": {
    "files": 42,
    "dataStructures": 156,
    "procedures": 287,
    "errors": 3,
    "warnings": 12
  },
  "results": [
    {
      "file": "/path/to/file.cbl",
      "dataStructures": [...],
      "procedures": [...],
      "errors": [],
      "warnings": []
    }
  ]
}
```

## Notes

- The `--rules` flag provides a preview of detected business rules
- Full AI-powered rule extraction requires running a migration via `migrationpilot migrate`
- Use `--verbose` to see complexity metrics for each procedure
