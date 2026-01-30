# project

Manage migration projects from the command line.

## Usage

```bash
migrationpilot project <command> [options]
```

## Subcommands

### list

List all migration projects.

```bash
migrationpilot project list [options]
```

| Option | Description |
|--------|-------------|
| `-s, --status <status>` | Filter by status |

**Example:**

```bash
# List all projects
migrationpilot project list

# List only in-progress projects
migrationpilot project list --status in-progress
```

**Output:**

```
📁 Migration Projects
────────────────────────────────────────────────────────────
┌─────────────────┬─────────┬────────────┬─────────────┬──────────┬────────────┐
│ Name            │ Source  │ Target     │ Status      │ Progress │ Updated    │
├─────────────────┼─────────┼────────────┼─────────────┼──────────┼────────────┤
│ cobol-banking   │ COBOL   │ Java       │ in-progress │ 65%      │ 2024-01-28 │
│ fortran-calc    │ Fortran │ Python     │ analysis    │ 25%      │ 2024-01-27 │
│ vb6-inventory   │ VB6     │ TypeScript │ complete    │ 100%     │ 2024-01-20 │
└─────────────────┴─────────┴────────────┴─────────────┴──────────┴────────────┘
```

---

### create

Create a new migration project.

```bash
migrationpilot project create <name> [options]
```

| Option | Description |
|--------|-------------|
| `-l, --language <lang>` | Source language |
| `-t, --target <lang>` | Target language |
| `-d, --description <text>` | Project description |

**Example:**

```bash
migrationpilot project create banking-migration \
  --language cobol \
  --target java \
  --description "Modernize core banking COBOL system"
```

---

### show

Show detailed project information.

```bash
migrationpilot project show <name>
```

**Example:**

```bash
migrationpilot project show cobol-banking
```

**Output:**

```
📁 Project: cobol-banking
──────────────────────────────────────────────────
  Status: in-progress
  Source Language: COBOL
  Target Language: Java
  Created: 2024-01-15
  Updated: 2024-01-28

  Progress
    ✓ Analysis: Complete
    ✓ Architecture Design: Complete
    ⋯ Code Generation: 65% (45/70 modules)
    ○ Validation: Pending

  Statistics
    Files: 70
    Lines of Code: 125,000
    Business Rules: 342
    Data Structures: 89
```

---

### delete

Delete a migration project.

```bash
migrationpilot project delete <name> [options]
```

| Option | Description |
|--------|-------------|
| `-f, --force` | Skip confirmation prompt |

**Example:**

```bash
# With confirmation
migrationpilot project delete old-project

# Skip confirmation
migrationpilot project delete old-project --force
```

---

### export

Export project data to a file.

```bash
migrationpilot project export <name> [options]
```

| Option | Description | Default |
|--------|-------------|---------|
| `-o, --output <file>` | Output file | `project-export.json` |

**Example:**

```bash
migrationpilot project export banking-migration -o banking-backup.json
```

## Project Status

Projects can be in one of the following states:

| Status | Description |
|--------|-------------|
| `draft` | Project created but not started |
| `analysis` | Code analysis in progress |
| `in-progress` | Migration actively running |
| `validation` | Equivalence testing in progress |
| `complete` | Migration finished successfully |
