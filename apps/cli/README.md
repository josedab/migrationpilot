# @migrationpilot/cli

Command-line interface for MigrationPilot.

## Overview

The MigrationPilot CLI provides command-line access to all migration capabilities, enabling automation, CI/CD integration, and scripted workflows.

## Installation

```bash
# Install globally
npm install -g @migrationpilot/cli

# Or use with npx
npx @migrationpilot/cli analyze ./legacy-code
```

## Quick Start

```bash
# Configure API connection
migrationpilot config set api-url http://localhost:3001
migrationpilot config set api-key mp_live_abc123

# Analyze legacy code
migrationpilot analyze ./legacy-code --language cobol

# Create a project
migrationpilot project create "Payroll Migration" --source cobol --target java

# Start migration
migrationpilot migrate --project payroll-migration --dry-run

# Validate results
migrationpilot validate --project payroll-migration
```

## Commands

### `analyze`

Analyze legacy source code and extract business rules.

```bash
migrationpilot analyze <path> [options]

Options:
  -l, --language <lang>     Source language (cobol, fortran, vb6, java-legacy)
  -o, --output <path>       Output file for analysis results
  -f, --format <format>     Output format (json, yaml, markdown)
  --include <patterns>      File patterns to include
  --exclude <patterns>      File patterns to exclude
  --confidence <threshold>  Minimum confidence for rules (0-1)

Examples:
  migrationpilot analyze ./cobol-src --language cobol
  migrationpilot analyze ./src --language cobol --output analysis.json
  migrationpilot analyze . --include "*.cbl,*.cpy" --exclude "test/*"
```

### `project`

Manage migration projects.

```bash
migrationpilot project <command>

Commands:
  create <name>     Create a new project
  list              List all projects
  show <id>         Show project details
  delete <id>       Delete a project
  export <id>       Export project artifacts

Examples:
  migrationpilot project create "Payroll" --source cobol --target java
  migrationpilot project list --status active
  migrationpilot project show proj_abc123
  migrationpilot project export proj_abc123 --output ./export
```

### `migrate`

Run the migration pipeline.

```bash
migrationpilot migrate [options]

Options:
  -p, --project <id>        Project ID or name
  -t, --target <lang>       Target language (java, python, typescript, go, csharp)
  -f, --framework <name>    Target framework (spring-boot, fastapi, nestjs, etc.)
  --dry-run                 Preview migration without generating code
  --skip-review             Skip SME review step
  --confidence <threshold>  Auto-approve rules above threshold

Examples:
  migrationpilot migrate --project payroll --target java --framework spring-boot
  migrationpilot migrate --project payroll --dry-run
  migrationpilot migrate --project payroll --skip-review --confidence 0.95
```

### `validate`

Run equivalence validation tests.

```bash
migrationpilot validate [options]

Options:
  -p, --project <id>        Project ID or name
  --tests <path>            Path to test data
  --tolerance <value>       Comparison tolerance (default: 0.01)
  --report <path>           Output report path
  --format <format>         Report format (html, json, markdown)

Examples:
  migrationpilot validate --project payroll
  migrationpilot validate --project payroll --tests ./test-data.json
  migrationpilot validate --project payroll --report ./report.html --format html
```

### `rules`

Manage business rules.

```bash
migrationpilot rules <command>

Commands:
  list              List extracted rules
  show <id>         Show rule details
  approve <id>      Approve a rule
  reject <id>       Reject a rule
  export            Export rules

Examples:
  migrationpilot rules list --project payroll --status pending
  migrationpilot rules show BR-001
  migrationpilot rules approve BR-001 --comment "Verified with SME"
  migrationpilot rules export --project payroll --format markdown
```

### `trace`

Execution tracing commands.

```bash
migrationpilot trace <command>

Commands:
  capture           Capture execution trace
  compare           Compare traces
  visualize         Generate trace visualization

Examples:
  migrationpilot trace capture --legacy ./legacy-run --modern ./modern-run
  migrationpilot trace compare --baseline trace1.json --current trace2.json
  migrationpilot trace visualize trace.json --format mermaid
```

### `graph`

Knowledge graph operations.

```bash
migrationpilot graph <command>

Commands:
  build             Build knowledge graph from analysis
  query             Query the knowledge graph
  export            Export graph data

Examples:
  migrationpilot graph build --project payroll
  migrationpilot graph query "dependencies of CALC-PAY"
  migrationpilot graph export --format neo4j
```

### `oracle`

Test oracle commands.

```bash
migrationpilot oracle <command>

Commands:
  generate          Generate test oracle from rules
  validate          Validate outputs against oracle
  train             Train oracle from historical data

Examples:
  migrationpilot oracle generate --project payroll --rules BR-001,BR-002
  migrationpilot oracle validate --input test-data.json
```

### `config`

Manage CLI configuration.

```bash
migrationpilot config <command>

Commands:
  set <key> <value>  Set configuration value
  get <key>          Get configuration value
  list               List all configuration
  reset              Reset to defaults

Examples:
  migrationpilot config set api-url http://localhost:3001
  migrationpilot config set api-key mp_live_abc123
  migrationpilot config get api-url
  migrationpilot config list
```

## Configuration

### Configuration File

Configuration is stored in `~/.migrationpilot/config.yaml`:

```yaml
api:
  url: http://localhost:3001
  key: mp_live_abc123
  timeout: 30000

defaults:
  language: cobol
  target: java
  framework: spring-boot
  confidenceThreshold: 0.9

output:
  format: json
  color: true
  verbose: false
```

### Environment Variables

```bash
MIGRATIONPILOT_API_URL=http://localhost:3001
MIGRATIONPILOT_API_KEY=mp_live_abc123
MIGRATIONPILOT_DEFAULT_LANGUAGE=cobol
MIGRATIONPILOT_OUTPUT_FORMAT=json
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Migration Pipeline
on:
  push:
    branches: [main]

jobs:
  migrate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: '20'
      
      - name: Install CLI
        run: npm install -g @migrationpilot/cli
      
      - name: Configure CLI
        run: |
          migrationpilot config set api-url ${{ secrets.API_URL }}
          migrationpilot config set api-key ${{ secrets.API_KEY }}
      
      - name: Analyze Code
        run: migrationpilot analyze ./src --language cobol --output analysis.json
      
      - name: Run Migration
        run: migrationpilot migrate --project ${{ vars.PROJECT_ID }} --skip-review
      
      - name: Validate
        run: migrationpilot validate --project ${{ vars.PROJECT_ID }} --report report.html
      
      - name: Upload Report
        uses: actions/upload-artifact@v4
        with:
          name: migration-report
          path: report.html
```

### GitLab CI

```yaml
migrate:
  stage: build
  image: node:20
  script:
    - npm install -g @migrationpilot/cli
    - migrationpilot config set api-url $API_URL
    - migrationpilot config set api-key $API_KEY
    - migrationpilot analyze ./src --language cobol
    - migrationpilot migrate --project $PROJECT_ID
    - migrationpilot validate --project $PROJECT_ID
  artifacts:
    paths:
      - report.html
```

## Output Formats

### JSON

```bash
migrationpilot analyze ./src --format json
```

### YAML

```bash
migrationpilot rules list --format yaml
```

### Markdown

```bash
migrationpilot rules export --format markdown > rules.md
```

### Table (TTY)

```bash
migrationpilot project list
```

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | General error |
| 2 | Invalid arguments |
| 3 | Configuration error |
| 4 | API error |
| 5 | Validation failed |
| 6 | Migration failed |

## Development

```bash
# Build
pnpm build

# Run locally
pnpm dev analyze ./test-data

# Link globally
pnpm link --global

# Test
pnpm test
```

## License

MIT
