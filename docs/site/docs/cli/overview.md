# CLI Overview

The MigrationPilot CLI provides a powerful command-line interface for legacy code modernization. It allows you to analyze legacy code, run migrations, validate equivalence, and manage business rules—all from your terminal.

## Installation

```bash
# Install globally via npm
npm install -g @migrationpilot/cli

# Or run directly with npx
npx @migrationpilot/cli
```

## Quick Start

```bash
# Initialize configuration
migrationpilot config init

# Analyze legacy code
migrationpilot analyze ./legacy-code --language cobol

# Start a migration
migrationpilot migrate --project my-migration --target java

# Validate equivalence
migrationpilot validate --project my-migration
```

## Global Options

| Option | Description |
|--------|-------------|
| `-v, --verbose` | Enable verbose output |
| `-q, --quiet` | Suppress all output except errors |
| `--api-url <url>` | MigrationPilot API URL (default: http://localhost:3001) |
| `--api-key <key>` | MigrationPilot API Key |
| `-V, --version` | Output the version number |
| `-h, --help` | Display help for command |

## Available Commands

| Command | Description |
|---------|-------------|
| [`analyze`](./analyze) | Analyze legacy code to extract structure and business rules |
| [`migrate`](./migrate) | Start or continue a migration project |
| [`validate`](./validate) | Run equivalence validation tests |
| [`project`](./project) | Manage migration projects |
| [`rules`](./rules) | Manage extracted business rules |
| [`config`](./config) | Manage CLI configuration |

## Environment Variables

The CLI respects the following environment variables:

| Variable | Description |
|----------|-------------|
| `MIGRATIONPILOT_API_URL` | API server URL |
| `MIGRATIONPILOT_API_KEY` | API authentication key |

## Configuration File

The CLI stores configuration in `~/.migrationpilot/config.json`. Use `migrationpilot config init` to set up your configuration interactively.

## Exit Codes

| Code | Description |
|------|-------------|
| `0` | Success |
| `1` | General error |

## Examples

### Analyze a COBOL codebase

```bash
migrationpilot analyze ./cobol-src --language cobol --output analysis.json
```

### Run an interactive migration

```bash
migrationpilot migrate -i
```

### Export business rules

```bash
migrationpilot rules export --project my-project --format json -o rules.json
```

### Check validation results

```bash
migrationpilot validate --project my-project --verbose --report validation-report.json
```
