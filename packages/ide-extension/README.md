# @migrationpilot/ide-extension

IDE integration for VS Code and IntelliJ with legacy code analysis.

## Overview

This package provides IDE extensions that bring MigrationPilot capabilities directly into your development environment, enabling inline analysis, code navigation, and migration assistance.

## Features

- **Inline Analysis**: View business rules directly in legacy code
- **Code Navigation**: Jump between legacy and modern code
- **Rule Highlighting**: Visual indicators for extracted rules
- **Quick Actions**: Approve, reject, or edit rules from IDE
- **Progress Tracking**: Monitor migration status

## VS Code Extension

### Installation

```bash
# Install from VSIX
code --install-extension migrationpilot-vscode.vsix

# Or from marketplace (when published)
code --install-extension migrationpilot.migrationpilot
```

### Features

- **Syntax highlighting** for COBOL, Fortran, VB6
- **Hover information** showing extracted business rules
- **Code lens** for rule confidence and review status
- **Side-by-side view** of legacy vs modern code
- **Command palette** integration

### Commands

| Command | Description |
|---------|-------------|
| `MigrationPilot: Analyze File` | Analyze current file |
| `MigrationPilot: Show Rules` | Open rules panel |
| `MigrationPilot: Navigate to Modern` | Jump to generated code |
| `MigrationPilot: Approve Rule` | Approve highlighted rule |

## IntelliJ Extension

### Installation

Install from JetBrains Marketplace or from disk.

### Features

- **Legacy language support** via language server
- **Gutter icons** for business rules
- **Tool window** for migration status
- **Intentions** for rule actions

## Language Server

Both extensions use a common language server:

```typescript
import { MigrationLanguageServer } from '@migrationpilot/ide-extension';

const server = new MigrationLanguageServer({
  apiUrl: 'http://localhost:3001',
  apiKey: process.env.MIGRATIONPILOT_API_KEY,
});

server.start();
```

## Configuration

VS Code settings:

```json
{
  "migrationpilot.apiUrl": "http://localhost:3001",
  "migrationpilot.autoAnalyze": true,
  "migrationpilot.showConfidence": true,
  "migrationpilot.minConfidenceHighlight": 0.7
}
```

## Installation

```bash
pnpm add @migrationpilot/ide-extension
```

## License

MIT
