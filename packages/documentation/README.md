# @migrationpilot/documentation

AI-Powered documentation generation for MigrationPilot.

## Overview

This package generates comprehensive documentation from migration artifacts, including API docs, architecture diagrams, business rule catalogs, and migration guides.

## Features

- **API Documentation**: OpenAPI specs from generated code
- **Architecture Docs**: System diagrams and descriptions
- **Business Rule Catalog**: Searchable rule documentation
- **Migration Guides**: Step-by-step migration instructions
- **Multi-Format Export**: Markdown, HTML, PDF, Confluence

## Usage

```typescript
import { DocumentationGenerator } from '@migrationpilot/documentation';

const generator = new DocumentationGenerator();

// Generate all documentation
const docs = await generator.generate({
  project,
  analysis,
  architecture,
  generatedCode,
});

// Export to different formats
await docs.exportMarkdown('./docs');
await docs.exportHTML('./site');
await docs.exportPDF('./migration-guide.pdf');
await docs.exportConfluence({
  spaceKey: 'MIGRATION',
  parentPageId: '12345',
});
```

### Templates

Use custom templates for documentation:

```typescript
import { DocumentationGenerator, Template } from '@migrationpilot/documentation';

const generator = new DocumentationGenerator({
  templates: {
    businessRule: myCustomRuleTemplate,
    apiEndpoint: myCustomApiTemplate,
  },
});
```

## Generated Documentation

### Business Rule Catalog

```markdown
# Business Rules

## BR-001: Calculate Overtime Pay

**Category**: Calculation
**Confidence**: 92%

### Description
Calculates overtime pay for hours worked over 40 in a week.

### Logic
If hours > 40, overtime = (hours - 40) * rate * 1.5

### Inputs
- hoursWorked (decimal)
- hourlyRate (decimal)

### Outputs
- overtimePay (decimal)

### Source Reference
PAYROLL.cbl, lines 150-165
```

### API Reference

Generates OpenAPI-compliant documentation with request/response examples.

### Architecture Diagrams

Generates Mermaid diagrams for:
- Service architecture
- Data flow
- Component dependencies
- Database schema

## Installation

```bash
pnpm add @migrationpilot/documentation
```

## License

MIT
