# @migrationpilot/marketplace

Multi-tenant migration marketplace with templates, providers, and monetization.

## Overview

This package provides a marketplace ecosystem for sharing and monetizing migration templates, custom parsers, generators, and expert services.

## Features

- **Template Library**: Pre-built migration templates
- **Provider Ecosystem**: Third-party migration services
- **Custom Components**: Parsers, generators, validators
- **Multi-Tenancy**: Organization-level isolation
- **Monetization**: Revenue sharing for contributors

## Usage

### Browse Templates

```typescript
import { Marketplace } from '@migrationpilot/marketplace';

const marketplace = new Marketplace();

// Search templates
const templates = await marketplace.search({
  sourceLanguage: 'cobol',
  targetLanguage: 'java',
  category: 'banking',
});

// Get template details
const template = await marketplace.getTemplate('banking-cobol-spring');

// Use template
await marketplace.applyTemplate(project, template);
```

### Publish Components

```typescript
// Publish a custom parser
await marketplace.publish({
  type: 'parser',
  name: 'cobol-cics-parser',
  description: 'CICS COBOL parser with transaction support',
  package: '@myorg/cobol-cics-parser',
  pricing: { type: 'free' }, // or { type: 'paid', price: 99 }
});

// Publish a template
await marketplace.publish({
  type: 'template',
  name: 'insurance-cobol-to-java',
  description: 'Insurance industry COBOL to Spring Boot migration',
  includes: ['parsers', 'generators', 'test-data'],
  pricing: { type: 'subscription', monthly: 199 },
});
```

### Provider Services

```typescript
// Register as a provider
await marketplace.registerProvider({
  name: 'Expert Migration Services',
  services: ['consulting', 'code-review', 'custom-development'],
  rates: {
    consulting: { hourly: 250 },
    codeReview: { perFile: 50 },
  },
});

// Request provider services
await marketplace.requestService({
  provider: 'expert-migration-services',
  service: 'code-review',
  project: projectId,
});
```

## Template Structure

```yaml
name: banking-cobol-spring
version: 1.0.0
description: Banking COBOL to Spring Boot migration
author: MigrationPilot Team

source:
  language: cobol
  dialects: [ibm-enterprise, micro-focus]

target:
  language: java
  framework: spring-boot
  version: "3.2"

includes:
  - parsers/banking-cobol
  - generators/spring-banking
  - tests/banking-scenarios
  - data/sample-transactions

rules:
  - banking-calculations
  - transaction-handling
  - reporting-logic
```

## Installation

```bash
pnpm add @migrationpilot/marketplace
```

## License

MIT
