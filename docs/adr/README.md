# Architecture Decision Records

This directory contains Architecture Decision Records (ADRs) for MigrationPilot.

## What is an ADR?

An Architecture Decision Record captures an important architectural decision made along with its context and consequences.

## ADR Index

| ADR | Title | Status |
|-----|-------|--------|
| [001](001-multi-agent-architecture.md) | Multi-Agent AI Architecture | Accepted |
| [002](002-parser-strategy.md) | Parser Implementation Strategy | Accepted |
| [003](003-typescript-monorepo.md) | TypeScript Monorepo Structure | Accepted |

## Template

When creating a new ADR, use the following template:

```markdown
# ADR-NNN: Title

## Status

[Proposed | Accepted | Deprecated | Superseded]

## Context

What is the issue that we're seeing that is motivating this decision?

## Decision

What is the change that we're proposing and/or doing?

## Consequences

What becomes easier or more difficult to do because of this change?
```

## Contributing

When making significant architectural decisions:

1. Copy the template
2. Number it sequentially
3. Fill in all sections
4. Submit as part of your PR
5. Update the index above
