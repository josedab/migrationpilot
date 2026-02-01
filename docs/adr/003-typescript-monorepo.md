# ADR-003: TypeScript Monorepo Structure

## Status

Accepted

## Context

MigrationPilot consists of multiple components:

- **Apps**: API server, web dashboard, CLI
- **Packages**: Agents, parsers, generators, core utilities
- **Infrastructure**: Docker, Kubernetes, Terraform

Options considered:

1. **Polyrepo**: Separate repositories per component
2. **Monorepo with npm workspaces**: Native npm/yarn/pnpm workspaces
3. **Monorepo with Nx**: Enterprise monorepo tooling
4. **Monorepo with Turborepo**: Vercel's build system

## Decision

We will use **Turborepo with pnpm workspaces**:

```
migrationpilot/
├── apps/
│   ├── api/          # Hono API server
│   ├── web/          # Next.js frontend
│   └── cli/          # Commander-based CLI
├── packages/
│   ├── agents/       # AI agents
│   ├── parsers/      # Language parsers
│   ├── generators/   # Code generators
│   ├── core/         # Shared types and utilities
│   ├── database/     # Drizzle ORM schema
│   └── ui/           # Shared UI components
└── infrastructure/   # Deployment configs
```

Key choices:

- **pnpm**: Fast, disk-efficient package management
- **Turborepo**: Smart caching, parallel builds, task dependencies
- **TypeScript**: End-to-end type safety with shared types
- **Workspace protocols**: `workspace:*` for internal dependencies

## Consequences

### Positive

- **Atomic changes**: Update API and types together
- **Shared code**: Core utilities used everywhere
- **Type safety**: TypeScript types flow across packages
- **Fast builds**: Turborepo caching speeds development
- **Single toolchain**: One tsconfig, eslint, prettier setup

### Negative

- **Initial complexity**: Monorepo setup has learning curve
- **CI complexity**: Need smart caching in CI
- **Large repo**: Clone size grows over time

### Mitigations

- Comprehensive `turbo.json` configuration
- GitHub Actions with Turborepo remote caching
- Clear package boundaries and dependencies
- Documentation of workspace commands
