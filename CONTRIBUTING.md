# Contributing to MigrationPilot

Thank you for your interest in contributing to MigrationPilot! This document provides guidelines for contributing.

## Development Setup

1. Fork and clone the repository
2. Install dependencies: `pnpm install`
3. Copy `.env.example` to `.env` and configure
4. Start infrastructure: `docker compose -f infrastructure/docker/docker-compose.yml up -d`
5. Run development servers: `pnpm dev`

## Code Style

- Use TypeScript for all new code
- Follow existing patterns in the codebase
- Run `pnpm lint` before committing
- Run `pnpm typecheck` to verify types

## Pull Request Process

1. Create a feature branch from `main`
2. Make your changes with clear commit messages
3. Add tests for new functionality
4. Update documentation if needed
5. Submit a PR with a clear description

## Commit Messages

Follow conventional commits:
- `feat:` New features
- `fix:` Bug fixes
- `docs:` Documentation changes
- `refactor:` Code refactoring
- `test:` Test additions/changes
- `chore:` Build/tooling changes

## Questions?

Open a GitHub issue or reach out on Discord.
