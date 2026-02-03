---
sidebar_position: 50
---

# Contributing

Thank you for your interest in contributing to MigrationPilot! This guide will help you get started.

## Ways to Contribute

### Code Contributions

- **Bug fixes**: Found a bug? PRs welcome!
- **Features**: Check the [roadmap](https://github.com/migrationpilot/migrationpilot/issues?q=is%3Aissue+is%3Aopen+label%3Aenhancement) for planned features
- **Parser improvements**: Help improve legacy language parsing accuracy
- **New language support**: Add parsers for additional legacy languages
- **Target generators**: Improve code generation for modern languages

### Non-Code Contributions

- **Documentation**: Fix typos, add examples, improve clarity
- **Bug reports**: Detailed bug reports help us fix issues faster
- **Feature requests**: Share ideas for improving MigrationPilot
- **Testing**: Try MigrationPilot with your legacy code and report issues
- **Translations**: Help translate documentation

## Development Setup

### Prerequisites

- **Node.js 20+**: [Download](https://nodejs.org/)
- **pnpm 8+**: Install with `npm install -g pnpm`
- **Docker**: For local infrastructure
- **Git**: For version control

### Getting Started

```bash
# 1. Fork the repository on GitHub

# 2. Clone your fork
git clone https://github.com/YOUR_USERNAME/migrationpilot.git
cd migrationpilot

# 3. Add upstream remote
git remote add upstream https://github.com/migrationpilot/migrationpilot.git

# 4. Install dependencies
pnpm install

# 5. Copy environment file
cp .env.example .env

# 6. Start infrastructure (PostgreSQL, Redis, MinIO)
pnpm docker:up

# 7. Run database migrations
pnpm db:migrate

# 8. Start development servers
pnpm dev
```

### Project Structure

```
migrationpilot/
├── apps/
│   ├── api/         # Hono API server (port 3001)
│   ├── web/         # Next.js frontend (port 3000)
│   └── cli/         # Command-line tool
├── packages/
│   ├── agents/      # AI agents (Archeologist, Architect, Builder, Validator)
│   ├── parsers/     # Legacy language parsers
│   ├── generators/  # Target language code generators
│   ├── core/        # Shared types and utilities
│   └── ...          # Other packages
├── docs/
│   └── site/        # Docusaurus documentation
└── infrastructure/  # Docker, Terraform, Helm configs
```

### Useful Commands

```bash
# Development
pnpm dev                    # Start all dev servers
pnpm dev --filter api       # Start only API server
pnpm dev --filter web       # Start only web app

# Testing
pnpm test                   # Run all tests
pnpm test --filter agents   # Test specific package
pnpm test:watch             # Watch mode
pnpm test:coverage          # With coverage report

# Code Quality
pnpm lint                   # Run ESLint
pnpm lint:fix               # Auto-fix lint issues
pnpm typecheck              # TypeScript type checking
pnpm format                 # Format with Prettier

# Build
pnpm build                  # Build all packages
pnpm build --filter api     # Build specific package

# Database
pnpm db:migrate             # Run migrations
pnpm db:seed                # Seed test data
pnpm db:push                # Push schema changes
```

## Making Changes

### 1. Create a Branch

```bash
# Sync with upstream
git fetch upstream
git checkout main
git merge upstream/main

# Create feature branch
git checkout -b feat/your-feature-name
```

### 2. Make Your Changes

- Write code following existing patterns
- Add tests for new functionality
- Update documentation if needed
- Keep commits focused and atomic

### 3. Test Your Changes

```bash
# Run tests
pnpm test

# Check types
pnpm typecheck

# Check lint
pnpm lint

# Try a build
pnpm build
```

### 4. Commit Your Changes

We use [Conventional Commits](https://www.conventionalcommits.org/):

```bash
# Format: type(scope): description

# Examples:
git commit -m "feat(parsers): add RPG IV parser support"
git commit -m "fix(agents): handle null values in rule extraction"
git commit -m "docs: add IDE extension setup guide"
git commit -m "test(validators): add edge case tests"
git commit -m "refactor(core): simplify AST traversal"
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Formatting (no code change)
- `refactor`: Code change that neither fixes nor adds
- `perf`: Performance improvement
- `test`: Adding or fixing tests
- `chore`: Build process or auxiliary tools

### 5. Submit a Pull Request

```bash
# Push your branch
git push origin feat/your-feature-name
```

Then open a PR on GitHub with:
- Clear title following conventional commits
- Description of what changed and why
- Link to related issues
- Screenshots for UI changes
- Test instructions if applicable

## Code Guidelines

### TypeScript

```typescript
// Use explicit types for function parameters and returns
function analyzeCode(
  source: string,
  language: SourceLanguage
): Promise<AnalysisResult> {
  // ...
}

// Prefer interfaces over type aliases for objects
interface BusinessRule {
  id: string;
  name: string;
  confidence: number;
}

// Use const assertions for literal types
const SUPPORTED_LANGUAGES = ['cobol', 'fortran', 'vb6'] as const;
type Language = typeof SUPPORTED_LANGUAGES[number];
```

### Testing

```typescript
import { describe, it, expect, beforeEach } from 'vitest';

describe('ArcheologistAgent', () => {
  let agent: ArcheologistAgent;

  beforeEach(() => {
    agent = new ArcheologistAgent();
  });

  it('should extract business rules from COBOL', async () => {
    const result = await agent.analyze(context, cobolCode);

    expect(result.success).toBe(true);
    expect(result.data.rules).toHaveLength(3);
    expect(result.data.rules[0].confidence).toBeGreaterThan(0.8);
  });

  it('should handle empty input gracefully', async () => {
    const result = await agent.analyze(context, '');

    expect(result.success).toBe(true);
    expect(result.data.rules).toHaveLength(0);
  });
});
```

### Error Handling

```typescript
// Use typed errors
class ParserError extends Error {
  constructor(
    message: string,
    public readonly line: number,
    public readonly column: number
  ) {
    super(message);
    this.name = 'ParserError';
  }
}

// Return Result types instead of throwing
interface Result<T> {
  success: boolean;
  data?: T;
  error?: string;
}
```

## Documentation

### Writing Docs

- Use clear, concise language
- Include code examples for every feature
- Test code examples to ensure they work
- Add cross-references to related topics

### Running Docs Locally

```bash
cd docs/site
pnpm install
pnpm dev
# Opens at http://localhost:3000
```

## Getting Help

### Resources

- **GitHub Issues**: [Report bugs or request features](https://github.com/migrationpilot/migrationpilot/issues)
- **Discussions**: [Ask questions and share ideas](https://github.com/migrationpilot/migrationpilot/discussions)
- **Discord**: [Join the community](https://discord.gg/migrationpilot)

### Good First Issues

New to the project? Look for issues labeled [`good first issue`](https://github.com/migrationpilot/migrationpilot/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).

### Questions?

- For bugs: Open a [GitHub issue](https://github.com/migrationpilot/migrationpilot/issues/new?template=bug_report.md)
- For questions: Start a [Discussion](https://github.com/migrationpilot/migrationpilot/discussions/new)
- For chat: Join [Discord](https://discord.gg/migrationpilot)

## Recognition

Contributors are recognized in:
- The project README
- Release notes
- The contributors page on our website

Thank you for helping make MigrationPilot better!
