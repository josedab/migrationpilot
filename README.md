# MigrationPilot

**AI-Powered Legacy Code Modernization Platform**

Transform your COBOL, Fortran, and Visual Basic systems into modern, maintainable code while preserving decades of business logic.

[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Node.js](https://img.shields.io/badge/Node.js-20%2B-green.svg)](https://nodejs.org/)
[![TypeScript](https://img.shields.io/badge/TypeScript-5.3-blue.svg)](https://www.typescriptlang.org/)
[![pnpm](https://img.shields.io/badge/pnpm-8.15-orange.svg)](https://pnpm.io/)

---

## Overview

MigrationPilot is an enterprise-grade platform that automates the modernization of legacy codebases using a multi-agent AI architecture. It extracts business rules from legacy code, designs modern architectures, generates clean idiomatic code, and validates behavioral equivalence—all while enabling human oversight of critical decisions.

## 🚀 Features

- **Multi-Agent AI Architecture**: Specialized agents for code analysis, architecture design, code generation, and validation
- **Legacy Language Support**: COBOL, Fortran, VB6/VBA, and legacy Java (J2EE, EJB 2.x)
- **Modern Target Languages**: Java, Python, TypeScript, Go, C#
- **Behavioral Equivalence Testing**: Automated validation that business logic is preserved
- **Business Rule Extraction**: AI-powered extraction and documentation of business rules
- **SME Collaboration**: Human-in-the-loop rule review with comments and corrections
- **Strangler Fig Support**: Incremental migration with traffic routing
- **Enterprise Ready**: On-premises deployment option, SOC 2 compliance, multi-tenancy

## 🏗️ Architecture

MigrationPilot uses a multi-agent architecture where specialized AI agents collaborate through an orchestrator:

```mermaid
flowchart TB
    subgraph Input["📥 Legacy Code Input"]
        COBOL[COBOL]
        Fortran[Fortran]
        VB6[VB6/VBA]
        LegacyJava[Legacy Java]
    end

    subgraph Parsers["🔍 Language Parsers"]
        Parser[("Parser Engine<br/>AST Generation")]
    end

    subgraph Agents["🤖 AI Agents"]
        direction TB
        Arch["🏺 Archeologist<br/>Business Rule Extraction"]
        Architect["📐 Architect<br/>Modern Design"]
        Builder["🔨 Builder<br/>Code Generation"]
        Validator["✅ Validator<br/>Equivalence Testing"]
    end

    subgraph Orchestrator["🎯 Migration Orchestrator"]
        Orch[("Pipeline<br/>Coordinator")]
    end

    subgraph Output["📤 Modern Output"]
        Java[Java/Spring]
        Python[Python/FastAPI]
        TS[TypeScript/NestJS]
        Go[Go/Gin]
        CSharp[C#/.NET]
    end

    subgraph Review["👥 Human Review"]
        SME["SME Collaboration<br/>Rule Validation"]
    end

    Input --> Parsers
    Parsers --> Orch
    Orch --> Arch
    Arch --> SME
    SME --> Architect
    Architect --> Builder
    Builder --> Validator
    Validator --> Output
    Validator -.->|"Discrepancies"| Builder
```

### Migration Pipeline

| Phase | Agent | Description |
|-------|-------|-------------|
| **1. Analysis** | Archeologist | Parses legacy code, extracts business rules with confidence scores |
| **2. Review** | Human (SME) | Reviews low-confidence rules, adds corrections and context |
| **3. Design** | Architect | Creates modern architecture with services, APIs, and data models |
| **4. Generation** | Builder | Produces clean, idiomatic code with full traceability |
| **5. Validation** | Validator | Runs equivalence tests, calculates confidence score |

## 📦 Project Structure

```
migrationpilot/
├── apps/
│   ├── web/              # Next.js frontend dashboard
│   ├── api/              # Hono API server
│   └── cli/              # Command-line tool
├── packages/
│   ├── agents/           # AI agents (Archeologist, Architect, Builder, Validator)
│   ├── parsers/          # Language parsers (COBOL, Fortran, VB6, RPG, PL/I, etc.)
│   ├── generators/       # Target language code generators
│   ├── core/             # Shared types, utilities, and knowledge graph
│   ├── database/         # Database schema (Drizzle ORM)
│   ├── collaboration/    # SME review and annotation features
│   ├── testing/          # Test generation and execution
│   ├── tracing/          # Observability and debugging
│   ├── rollout/          # Strangler Fig deployment support
│   ├── self-healing/     # Automatic issue detection and remediation
│   └── ui/               # Shared UI components
├── infrastructure/
│   ├── docker/           # Docker configurations
│   ├── terraform/        # AWS infrastructure (VPC, EKS, RDS)
│   ├── helm/             # Kubernetes charts
│   └── on-premises/      # On-premises deployment
├── docs/
│   ├── site/             # Docusaurus documentation
│   ├── adr/              # Architecture Decision Records
│   └── openapi.yaml      # API specification
├── examples/             # Sample legacy codebases
└── scripts/              # Development scripts
```

## 🛠️ Tech Stack

- **Frontend**: Next.js 14, React, TailwindCSS, Radix UI
- **Backend**: Hono (Node.js), TypeScript
- **AI**: GitHub Copilot SDK, Multi-agent orchestration
- **Parsers**: Custom TypeScript parsers for legacy languages
- **Database**: PostgreSQL, Drizzle ORM
- **Cache**: Redis
- **Storage**: S3/MinIO
- **Infrastructure**: Docker, Kubernetes (EKS), Terraform
- **CI/CD**: GitHub Actions

## 🏁 Getting Started

### Prerequisites

- Node.js 20+
- pnpm 8+
- Docker (for local development)

### Installation

```bash
# Clone the repository
git clone https://github.com/your-org/migrationpilot.git
cd migrationpilot

# Install dependencies
pnpm install

# Copy environment file
cp .env.example .env

# Start infrastructure (PostgreSQL, Redis, MinIO)
pnpm docker:up

# Run development servers
pnpm dev
```

### Quick Start

1. **Start the API server** (http://localhost:3001):
   ```bash
   pnpm --filter @migrationpilot/api dev
   ```

2. **Start the web dashboard** (http://localhost:3000):
   ```bash
   pnpm --filter @migrationpilot/web dev
   ```

3. **Create a new project** and upload your legacy code

4. **Run analysis** to extract business rules

5. **Review rules** with SME collaboration features

6. **Generate modern code** and validate equivalence

### Using the CLI

```bash
# Install CLI globally
npm install -g @migrationpilot/cli

# Analyze legacy code
migrationpilot analyze ./legacy-code --language cobol

# Start a migration
migrationpilot migrate --project my-migration --target java

# Validate equivalence
migrationpilot validate --project my-migration
```

## 📖 Documentation

Full documentation is available in the `docs/site` directory:

```bash
cd docs/site
pnpm install
pnpm dev
```

Topics include:
- [Introduction & Overview](docs/site/docs/intro.md)
- [Getting Started Guide](docs/site/docs/getting-started/installation.md)
- [Architecture Overview](docs/site/docs/architecture/overview.md)
- [API Reference](docs/site/docs/api/overview.md)
- [Deployment Guide](docs/site/docs/deployment/)

## 🧪 Testing

```bash
# Run all tests
pnpm test

# Run tests for a specific package
pnpm --filter @migrationpilot/agents test

# Run tests with coverage
pnpm test -- --coverage
```

## 🚢 Deployment

### Docker (Local)

```bash
# Build and run with Docker Compose
docker-compose -f infrastructure/docker/docker-compose.yml --profile full up -d
```

### On-Premises

```bash
cd infrastructure/on-premises
./install.sh
```

See [on-premises README](infrastructure/on-premises/README.md) for:
- Air-gapped installation
- Local LLM setup (Ollama, vLLM)
- Security hardening
- Backup procedures

### Kubernetes (Production)

```bash
# Deploy with Helm
helm install migrationpilot infrastructure/helm/migrationpilot \
  --namespace migrationpilot \
  --create-namespace \
  -f values.yaml
```

### AWS (Terraform)

```bash
cd infrastructure/terraform
terraform init
terraform plan -var-file="environments/production.tfvars"
terraform apply
```

## 🤖 AI Agents

MigrationPilot uses a multi-agent architecture powered by GitHub Copilot SDK:

| Agent | Purpose | Key Capabilities |
|-------|---------|------------------|
| **Archeologist** | Analyzes legacy code, extracts business rules with 90%+ confidence | AST parsing, rule extraction, complexity analysis, dependency mapping |
| **Architect** | Designs modern architecture, service boundaries, database schemas | DDD patterns, API design, migration strategies, risk assessment |
| **Builder** | Generates clean, idiomatic code and comprehensive tests | Multi-language generation, test creation, full traceability |
| **Validator** | Runs equivalence tests, calculates confidence scores | Boundary testing, property-based testing, root cause analysis |

### Agent Configuration

Each agent is configurable:

```typescript
// Example: Archeologist with custom settings
const archeologist = new ArcheologistAgent({
  model: 'gpt-4',
  temperature: 0.1,  // Low for analytical precision
  maxTokens: 8000,
});

const analysis = await archeologist.analyzeFile(context, sourceCode, 'cobol', 'PAYROLL.cbl');
```

## 🔧 Configuration

Key environment variables:

```env
# Database
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/migrationpilot

# Redis
REDIS_URL=redis://localhost:6379

# GitHub Copilot SDK
GITHUB_TOKEN=your-github-token

# Storage
S3_ENDPOINT=http://localhost:9000
S3_ACCESS_KEY=minioadmin
S3_SECRET_KEY=minioadmin
S3_BUCKET=migrationpilot

# Security
JWT_SECRET=your-jwt-secret
ENCRYPTION_KEY=your-encryption-key
```

## 📊 Supported Languages

### Source Languages

| Language | Dialects | Parser |
|----------|----------|--------|
| COBOL | COBOL-85, IBM Enterprise, Micro Focus, GnuCOBOL | `@migrationpilot/parsers` |
| Fortran | F77, F90, F95 | `@migrationpilot/parsers` |
| Visual Basic | VB6, VBA | `@migrationpilot/parsers` |
| Legacy Java | J2EE, EJB 2.x, Struts 1.x | `@migrationpilot/parsers` |
| RPG | RPG II, RPG III, ILE RPG | `@migrationpilot/parsers` |
| PL/I | Enterprise PL/I | `@migrationpilot/parsers` |
| Natural | Software AG Natural | `@migrationpilot/parsers` |
| Assembler | IBM HLASM | `@migrationpilot/parsers` |
| CICS | CICS command-level | `@migrationpilot/parsers` |

### Target Languages

| Language | Frameworks | Generator |
|----------|------------|-----------|
| Java | Spring Boot, Quarkus | `@migrationpilot/generators` |
| Python | FastAPI, Django | `@migrationpilot/generators` |
| TypeScript | NestJS, Express | `@migrationpilot/generators` |
| Go | Standard Library, Gin | `@migrationpilot/generators` |
| C# | .NET Core, ASP.NET Core | `@migrationpilot/generators` |

## 🔌 API Reference

The API server exposes REST endpoints for all migration operations:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/projects` | GET/POST | List or create migration projects |
| `/api/projects/:id/analysis` | POST | Run legacy code analysis |
| `/api/projects/:id/migration` | POST | Start full migration pipeline |
| `/api/projects/:id/validation` | POST | Run equivalence validation |
| `/api/dashboard/stats` | GET | Get dashboard statistics |
| `/api/metrics` | GET | Prometheus-compatible metrics |
| `/api/export/:projectId` | GET | Export migration artifacts |

See [docs/openapi.yaml](docs/openapi.yaml) for the complete API specification.

## 🔐 Security & Compliance

- **Authentication**: JWT tokens, API keys, OAuth2/OIDC
- **Authorization**: RBAC with fine-grained permissions
- **Encryption**: TLS 1.3 in transit, AES-256 at rest
- **Audit Logging**: Complete trail of all actions
- **Multi-tenancy**: Data isolation between organizations
- **Compliance**: SOC 2 Type II ready

## 🤝 Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## 📄 License

This project is licensed under the MIT License - see [LICENSE](LICENSE) for details.

## 🙏 Acknowledgments

- [GitHub Copilot SDK](https://github.com/github/copilot-sdk) for AI capabilities
- [Hono](https://hono.dev/) for the fast API framework
- [Drizzle ORM](https://orm.drizzle.team/) for type-safe database access
- [Turborepo](https://turbo.build/) for monorepo management
- [Next.js](https://nextjs.org/) for the web dashboard

---

<div align="center">
  <strong>Built with ❤️ by Jose David Baena</strong>
  <br><br>
  <a href="docs/site/docs/intro.md">Documentation</a> •
  <a href="CONTRIBUTING.md">Contributing</a> •
  <a href="CHANGELOG.md">Changelog</a>
</div>
