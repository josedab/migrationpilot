---
sidebar_position: 51
---

# Changelog

All notable changes to MigrationPilot are documented here. This project follows [Semantic Versioning](https://semver.org/) and the changelog format is based on [Keep a Changelog](https://keepachangelog.com/).

## Release Policy

- **Major versions** (1.0.0, 2.0.0): Breaking changes, major new features
- **Minor versions** (0.2.0, 0.3.0): New features, backward compatible
- **Patch versions** (0.1.1, 0.1.2): Bug fixes, security patches

## [Unreleased]

These changes are in the `main` branch but not yet released.

### Added

- TypeScript type safety improvements across core modules
- Real JWT authentication using `jose` library
- Secure API key validation with SHA-256 hashing
- Test coverage reporting with thresholds (50% baseline)
- GitHub issue templates for bug reports and feature requests
- Pull request template with comprehensive checklist
- ADR (Architecture Decision Records) documentation
- CLI `--dry-run` mode for migration preview
- Explainer Agent for interactive Q&A about legacy code
- IDE extension for VS Code and IntelliJ
- Confidence visualization in code editor
- Differential testing engine for behavioral comparison
- Mainframe tracing with CICS, DB2, and batch job support

### Changed

- Security middleware now uses proper cryptographic validation
- Improved error messages for authentication failures
- Enhanced business rule extraction accuracy (89% → 95.5%)
- Faster migration processing (18 min/KLOC → 4 min/KLOC)

### Security

- Replaced stub JWT verification with proper jose-based implementation
- Added timing-safe comparison for API key validation
- Improved security headers configuration
- PII masking in mainframe traces

---

## [0.1.0] - 2026-01-30

First public release of MigrationPilot.

### Highlights

- Complete multi-agent AI architecture for legacy code modernization
- Support for 4 legacy languages and 5 modern target languages
- Web dashboard, API, and CLI interfaces
- Enterprise features: multi-tenancy, audit logging, on-premises deployment

### Added

#### Core Platform

- **Multi-agent AI architecture**
  - Archeologist Agent: Business rule extraction with confidence scoring
  - Architect Agent: Modern architecture design
  - Builder Agent: Clean code generation with full traceability
  - Validator Agent: Automated equivalence testing

- **Legacy Language Parsers**
  - COBOL (COBOL-85, IBM Enterprise, Micro Focus, GnuCOBOL)
  - Fortran (F77, F90, F95)
  - Visual Basic 6/VBA
  - Legacy Java (J2EE, EJB 2.x, Struts 1.x)

- **Modern Target Generators**
  - Java with Spring Boot
  - Python with FastAPI
  - TypeScript with NestJS
  - Go with Gin
  - C# with .NET Core

#### Applications

- **Web Dashboard** (Next.js)
  - Project management
  - Code exploration
  - Rule review interface
  - Test results visualization
  - Migration progress tracking

- **REST API** (Hono)
  - Full project lifecycle management
  - Real-time streaming via SSE
  - GraphQL endpoint
  - Prometheus metrics
  - OpenAPI documentation

- **CLI Tool**
  - `migrationpilot analyze` - Analyze legacy code
  - `migrationpilot migrate` - Run full migration
  - `migrationpilot validate` - Equivalence testing
  - `migrationpilot rules` - Manage business rules
  - `migrationpilot config` - Configuration management

#### Features

- **Business Rule Extraction**
  - Automatic extraction from legacy code
  - Confidence scoring (0-100%)
  - Rule categorization (calculation, validation, transformation, workflow)
  - Source traceability

- **SME Collaboration**
  - Rule review workflows
  - Comments and corrections
  - Knowledge capture
  - Approval workflows

- **Strangler Fig Support**
  - Incremental migration
  - Traffic routing between legacy and modern
  - Rollback capabilities
  - Zero-downtime transition

#### Infrastructure

- **Deployment Options**
  - Docker Compose for local development
  - Kubernetes with Helm charts
  - Terraform for AWS (VPC, EKS, RDS)
  - On-premises installation

- **Database**
  - PostgreSQL with Drizzle ORM
  - Type-safe schema definitions
  - Migration system

- **Supporting Services**
  - Redis for caching and session state
  - MinIO/S3 for artifact storage
  - Kafka/Redis Streams for job queue (optional)

#### Security & Compliance

- JWT token authentication
- API key authentication
- Role-based access control (RBAC)
- Multi-tenancy with data isolation
- TLS 1.3 in transit
- AES-256 encryption at rest
- Audit logging
- SOC 2 Type II ready architecture

### Known Issues

- Large COBOL programs (>10K lines) may require increased memory
- COPY REPLACING directive has limited support
- Some VB6 form bindings not fully parsed

---

## Upgrade Guide

### Upgrading to 0.1.x

For fresh installations only - no upgrade path needed.

### Future Upgrades

Starting with 0.2.0, we will provide:
- Database migration scripts
- Configuration migration guides
- Breaking change documentation

---

## Version Support

| Version | Status | Support Until |
|---------|--------|---------------|
| 0.1.x | Current | TBD |
| < 0.1 | Development | Not supported |

---

## Links

- [GitHub Releases](https://github.com/migrationpilot/migrationpilot/releases)
- [Migration Guides](/docs/deployment/cloud)
- [API Changelog](/docs/api/overview#changelog)
- [Report an Issue](https://github.com/migrationpilot/migrationpilot/issues/new)
