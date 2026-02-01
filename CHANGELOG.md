# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- TypeScript type safety improvements across core modules
- Real JWT authentication using `jose` library
- Secure API key validation with SHA-256 hashing
- Test coverage reporting with thresholds (50% baseline)
- GitHub issue templates for bug reports and feature requests
- Pull request template with comprehensive checklist
- ADR (Architecture Decision Records) documentation
- CLI `--dry-run` mode for migration preview

### Changed

- Security middleware now uses proper cryptographic validation
- Improved error messages for authentication failures

### Security

- Replaced stub JWT verification with proper jose-based implementation
- Added timing-safe comparison for API key validation
- Improved security headers configuration

## [0.1.0] - 2026-01-30

### Added

- Initial release of MigrationPilot
- Multi-agent AI architecture (Archeologist, Architect, Builder, Validator)
- Legacy language parsers for COBOL, Fortran, VB6, and Legacy Java
- Modern target language generators (Java/Spring, Python/FastAPI, TypeScript/NestJS, Go/Gin, C#/.NET)
- Web dashboard with Next.js
- REST API with Hono
- CLI tool for command-line migrations
- Business rule extraction with confidence scoring
- SME collaboration features for rule review
- Strangler Fig pattern support for incremental migration
- Docker, Kubernetes, and Terraform deployment options
- On-premises deployment support with local LLM option
- Multi-tenancy support
- Audit logging for compliance

### Infrastructure

- Turborepo monorepo setup
- PostgreSQL database with Drizzle ORM
- Redis for caching
- MinIO/S3 for artifact storage
- GitHub Actions CI/CD pipeline

[Unreleased]: https://github.com/your-org/migrationpilot/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/your-org/migrationpilot/releases/tag/v0.1.0
