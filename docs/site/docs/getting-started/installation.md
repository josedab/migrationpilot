---
sidebar_position: 1
---

# Installation

This guide covers how to install MigrationPilot in different environments.

## Prerequisites

- Node.js 18 or later
- Docker (for local development)
- A GitHub account (for Copilot SDK access)

## Quick Installation

### Using the CLI

The fastest way to get started is with the MigrationPilot CLI:

```bash
# Install globally
npm install -g @migrationpilot/cli

# Verify installation
migrationpilot --version
```

### Using Docker

For a complete local environment:

```bash
# Clone the repository
git clone https://github.com/migrationpilot/migrationpilot.git
cd migrationpilot

# Copy environment variables
cp .env.example .env

# Start all services
docker compose up -d

# Access the dashboard
open http://localhost:3000
```

## Development Setup

For contributing to MigrationPilot or running from source:

```bash
# Clone the repository
git clone https://github.com/migrationpilot/migrationpilot.git
cd migrationpilot

# Install dependencies
pnpm install

# Set up environment variables
cp .env.example .env
# Edit .env with your configuration

# Start infrastructure (PostgreSQL, Redis, MinIO)
pnpm docker:up

# Run database migrations
pnpm db:migrate

# Start development servers
pnpm dev
```

This starts:
- Web dashboard at http://localhost:3000
- API server at http://localhost:3001

## Cloud Deployment

### AWS

See our [AWS deployment guide](/docs/deployment/cloud#aws) for:
- Terraform configurations
- EKS cluster setup
- RDS and ElastiCache configuration

### Azure

Coming soon.

### Google Cloud

Coming soon.

## On-Premises Deployment

For air-gapped or security-sensitive environments:

```bash
cd infrastructure/on-premises

# Run the installation script
./install.sh

# Follow the prompts
```

See the [on-premises guide](/docs/deployment/on-premises) for:
- Hardware requirements
- Security configuration
- Local LLM setup
- Backup procedures

## Configuration

### Environment Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `DATABASE_URL` | PostgreSQL connection string | Yes |
| `REDIS_URL` | Redis connection string | Yes |
| `GITHUB_TOKEN` | GitHub token for Copilot SDK | Yes |
| `S3_ENDPOINT` | S3-compatible storage endpoint | Yes |
| `JWT_SECRET` | Secret for JWT signing | Yes |

### Configuration File

Create a `migrationpilot.config.js` in your project root:

```javascript
module.exports = {
  defaultSourceLanguage: 'cobol',
  defaultTargetLanguage: 'java',
  confidenceThreshold: 0.85,
  humanReviewRequired: true,
  targetFrameworks: {
    java: 'spring-boot',
    python: 'fastapi',
    typescript: 'nestjs',
  },
};
```

## Verifying Installation

Run the health check:

```bash
# Using CLI
migrationpilot config list

# Using API
curl http://localhost:3001/api/health
```

Expected output:

```json
{
  "status": "healthy",
  "version": "0.1.0",
  "services": {
    "database": "connected",
    "redis": "connected",
    "storage": "connected"
  }
}
```

## Troubleshooting

### Database Connection Issues

```bash
# Check if PostgreSQL is running
docker ps | grep postgres

# Check connection
psql $DATABASE_URL -c "SELECT 1"
```

### Redis Connection Issues

```bash
# Check if Redis is running
docker ps | grep redis

# Test connection
redis-cli -u $REDIS_URL ping
```

### Missing Dependencies

```bash
# Clean and reinstall
rm -rf node_modules
pnpm install
```

## Next Steps

- [Try the quickstart](/docs/getting-started/quickstart)
- [Run your first migration](/docs/getting-started/your-first-migration)
- [Learn about the AI agents](/docs/concepts/ai-agents)
