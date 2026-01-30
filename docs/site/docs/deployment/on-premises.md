# On-Premises Deployment

Deploy MigrationPilot in your own data center or private cloud environment.

## Overview

On-premises deployment is ideal for organizations with:
- Strict data residency requirements
- Air-gapped or highly secured environments
- Existing infrastructure investments
- Custom compliance requirements

## Deployment Options

### Docker Compose (Small Teams)

Best for teams with up to 10 concurrent users and projects under 500K lines of code.

```bash
cd infrastructure/on-premises

# Start all services
docker-compose -f docker-compose.on-prem.yml up -d

# View logs
docker-compose -f docker-compose.on-prem.yml logs -f

# Stop services
docker-compose -f docker-compose.on-prem.yml down
```

### Kubernetes with Helm (Enterprise)

For larger deployments with high availability requirements. See the [Kubernetes guide](./kubernetes.md).

## Quick Start

### 1. Install Prerequisites

```bash
# Install Docker
curl -fsSL https://get.docker.com | sh

# Install Docker Compose
apt-get install docker-compose-plugin

# Verify installation
docker --version
docker compose version
```

### 2. Configure Environment

```bash
cp .env.example .env

# Edit configuration
nano .env
```

Key configuration:

```env
# Database
POSTGRES_USER=migrationpilot
POSTGRES_PASSWORD=secure-password-here
POSTGRES_DB=migrationpilot

# Storage
S3_ACCESS_KEY=your-access-key
S3_SECRET_KEY=your-secret-key

# Security
JWT_SECRET=generate-a-32-char-secret
ENCRYPTION_KEY=generate-a-32-char-key
```

### 3. Run Installation

```bash
./install.sh
```

This script will:
1. Validate prerequisites
2. Pull required Docker images
3. Initialize the database
4. Start all services
5. Run health checks

### 4. Access the Application

- **Web Dashboard**: http://your-server:3000
- **API**: http://your-server:4000
- **MinIO Console**: http://your-server:9001

## Air-Gapped Installation

For environments without internet access:

### Prepare Bundle (On Connected Machine)

```bash
# Download all images and dependencies
./scripts/download-bundle.sh

# This creates: migrationpilot-bundle-v0.1.0.tar.gz
```

### Transfer and Install

```bash
# Transfer bundle to air-gapped machine
scp migrationpilot-bundle-v0.1.0.tar.gz user@airgapped-server:/tmp/

# On air-gapped server
cd /tmp
tar -xzf migrationpilot-bundle-v0.1.0.tar.gz
cd migrationpilot-bundle

# Load Docker images
./load-images.sh

# Deploy
docker-compose -f docker-compose.airgap.yml up -d
```

## Local LLM Configuration

For environments that cannot use cloud AI APIs, MigrationPilot supports local LLM deployments.

### Supported Providers

| Provider | Best For | GPU Required |
|----------|----------|--------------|
| **Ollama** | Easy setup, development | Optional |
| **vLLM** | Production, high throughput | Yes |
| **llama.cpp** | CPU-only environments | No |

### Ollama Setup

```bash
# Install Ollama
curl -fsSL https://ollama.com/install.sh | sh

# Pull recommended model
ollama pull codellama:34b

# Configure MigrationPilot
export LLM_PROVIDER=ollama
export OLLAMA_BASE_URL=http://localhost:11434
export OLLAMA_MODEL=codellama:34b
```

### Recommended Models

| Use Case | Model | RAM Required | Accuracy |
|----------|-------|--------------|----------|
| Code Analysis | codellama:34b | 64GB | Excellent |
| Code Generation | deepseek-coder:33b | 64GB | Excellent |
| Smaller Systems | codellama:7b | 16GB | Good |
| Testing | phi-2 | 8GB | Fair |

## Resource Requirements

### Minimum (Small Team)

| Component | CPU | RAM | Storage |
|-----------|-----|-----|---------|
| API Server | 2 cores | 4GB | - |
| Web Frontend | 1 core | 1GB | - |
| PostgreSQL | 2 cores | 4GB | 100GB |
| Redis | 1 core | 2GB | 10GB |
| MinIO | 1 core | 2GB | 500GB |
| **Total** | **7 cores** | **13GB** | **610GB** |

### Recommended (Enterprise)

| Component | CPU | RAM | Storage |
|-----------|-----|-----|---------|
| API (x3) | 4 cores each | 8GB each | - |
| Web (x2) | 2 cores each | 4GB each | - |
| PostgreSQL (HA) | 8 cores | 32GB | 500GB SSD |
| Redis (HA) | 4 cores | 16GB | 50GB |
| MinIO (distributed) | 4 cores | 8GB | 2TB |
| Local LLM | 16 cores | 64GB | 100GB |
| **Total** | **46+ cores** | **156GB** | **2.65TB** |

## Security Hardening

### Network Isolation

Services run on internal networks by default:

```yaml
networks:
  migrationpilot-internal:
    internal: true  # Database, Redis - no external access
  migrationpilot-frontend:
    # Only web and API are exposed
```

### TLS Configuration

```bash
# Generate self-signed certificates (or use your CA)
./scripts/generate-certs.sh

# Configure in environment
export TLS_CERT_PATH=/etc/migrationpilot/certs/server.crt
export TLS_KEY_PATH=/etc/migrationpilot/certs/server.key
```

### Secret Management

Integrate with your existing secret manager:

```env
# HashiCorp Vault
SECRETS_PROVIDER=vault
VAULT_ADDR=https://vault.internal:8200
VAULT_PATH=secret/data/migrationpilot

# AWS Secrets Manager
SECRETS_PROVIDER=aws-secrets
AWS_SECRET_NAME=migrationpilot/production
```

## Backup & Recovery

### Database Backup

```bash
# Manual backup
./scripts/backup.sh

# Automated daily backups (add to crontab)
0 2 * * * /opt/migrationpilot/scripts/backup.sh
```

### Restore from Backup

```bash
./scripts/restore.sh backup-2024-01-28.sql.gz
```

### Full System Backup

```bash
# Backup everything (database, files, config)
./scripts/full-backup.sh

# Disaster recovery
./scripts/disaster-recovery.sh backup-full-2024-01-28.tar.gz
```

## Monitoring

### Health Endpoints

| Endpoint | Purpose |
|----------|---------|
| `GET /api/health` | Overall health |
| `GET /api/health/ready` | Readiness probe |
| `GET /api/health/live` | Liveness probe |
| `GET /metrics` | Prometheus metrics |

### Key Metrics

- `migrationpilot_projects_total`
- `migrationpilot_rules_extracted_total`
- `migrationpilot_api_requests_total`
- `migrationpilot_api_latency_seconds`

## Troubleshooting

### Services Won't Start

```bash
# Check service status
docker-compose ps

# View logs
docker-compose logs api

# Check resource usage
docker stats
```

### Database Connection Issues

```bash
# Test connection
docker exec migrationpilot-db pg_isready

# Check PostgreSQL logs
docker logs migrationpilot-db
```

### Performance Issues

```bash
# Check API response time
curl -w "@curl-format.txt" http://localhost:4000/api/health

# Monitor real-time
docker stats --no-stream
```

## Support

For enterprise support:
- Documentation: https://docs.migrationpilot.dev/on-premises
- Email: enterprise-support@migrationpilot.dev
