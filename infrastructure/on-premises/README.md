# MigrationPilot On-Premises Deployment

This directory contains configuration and scripts for deploying MigrationPilot in on-premises environments.

## Deployment Options

### 1. Docker Compose (Recommended for Small Teams)

Suitable for teams with up to 10 concurrent users and projects with less than 500K lines of code.

```bash
# Start all services
docker-compose -f docker-compose.on-prem.yml up -d

# View logs
docker-compose -f docker-compose.on-prem.yml logs -f

# Stop services
docker-compose -f docker-compose.on-prem.yml down
```

### 2. Kubernetes with Helm (Enterprise)

For larger deployments with high availability requirements.

```bash
# Add the MigrationPilot Helm repository
helm repo add migrationpilot https://charts.migrationpilot.dev

# Install with custom values
helm install migrationpilot migrationpilot/migrationpilot \
  -f values.on-prem.yaml \
  --namespace migrationpilot \
  --create-namespace
```

## Air-Gapped Installation

For environments without internet access:

1. **Download Bundle** (on machine with internet):
   ```bash
   ./scripts/download-bundle.sh
   ```

2. **Transfer Bundle** to air-gapped environment

3. **Load Images**:
   ```bash
   ./scripts/load-images.sh
   ```

4. **Deploy**:
   ```bash
   docker-compose -f docker-compose.airgap.yml up -d
   ```

## Local LLM Configuration

MigrationPilot supports local LLM deployments for environments that cannot use cloud AI APIs.

### Supported Local LLM Options

1. **Ollama** (Recommended)
   - Easy setup
   - Supports many models
   - Good for smaller deployments

2. **vLLM**
   - High performance
   - Production-grade
   - Requires GPU

3. **llama.cpp**
   - CPU-friendly
   - Minimal resources
   - Good for testing

### Configuration

Set the following environment variables:

```env
# Ollama
LLM_PROVIDER=ollama
OLLAMA_BASE_URL=http://ollama:11434
OLLAMA_MODEL=codellama:34b

# vLLM
LLM_PROVIDER=vllm
VLLM_BASE_URL=http://vllm:8000
VLLM_MODEL=codellama/CodeLlama-34b-hf

# llama.cpp
LLM_PROVIDER=llamacpp
LLAMACPP_BASE_URL=http://llamacpp:8080
```

### Recommended Models

| Use Case | Model | Min RAM | Notes |
|----------|-------|---------|-------|
| Code Analysis | codellama:34b | 64GB | Best accuracy |
| Code Generation | deepseek-coder:33b | 64GB | Good for generation |
| Smaller Systems | codellama:7b | 16GB | Faster, lower accuracy |
| Testing | phi-2 | 8GB | Fast iteration |

## Security Hardening

### Network Isolation

```yaml
# docker-compose.on-prem.yml network configuration
networks:
  migrationpilot-internal:
    internal: true  # No external access
  migrationpilot-frontend:
    # Only web and API exposed
```

### Secret Management

Integrate with your existing secret manager:

```yaml
# For HashiCorp Vault
secrets:
  provider: vault
  vault_addr: https://vault.internal:8200
  vault_path: secret/data/migrationpilot
```

### TLS Configuration

```yaml
# Generate certificates
./scripts/generate-certs.sh

# Configure in docker-compose
services:
  api:
    environment:
      TLS_CERT_PATH: /certs/server.crt
      TLS_KEY_PATH: /certs/server.key
    volumes:
      - ./certs:/certs:ro
```

### Audit Logging

All actions are logged to:
- stdout/stderr (for log aggregation)
- PostgreSQL `audit_logs` table
- Optional: External SIEM integration

```env
AUDIT_LOG_DESTINATION=postgres,stdout
SIEM_WEBHOOK_URL=https://siem.internal/webhook
```

## Resource Requirements

### Minimum (Small Team)

| Component | CPU | RAM | Storage |
|-----------|-----|-----|---------|
| API | 2 cores | 4GB | - |
| Web | 1 core | 1GB | - |
| PostgreSQL | 2 cores | 4GB | 100GB |
| Redis | 1 core | 2GB | 10GB |
| MinIO | 1 core | 2GB | 500GB |
| **Total** | **7 cores** | **13GB** | **610GB** |

### Recommended (Enterprise)

| Component | CPU | RAM | Storage |
|-----------|-----|-----|---------|
| API (x3) | 4 cores | 8GB | - |
| Web (x2) | 2 cores | 4GB | - |
| PostgreSQL (HA) | 8 cores | 32GB | 500GB SSD |
| Redis (HA) | 4 cores | 16GB | 50GB |
| MinIO (distributed) | 4 cores | 8GB | 2TB |
| Local LLM (optional) | 16 cores | 64GB | 100GB |
| **Total** | **46+ cores** | **156GB** | **2.65TB** |

## Backup & Recovery

### Database Backup

```bash
# Create backup
./scripts/backup.sh

# Restore from backup
./scripts/restore.sh backup-2024-01-28.sql.gz
```

### Full System Backup

```bash
# Backup all data (database, files, configuration)
./scripts/full-backup.sh

# Disaster recovery
./scripts/disaster-recovery.sh backup-full-2024-01-28.tar.gz
```

## Health Monitoring

### Health Check Endpoints

- `GET /api/health` - Overall health
- `GET /api/health/ready` - Readiness probe
- `GET /api/health/live` - Liveness probe

### Metrics

Prometheus metrics available at `/metrics`:

- `migrationpilot_projects_total`
- `migrationpilot_rules_extracted_total`
- `migrationpilot_tests_executed_total`
- `migrationpilot_api_requests_total`
- `migrationpilot_api_latency_seconds`

### Alerts

Configure alerts in your monitoring system:

```yaml
# Example Prometheus alert rules
groups:
  - name: migrationpilot
    rules:
      - alert: HighErrorRate
        expr: rate(migrationpilot_api_errors_total[5m]) > 0.1
        for: 5m
        labels:
          severity: warning
          
      - alert: APIDown
        expr: up{job="migrationpilot-api"} == 0
        for: 1m
        labels:
          severity: critical
```

## Support

For on-premises deployment support:
- Email: enterprise-support@migrationpilot.dev
- Documentation: https://docs.migrationpilot.dev/on-premises
