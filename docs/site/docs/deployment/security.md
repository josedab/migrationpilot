# Security

Security best practices and configuration for MigrationPilot deployments.

## Authentication

### JWT Tokens

MigrationPilot uses JWT tokens for API authentication:

```env
JWT_SECRET=your-secure-32-character-secret-key
JWT_EXPIRATION=24h
```

### API Keys

For programmatic access, use API keys:

```bash
# Create an API key via CLI
migrationpilot config set apiKey mp_sk_xxxxxxxxxxxx

# Use in requests
curl -H "Authorization: Bearer mp_sk_xxxxxxxxxxxx" \
  https://api.migrationpilot.example.com/api/projects
```

### OAuth2/OIDC Integration

Configure SSO with your identity provider:

```env
# Generic OIDC
OIDC_ISSUER=https://auth.example.com
OIDC_CLIENT_ID=migrationpilot
OIDC_CLIENT_SECRET=your-client-secret
OIDC_REDIRECT_URI=https://migrationpilot.example.com/auth/callback

# Azure AD
AZURE_AD_TENANT_ID=your-tenant-id
AZURE_AD_CLIENT_ID=your-client-id
AZURE_AD_CLIENT_SECRET=your-client-secret

# Okta
OKTA_DOMAIN=your-org.okta.com
OKTA_CLIENT_ID=your-client-id
OKTA_CLIENT_SECRET=your-client-secret
```

## Authorization

### Role-Based Access Control (RBAC)

MigrationPilot supports fine-grained permissions:

| Role | Description | Permissions |
|------|-------------|-------------|
| `admin` | Full access | All actions |
| `developer` | Standard user | Create projects, run migrations |
| `reviewer` | SME reviewer | Review and approve rules |
| `viewer` | Read-only | View projects and reports |

### Configuring Roles

```yaml
# In values.yaml for Kubernetes
rbac:
  enabled: true
  roles:
    - name: developer
      permissions:
        - projects:read
        - projects:create
        - migrations:run
        - rules:read
    - name: reviewer
      permissions:
        - projects:read
        - rules:read
        - rules:approve
        - rules:reject
```

## Encryption

### Data at Rest

All sensitive data is encrypted using AES-256:

```env
ENCRYPTION_KEY=your-32-character-encryption-key
```

Encrypted data includes:
- Source code files
- Business rules
- API keys
- User credentials

### Data in Transit

All connections use TLS 1.3:

```env
# TLS Configuration
TLS_ENABLED=true
TLS_CERT_PATH=/etc/migrationpilot/certs/server.crt
TLS_KEY_PATH=/etc/migrationpilot/certs/server.key
TLS_MIN_VERSION=1.3
```

### Database Encryption

Enable PostgreSQL encryption:

```sql
-- Enable pgcrypto extension
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Connection SSL
DATABASE_URL=postgresql://user:pass@host:5432/db?sslmode=verify-full
```

## Network Security

### Firewall Rules

Recommended ingress rules:

| Port | Service | Source |
|------|---------|--------|
| 443 | Web/API | 0.0.0.0/0 (via load balancer) |
| 4000 | API (internal) | Internal network only |
| 5432 | PostgreSQL | API servers only |
| 6379 | Redis | API servers only |

### Network Policies (Kubernetes)

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: migrationpilot-api
  namespace: migrationpilot
spec:
  podSelector:
    matchLabels:
      app: migrationpilot-api
  policyTypes:
    - Ingress
    - Egress
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              name: ingress-nginx
      ports:
        - protocol: TCP
          port: 4000
  egress:
    - to:
        - namespaceSelector:
            matchLabels:
              name: database
      ports:
        - protocol: TCP
          port: 5432
    - to:
        - namespaceSelector:
            matchLabels:
              name: redis
      ports:
        - protocol: TCP
          port: 6379
```

## Audit Logging

### Configuration

```env
AUDIT_LOG_ENABLED=true
AUDIT_LOG_DESTINATION=postgres,stdout
AUDIT_LOG_RETENTION_DAYS=90
```

### Logged Events

| Event | Data Captured |
|-------|---------------|
| `user.login` | User ID, IP, timestamp, success/failure |
| `project.create` | User ID, project details, timestamp |
| `migration.start` | User ID, project ID, configuration |
| `rule.approve` | User ID, rule ID, comments |
| `file.upload` | User ID, file hash, size, timestamp |

### Querying Audit Logs

```sql
-- Recent login attempts
SELECT * FROM audit_logs 
WHERE action = 'user.login' 
ORDER BY created_at DESC 
LIMIT 100;

-- Failed operations
SELECT * FROM audit_logs 
WHERE metadata->>'success' = 'false' 
ORDER BY created_at DESC;
```

### SIEM Integration

Forward logs to your SIEM:

```env
SIEM_WEBHOOK_URL=https://siem.example.com/webhook
SIEM_WEBHOOK_SECRET=your-webhook-secret
```

## Secrets Management

### Environment Variables

Never commit secrets to version control. Use environment variables:

```bash
# Development
export JWT_SECRET=$(openssl rand -hex 32)
export ENCRYPTION_KEY=$(openssl rand -hex 16)
```

### HashiCorp Vault

```env
SECRETS_PROVIDER=vault
VAULT_ADDR=https://vault.example.com:8200
VAULT_TOKEN=your-vault-token
VAULT_PATH=secret/data/migrationpilot
```

### AWS Secrets Manager

```env
SECRETS_PROVIDER=aws-secrets
AWS_REGION=us-east-1
AWS_SECRET_NAME=migrationpilot/production
```

## Vulnerability Scanning

### Container Images

Images are scanned for vulnerabilities:

```bash
# Scan with Trivy
trivy image ghcr.io/your-org/migrationpilot:latest
```

### Dependencies

Regular dependency audits:

```bash
# Node.js
npm audit
pnpm audit

# Run as part of CI/CD
npm audit --audit-level=high
```

## Security Headers

The API sets secure HTTP headers:

```
Strict-Transport-Security: max-age=31536000; includeSubDomains
X-Content-Type-Options: nosniff
X-Frame-Options: DENY
X-XSS-Protection: 1; mode=block
Content-Security-Policy: default-src 'self'
```

## Compliance

### SOC 2 Type II

MigrationPilot is designed for SOC 2 compliance:

- ✅ Access control and authentication
- ✅ Encryption at rest and in transit
- ✅ Audit logging
- ✅ Change management
- ✅ Incident response procedures

### GDPR

For GDPR compliance:

- Data minimization in logs
- Right to deletion (data purge APIs)
- Data export capabilities
- Consent tracking

### Data Retention

Configure data retention policies:

```env
DATA_RETENTION_DAYS=90
AUDIT_LOG_RETENTION_DAYS=365
DELETED_DATA_PURGE_DAYS=30
```

## Security Checklist

- [ ] Change default passwords
- [ ] Enable TLS everywhere
- [ ] Configure RBAC
- [ ] Enable audit logging
- [ ] Set up secret management
- [ ] Configure network policies
- [ ] Enable container scanning
- [ ] Set data retention policies
- [ ] Configure backup encryption
- [ ] Test disaster recovery
