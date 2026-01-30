# Kubernetes Deployment

Deploy MigrationPilot to Kubernetes using Helm charts.

## Prerequisites

- Kubernetes 1.24+
- Helm 3.0+
- kubectl configured for your cluster
- An ingress controller (nginx, ALB, Traefik)
- External PostgreSQL and Redis (or in-cluster)

## Quick Start

```bash
# Deploy with default values
helm install migrationpilot infrastructure/helm/migrationpilot \
  --namespace migrationpilot \
  --create-namespace
```

## Configuration

### Create Values File

Create a `values-production.yaml`:

```yaml
# Replicas
replicaCount:
  api: 3
  web: 2

# Image configuration
image:
  repository: ghcr.io/your-org/migrationpilot
  tag: "v0.1.0"
  pullPolicy: IfNotPresent

environment: production

# API Configuration
api:
  port: 4000
  resources:
    limits:
      cpu: 2000m
      memory: 4Gi
    requests:
      cpu: 500m
      memory: 1Gi
  env:
    LOG_LEVEL: info
    NODE_ENV: production

# Web Configuration
web:
  port: 3000
  resources:
    limits:
      cpu: 1000m
      memory: 2Gi
    requests:
      cpu: 250m
      memory: 512Mi

# Database (external RDS)
database:
  host: migrationpilot-db.xxxxx.us-east-1.rds.amazonaws.com
  port: 5432
  name: migrationpilot
  username: migrationpilot
  existingSecret: migrationpilot-db-credentials
  existingSecretKey: password

# Redis (external ElastiCache)
redis:
  host: migrationpilot-redis.xxxxx.cache.amazonaws.com
  port: 6379

# S3 Storage
storage:
  endpoint: https://s3.us-east-1.amazonaws.com
  bucket: migrationpilot-production
  existingSecret: migrationpilot-s3-credentials

# Ingress
ingress:
  enabled: true
  className: alb
  annotations:
    kubernetes.io/ingress.class: alb
    alb.ingress.kubernetes.io/scheme: internet-facing
    alb.ingress.kubernetes.io/target-type: ip
    alb.ingress.kubernetes.io/certificate-arn: arn:aws:acm:...
    alb.ingress.kubernetes.io/ssl-policy: ELBSecurityPolicy-TLS-1-2-2017-01
  hosts:
    - host: migrationpilot.example.com
      paths:
        - path: /api
          pathType: Prefix
          service: api
        - path: /
          pathType: Prefix
          service: web
  tls:
    - secretName: migrationpilot-tls
      hosts:
        - migrationpilot.example.com

# Autoscaling
autoscaling:
  enabled: true
  api:
    minReplicas: 3
    maxReplicas: 20
    targetCPUUtilizationPercentage: 70
  web:
    minReplicas: 2
    maxReplicas: 10
    targetCPUUtilizationPercentage: 70

# Pod Disruption Budget
podDisruptionBudget:
  enabled: true
  minAvailable: 2
```

### Deploy with Custom Values

```bash
helm install migrationpilot infrastructure/helm/migrationpilot \
  --namespace migrationpilot \
  --create-namespace \
  -f values-production.yaml
```

## Secrets Management

### Create Required Secrets

```bash
# Database credentials
kubectl create secret generic migrationpilot-db-credentials \
  --namespace migrationpilot \
  --from-literal=password='your-db-password'

# S3 credentials
kubectl create secret generic migrationpilot-s3-credentials \
  --namespace migrationpilot \
  --from-literal=accessKey='your-access-key' \
  --from-literal=secretKey='your-secret-key'

# GitHub token
kubectl create secret generic migrationpilot-github \
  --namespace migrationpilot \
  --from-literal=token='your-github-token'

# Auth secret
kubectl create secret generic migrationpilot-auth \
  --namespace migrationpilot \
  --from-literal=secret='your-jwt-secret'
```

### Using External Secrets Operator

For production, integrate with AWS Secrets Manager or HashiCorp Vault:

```yaml
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: migrationpilot-db-credentials
  namespace: migrationpilot
spec:
  refreshInterval: 1h
  secretStoreRef:
    name: aws-secrets-manager
    kind: ClusterSecretStore
  target:
    name: migrationpilot-db-credentials
  data:
    - secretKey: password
      remoteRef:
        key: migrationpilot/production/database
        property: password
```

## Upgrade

```bash
# Upgrade with new values
helm upgrade migrationpilot infrastructure/helm/migrationpilot \
  --namespace migrationpilot \
  -f values-production.yaml

# Upgrade to specific version
helm upgrade migrationpilot infrastructure/helm/migrationpilot \
  --namespace migrationpilot \
  --set image.tag=v0.2.0 \
  -f values-production.yaml
```

## Rollback

```bash
# List release history
helm history migrationpilot -n migrationpilot

# Rollback to previous release
helm rollback migrationpilot -n migrationpilot

# Rollback to specific revision
helm rollback migrationpilot 3 -n migrationpilot
```

## Health Checks

### Verify Deployment

```bash
# Check pods
kubectl get pods -n migrationpilot

# Check services
kubectl get svc -n migrationpilot

# Check ingress
kubectl get ingress -n migrationpilot
```

### View Logs

```bash
# API logs
kubectl logs -n migrationpilot -l app=migrationpilot-api -f

# Web logs
kubectl logs -n migrationpilot -l app=migrationpilot-web -f
```

### Test Endpoints

```bash
# Port forward for local testing
kubectl port-forward -n migrationpilot svc/migrationpilot-api 4000:4000

# Test health endpoint
curl http://localhost:4000/api/health
```

## Monitoring

### Enable ServiceMonitor (Prometheus)

```yaml
metrics:
  enabled: true
  serviceMonitor:
    enabled: true
    interval: 30s
    labels:
      release: prometheus
```

### Key Metrics

| Metric | Description |
|--------|-------------|
| `migrationpilot_api_requests_total` | Total API requests |
| `migrationpilot_api_latency_seconds` | Request latency histogram |
| `migrationpilot_projects_total` | Total projects |
| `migrationpilot_rules_extracted_total` | Extracted business rules |

## Troubleshooting

### Pod Not Starting

```bash
# Check pod status
kubectl describe pod -n migrationpilot <pod-name>

# Check events
kubectl get events -n migrationpilot --sort-by='.lastTimestamp'
```

### Database Connection Issues

```bash
# Exec into pod
kubectl exec -it -n migrationpilot <api-pod> -- sh

# Test database connection
nc -zv <db-host> 5432
```

### Ingress Not Working

```bash
# Check ingress status
kubectl describe ingress -n migrationpilot migrationpilot

# Check ALB controller logs (AWS)
kubectl logs -n kube-system -l app.kubernetes.io/name=aws-load-balancer-controller
```

## Uninstall

```bash
helm uninstall migrationpilot -n migrationpilot
kubectl delete namespace migrationpilot
```
