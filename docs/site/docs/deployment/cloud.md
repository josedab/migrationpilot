# Cloud Deployment

Deploy MigrationPilot to cloud providers using Docker or Kubernetes.

## Prerequisites

- Docker 20.10+
- Docker Compose v2+ (for Docker deployment)
- kubectl and Helm 3+ (for Kubernetes deployment)
- AWS CLI (for AWS deployment)

## Docker Compose Deployment

### Development Setup

Start infrastructure services only (PostgreSQL, Redis, MinIO):

```bash
cd infrastructure/docker
docker-compose up -d
```

Services started:
- **PostgreSQL** - Database at `localhost:5432`
- **Redis** - Cache at `localhost:6379`
- **MinIO** - S3-compatible storage at `localhost:9000`

### Full Stack Deployment

Deploy the complete stack including API and Web:

```bash
cd infrastructure/docker
docker-compose --profile full up -d
```

Services started:
- PostgreSQL at `localhost:5432`
- Redis at `localhost:6379`
- MinIO at `localhost:9000` (console at `localhost:9001`)
- API Server at `localhost:4000`
- Web Dashboard at `localhost:3000`

### Environment Variables

Create a `.env` file in the project root:

```env
# Database
DATABASE_URL=postgresql://postgres:postgres@localhost:5432/migrationpilot

# Redis
REDIS_URL=redis://localhost:6379

# Storage
S3_ENDPOINT=http://localhost:9000
S3_ACCESS_KEY=minioadmin
S3_SECRET_KEY=minioadmin
S3_BUCKET=migrationpilot

# GitHub Copilot SDK
GITHUB_TOKEN=your-github-token

# Security
JWT_SECRET=your-secure-jwt-secret
ENCRYPTION_KEY=your-32-character-encryption-key

# API
API_PORT=4000
NODE_ENV=production
```

### Scaling

Use Docker Swarm or Docker Compose scaling:

```bash
# Scale API servers
docker-compose --profile full up -d --scale api=3
```

## AWS Deployment with Terraform

### Prerequisites

1. AWS Account with appropriate permissions
2. Terraform 1.0+
3. AWS CLI configured

### Infrastructure Overview

The Terraform configuration creates:
- VPC with public/private subnets
- EKS cluster for Kubernetes workloads
- RDS PostgreSQL instance
- ElastiCache Redis cluster
- S3 bucket for file storage
- Application Load Balancer
- IAM roles and security groups

### Deployment Steps

```bash
cd infrastructure/terraform

# Initialize Terraform
terraform init

# Review the plan
terraform plan -var-file="environments/production.tfvars"

# Apply the infrastructure
terraform apply -var-file="environments/production.tfvars"
```

### Configuration Variables

Create `environments/production.tfvars`:

```hcl
# AWS Configuration
aws_region = "us-east-1"
environment = "production"

# VPC Configuration
vpc_cidr = "10.0.0.0/16"
availability_zones = ["us-east-1a", "us-east-1b", "us-east-1c"]

# EKS Configuration
cluster_name = "migrationpilot-prod"
node_instance_types = ["t3.large"]
node_desired_capacity = 3
node_min_size = 2
node_max_size = 10

# RDS Configuration
db_instance_class = "db.t3.medium"
db_allocated_storage = 100
db_engine_version = "16"
db_multi_az = true

# ElastiCache Configuration
cache_node_type = "cache.t3.medium"
cache_num_cache_nodes = 2

# S3 Configuration
s3_bucket_name = "migrationpilot-prod-storage"
```

### Post-Deployment

After Terraform completes:

```bash
# Update kubeconfig
aws eks update-kubeconfig --name migrationpilot-prod --region us-east-1

# Deploy application with Helm
helm install migrationpilot infrastructure/helm/migrationpilot \
  --namespace migrationpilot \
  --create-namespace \
  -f values-production.yaml
```

## Health Checks

Verify deployment health:

```bash
# Check API health
curl http://localhost:4000/health

# Check Web app
curl http://localhost:3000

# Check database connection
docker exec migrationpilot-db pg_isready

# Check Redis
docker exec migrationpilot-redis redis-cli ping
```

## Monitoring

### Logs

```bash
# API logs
docker logs -f migrationpilot-api

# Web logs
docker logs -f migrationpilot-web

# All services
docker-compose logs -f
```

### Resource Usage

```bash
docker stats
```

## Cleanup

```bash
# Docker Compose
docker-compose down -v

# Terraform
terraform destroy -var-file="environments/production.tfvars"
```
