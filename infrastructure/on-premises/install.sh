#!/bin/bash
# MigrationPilot On-Premises Installation Script

set -e

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║        MigrationPilot On-Premises Installation            ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo ""

# Check requirements
check_requirements() {
    echo "Checking requirements..."
    
    if ! command -v docker &> /dev/null; then
        echo "❌ Docker is not installed. Please install Docker first."
        exit 1
    fi
    
    if ! command -v docker-compose &> /dev/null && ! docker compose version &> /dev/null; then
        echo "❌ Docker Compose is not installed. Please install Docker Compose first."
        exit 1
    fi
    
    echo "✅ Docker and Docker Compose are installed"
}

# Generate secrets
generate_secrets() {
    echo ""
    echo "Generating secrets..."
    
    if [ -f .env ]; then
        echo "⚠️  .env file already exists. Skipping secret generation."
        return
    fi
    
    DB_PASSWORD=$(openssl rand -base64 32 | tr -dc 'a-zA-Z0-9' | head -c 32)
    JWT_SECRET=$(openssl rand -base64 64 | tr -dc 'a-zA-Z0-9' | head -c 64)
    ENCRYPTION_KEY=$(openssl rand -base64 32 | tr -dc 'a-zA-Z0-9' | head -c 32)
    MINIO_ACCESS_KEY="migrationpilot"
    MINIO_SECRET_KEY=$(openssl rand -base64 32 | tr -dc 'a-zA-Z0-9' | head -c 32)
    
    cat > .env << EOF
# MigrationPilot On-Premises Configuration
# Generated on $(date)

# Version
VERSION=latest

# Database
DB_PASSWORD=${DB_PASSWORD}

# Security
JWT_SECRET=${JWT_SECRET}
ENCRYPTION_KEY=${ENCRYPTION_KEY}

# Object Storage (MinIO)
MINIO_ACCESS_KEY=${MINIO_ACCESS_KEY}
MINIO_SECRET_KEY=${MINIO_SECRET_KEY}

# LLM Configuration
LLM_PROVIDER=ollama
OLLAMA_MODEL=codellama:34b

# TLS (set to true and provide certs for production)
TLS_ENABLED=false
EOF
    
    chmod 600 .env
    echo "✅ Secrets generated and saved to .env"
}

# Create required directories
create_directories() {
    echo ""
    echo "Creating directories..."
    
    mkdir -p certs
    mkdir -p init-scripts/postgres
    mkdir -p nginx
    mkdir -p backups
    
    echo "✅ Directories created"
}

# Generate self-signed certificates (for testing)
generate_test_certs() {
    if [ ! -f certs/server.crt ]; then
        echo ""
        echo "Generating self-signed certificates for testing..."
        
        openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
            -keyout certs/server.key \
            -out certs/server.crt \
            -subj "/CN=migrationpilot.local"
        
        echo "✅ Self-signed certificates generated"
        echo "⚠️  For production, replace with real certificates"
    fi
}

# Create nginx configuration
create_nginx_config() {
    if [ ! -f nginx/nginx.conf ]; then
        cat > nginx/nginx.conf << 'EOF'
events {
    worker_connections 1024;
}

http {
    upstream api {
        server api:3001;
    }
    
    upstream web {
        server web:3000;
    }
    
    server {
        listen 80;
        server_name _;
        return 301 https://$host$request_uri;
    }
    
    server {
        listen 443 ssl;
        server_name _;
        
        ssl_certificate /etc/nginx/certs/server.crt;
        ssl_certificate_key /etc/nginx/certs/server.key;
        ssl_protocols TLSv1.2 TLSv1.3;
        ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256;
        
        # Security headers
        add_header X-Frame-Options DENY;
        add_header X-Content-Type-Options nosniff;
        add_header X-XSS-Protection "1; mode=block";
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
        
        location /api {
            proxy_pass http://api;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection 'upgrade';
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
        }
        
        location / {
            proxy_pass http://web;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection 'upgrade';
            proxy_set_header Host $host;
            proxy_cache_bypass $http_upgrade;
        }
    }
}
EOF
        echo "✅ Nginx configuration created"
    fi
}

# Create PostgreSQL init script
create_postgres_init() {
    cat > init-scripts/postgres/01-init.sql << 'EOF'
-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- Create audit log table
CREATE TABLE IF NOT EXISTS audit_logs (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    action VARCHAR(255) NOT NULL,
    user_id VARCHAR(255),
    tenant_id VARCHAR(255),
    resource_type VARCHAR(255),
    resource_id VARCHAR(255),
    metadata JSONB,
    ip_address INET,
    user_agent TEXT
);

-- Create index for audit log queries
CREATE INDEX IF NOT EXISTS idx_audit_logs_timestamp ON audit_logs(timestamp);
CREATE INDEX IF NOT EXISTS idx_audit_logs_user_id ON audit_logs(user_id);
CREATE INDEX IF NOT EXISTS idx_audit_logs_tenant_id ON audit_logs(tenant_id);

-- Partition audit logs by month (optional, for large deployments)
-- CREATE TABLE audit_logs_y2024m01 PARTITION OF audit_logs FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');
EOF
    echo "✅ PostgreSQL init script created"
}

# Start services
start_services() {
    echo ""
    echo "Starting services..."
    
    docker compose -f docker-compose.on-prem.yml up -d
    
    echo ""
    echo "Waiting for services to be healthy..."
    sleep 10
    
    # Check health
    if docker compose -f docker-compose.on-prem.yml ps | grep -q "unhealthy"; then
        echo "⚠️  Some services are unhealthy. Check logs with:"
        echo "   docker compose -f docker-compose.on-prem.yml logs"
    else
        echo "✅ All services started successfully"
    fi
}

# Print completion message
print_completion() {
    echo ""
    echo "╔═══════════════════════════════════════════════════════════╗"
    echo "║             Installation Complete!                        ║"
    echo "╚═══════════════════════════════════════════════════════════╝"
    echo ""
    echo "Access MigrationPilot:"
    echo "  - Web UI: http://localhost:3000"
    echo "  - API: http://localhost:3001"
    echo ""
    echo "Default credentials are stored in .env"
    echo ""
    echo "Next steps:"
    echo "  1. Configure your LLM provider (Ollama, vLLM, or cloud API)"
    echo "  2. Set up TLS certificates for production"
    echo "  3. Configure backup schedule"
    echo "  4. Review security settings"
    echo ""
    echo "For local LLM with Ollama:"
    echo "  docker compose -f docker-compose.on-prem.yml --profile llm up -d"
    echo "  docker exec migrationpilot-ollama ollama pull codellama:34b"
    echo ""
    echo "Documentation: https://docs.migrationpilot.dev/on-premises"
}

# Main installation flow
main() {
    check_requirements
    generate_secrets
    create_directories
    generate_test_certs
    create_nginx_config
    create_postgres_init
    
    echo ""
    read -p "Start services now? (y/n) " -n 1 -r
    echo ""
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        start_services
    fi
    
    print_completion
}

main
