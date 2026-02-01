#!/bin/bash
# Start development environment

set -e

echo "🚀 Starting MigrationPilot development environment..."

# Check prerequisites
command -v docker >/dev/null 2>&1 || { echo "❌ Docker required"; exit 1; }
command -v pnpm >/dev/null 2>&1 || { echo "❌ pnpm required"; exit 1; }

# Start infrastructure
echo "📦 Starting infrastructure (PostgreSQL, Redis, MinIO)..."
docker compose -f infrastructure/docker/docker-compose.yml up -d postgres redis minio

# Wait for services
echo "⏳ Waiting for services..."
sleep 5

# Run migrations
echo "🗄️ Running database migrations..."
pnpm --filter @migrationpilot/database db:push 2>/dev/null || echo "Migrations skipped (schema not applied)"

# Start dev servers
echo "🔥 Starting development servers..."
pnpm dev

echo "✅ Development environment ready!"
echo "   Web: http://localhost:3000"
echo "   API: http://localhost:3001"
