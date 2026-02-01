#!/bin/bash
# Build all packages and apps

set -e

echo "🔨 Building MigrationPilot..."

pnpm install
pnpm build

echo "✅ Build complete!"
