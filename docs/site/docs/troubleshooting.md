# Troubleshooting

Common issues and their solutions when using MigrationPilot.

## Installation Issues

### Node.js Version Mismatch

**Error:**
```
error @migrationpilot/core@1.0.0: The engine "node" is incompatible with this module.
```

**Solution:**
MigrationPilot requires Node.js 20+:

```bash
# Check current version
node --version

# Install Node.js 20 (using nvm)
nvm install 20
nvm use 20
```

### pnpm Installation Fails

**Error:**
```
ERR_PNPM_PEER_DEP_ISSUES
```

**Solution:**
Use correct pnpm version:

```bash
# Install pnpm 8.15+
npm install -g pnpm@8.15.0

# Clean install
rm -rf node_modules pnpm-lock.yaml
pnpm install
```

### Build Fails with Memory Error

**Error:**
```
FATAL ERROR: Reached heap limit Allocation failed
```

**Solution:**
Increase Node.js memory:

```bash
export NODE_OPTIONS="--max-old-space-size=8192"
pnpm build
```

## Analysis Issues

### File Encoding Errors

**Error:**
```
Error: Unable to decode file: LOANCALC.cbl
Invalid UTF-8 sequence at byte 0x9A
```

**Solution:**
Convert files to UTF-8 or specify encoding:

```bash
# Convert file
iconv -f ISO-8859-1 -t UTF-8 LOANCALC.cbl > LOANCALC.utf8.cbl

# Or specify encoding in API
{
  "encoding": "ISO-8859-1"
}
```

### Missing Copybooks

**Error:**
```
Warning: Unable to resolve COPY CUSTOMER-RECORD
```

**Solution:**
Upload all required copybooks:

```bash
migrationpilot project upload --project proj_xxx \
  --files "*.cbl" "*.cpy" "copylib/*.cpy"
```

### Analysis Timeout

**Error:**
```
Error: Analysis timed out after 3600 seconds
```

**Solution:**
1. Split large projects into smaller batches
2. Increase timeout:
   ```bash
   migrationpilot analyze --project proj_xxx --timeout 7200
   ```
3. Increase parallel processing:
   ```bash
   migrationpilot analyze --project proj_xxx --parallel 10
   ```

### Circular Dependencies Detected

**Error:**
```
Warning: Circular dependency detected: PROGA -> PROGB -> PROGA
```

**Solution:**
This is informational. The analyzer handles circular dependencies, but consider:
1. Reviewing the dependency graph
2. Breaking cycles in generated code via interfaces

## Migration Issues

### Low Confidence Rules Blocking Migration

**Error:**
```
Error: Cannot proceed - 15 rules below confidence threshold (0.80)
```

**Solution:**

Option 1: Review and approve rules
```bash
migrationpilot rules list --project proj_xxx --status pending
migrationpilot rules approve --rule rule_xxx
```

Option 2: Lower threshold (not recommended for production)
```bash
migrationpilot migrate --project proj_xxx --confidence-threshold 0.70
```

### Memory Exhaustion During Generation

**Error:**
```
JavaScript heap out of memory
```

**Solution:**
1. Increase memory:
   ```bash
   export NODE_OPTIONS="--max-old-space-size=16384"
   ```
2. Process in batches:
   ```bash
   migrationpilot migrate --project proj_xxx --batch-size 50
   ```

### Invalid Target Framework

**Error:**
```
Error: Unknown target framework: SpringBoot 4
```

**Solution:**
Check supported frameworks:

```bash
migrationpilot config show frameworks

# Supported for Java:
# - Spring Boot (2.7, 3.0, 3.1, 3.2)
# - Quarkus (3.x)
# - Micronaut (4.x)
```

## Validation Issues

### Test Failures Due to Floating Point Precision

**Error:**
```
Expected: 100.56
Actual:   100.55999999999999
```

**Solution:**
Configure numeric comparison tolerance:

```bash
migrationpilot validate --project proj_xxx --tolerance 0.001
```

Or fix rounding in generated code:
```java
// Before
return principal * rate;

// After
return BigDecimal.valueOf(principal)
    .multiply(BigDecimal.valueOf(rate))
    .setScale(2, RoundingMode.HALF_UP)
    .doubleValue();
```

### Date/Time Comparison Failures

**Error:**
```
Expected: 2024-01-28T00:00:00
Actual:   2024-01-27T23:00:00
```

**Solution:**
Check timezone handling:

1. Ensure consistent timezone:
   ```bash
   export TZ=UTC
   ```

2. Configure date comparison in validation:
   ```bash
   migrationpilot validate --project proj_xxx --ignore-timezone
   ```

### Null Handling Differences

**Error:**
```
Expected: "" (empty string)
Actual:   null
```

**Solution:**
Legacy systems often use empty strings instead of null. Options:

1. Configure null handling:
   ```bash
   migrationpilot migrate --project proj_xxx --null-as-empty
   ```

2. Modify generated code to match legacy behavior

## CLI Issues

### Authentication Failures

**Error:**
```
Error: Unauthorized - Invalid or expired token
```

**Solution:**
Refresh authentication:

```bash
# Re-authenticate
migrationpilot config set apiKey <new-key>

# Or re-login
migrationpilot auth login
```

### Connection Refused

**Error:**
```
Error: connect ECONNREFUSED 127.0.0.1:4000
```

**Solution:**
1. Check if API is running:
   ```bash
   curl http://localhost:4000/health
   ```
2. Start the API:
   ```bash
   pnpm dev
   ```
3. Check API URL configuration:
   ```bash
   migrationpilot config show
   ```

### Command Not Found

**Error:**
```
bash: migrationpilot: command not found
```

**Solution:**
```bash
# Option 1: Install globally
pnpm add -g @migrationpilot/cli

# Option 2: Use npx
npx @migrationpilot/cli --help

# Option 3: Add to PATH
export PATH="$PATH:./node_modules/.bin"
```

## Database Issues

### Migration Schema Errors

**Error:**
```
Error: relation "projects" does not exist
```

**Solution:**
Run database migrations:

```bash
cd packages/database
pnpm db:push
```

### Connection Pool Exhausted

**Error:**
```
Error: Too many clients already
```

**Solution:**
1. Increase pool size in `.env`:
   ```
   DATABASE_POOL_SIZE=20
   ```
2. Ensure connections are released properly
3. Check for connection leaks

## Docker Issues

### Container Fails to Start

**Error:**
```
Error: Cannot start container: port is already allocated
```

**Solution:**
```bash
# Find and stop conflicting container
docker ps --filter "publish=4000"
docker stop <container_id>

# Or use different port
docker run -p 4001:4000 migrationpilot/api
```

### Out of Disk Space

**Error:**
```
Error: no space left on device
```

**Solution:**
```bash
# Clean up Docker
docker system prune -a

# Check disk usage
docker system df
```

## Performance Issues

### Slow Analysis

**Symptoms:**
- Analysis taking hours for small projects
- High CPU usage but slow progress

**Solutions:**
1. Check file count and sizes
2. Reduce parallel processing if memory-constrained:
   ```bash
   migrationpilot analyze --project proj_xxx --parallel 2
   ```
3. Use SSD storage for working files

### API Response Timeout

**Error:**
```
Error: Request timeout after 30000ms
```

**Solution:**
1. Increase timeout:
   ```bash
   migrationpilot config set timeout 120000
   ```
2. Use async operations for long-running tasks:
   ```bash
   migrationpilot analyze --project proj_xxx --async
   migrationpilot status --job job_xxx
   ```

## Getting More Help

### Enable Debug Logging

```bash
# CLI
export DEBUG=migrationpilot:*
migrationpilot analyze --project proj_xxx

# API
LOG_LEVEL=debug pnpm dev
```

### Collect Diagnostic Information

```bash
# System info
migrationpilot diagnose > diagnostics.txt

# Includes:
# - Version numbers
# - Configuration
# - Environment
# - Recent errors
```

### Report an Issue

When reporting issues, include:

1. **Command run** (sanitize sensitive data)
2. **Full error message**
3. **MigrationPilot version**: `migrationpilot --version`
4. **Node.js version**: `node --version`
5. **Operating system**
6. **Diagnostic output**

## Related Topics

- [FAQ](./faq.md) - Common questions
- [CLI Config](./cli/config.md) - Configuration reference
- [Security](./deployment/security.md) - Security settings
