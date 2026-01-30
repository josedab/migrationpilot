# Strangler Fig Migration Pattern

The Strangler Fig pattern enables gradual, low-risk migration from legacy systems to modern architecture. Named after the strangler fig tree that grows around a host tree, this pattern allows the new system to gradually replace the old one.

## Overview

Instead of a "big bang" rewrite, the Strangler Fig pattern:

1. **Routes traffic** through a facade layer
2. **Migrates incrementally** - one feature at a time
3. **Validates equivalence** before switching traffic
4. **Rolls back easily** if issues arise
5. **Decommissions legacy** only when fully replaced

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Load Balancer                          │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                   Strangler Facade                          │
│  ┌─────────────────────────────────────────────────────┐    │
│  │             Traffic Router                          │    │
│  │  /api/loans/*     → 100% Modern                     │    │
│  │  /api/accounts/*  → 70% Modern, 30% Legacy          │    │
│  │  /api/reports/*   → 100% Legacy                     │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────┬───────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          │               │               │
┌─────────▼─────┐ ┌───────▼──────┐ ┌──────▼──────┐
│   Modern      │ │   Shadow     │ │   Legacy    │
│   Service     │ │   Recorder   │ │   System    │
│  (Java/TS)    │ │              │ │   (COBOL)   │
└───────────────┘ └──────────────┘ └─────────────┘
```

## Enable Strangler Fig in MigrationPilot

### Project Configuration

```bash
migrationpilot project create banking-migration \
  --language cobol \
  --target java
```

During project creation or in settings, enable the Strangler Fig pattern:

```json
{
  "settings": {
    "enableStranglerFig": true,
    "stranglerConfig": {
      "defaultTrafficSplit": 0,
      "shadowMode": true,
      "validateBeforeSwitch": true
    }
  }
}
```

### Via Web Dashboard

1. Navigate to **Project Settings**
2. Enable **Strangler Fig Pattern**
3. Configure:
   - Shadow mode (record but don't serve)
   - Traffic split percentages
   - Validation thresholds

## Phase 1: Setup Facade

MigrationPilot generates a facade layer that intercepts all requests:

### Generated Facade (Node.js/Express)

```typescript
// strangler-facade/src/router.ts

import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { RoutingConfig, RouteRule } from './types';
import { ShadowRecorder } from './shadow-recorder';

export class StranglerRouter {
  private config: RoutingConfig;
  private shadowRecorder: ShadowRecorder;

  constructor(config: RoutingConfig) {
    this.config = config;
    this.shadowRecorder = new ShadowRecorder();
  }

  createRouter(): express.Router {
    const router = express.Router();

    for (const rule of this.config.rules) {
      router.use(rule.pathPattern, this.createRouteHandler(rule));
    }

    return router;
  }

  private createRouteHandler(rule: RouteRule): express.RequestHandler {
    return async (req, res, next) => {
      const target = this.selectTarget(rule);
      
      if (rule.shadowMode) {
        // Send to both, return legacy response
        await this.shadowRequest(req, rule);
      }

      // Proxy to selected target
      const proxy = createProxyMiddleware({
        target: target === 'modern' ? this.config.modernUrl : this.config.legacyUrl,
        changeOrigin: true,
      });

      proxy(req, res, next);
    };
  }

  private selectTarget(rule: RouteRule): 'modern' | 'legacy' {
    if (rule.trafficPercentage === 0) return 'legacy';
    if (rule.trafficPercentage === 100) return 'modern';
    
    // Weighted random selection
    return Math.random() * 100 < rule.trafficPercentage ? 'modern' : 'legacy';
  }

  private async shadowRequest(req: express.Request, rule: RouteRule): Promise<void> {
    // Record request to modern service without affecting response
    await this.shadowRecorder.record(req, this.config.modernUrl);
  }
}
```

### Routing Configuration

```typescript
// strangler-facade/src/config.ts

export const routingConfig: RoutingConfig = {
  legacyUrl: process.env.LEGACY_URL || 'http://legacy-system:8080',
  modernUrl: process.env.MODERN_URL || 'http://modern-service:3000',
  
  rules: [
    {
      name: 'Loan Calculations',
      pathPattern: '/api/loans/*',
      targetService: 'modern',
      trafficPercentage: 100,
      shadowMode: false,
    },
    {
      name: 'Account Management',
      pathPattern: '/api/accounts/*',
      targetService: 'modern',
      trafficPercentage: 50,  // 50% canary
      shadowMode: false,
    },
    {
      name: 'Reports',
      pathPattern: '/api/reports/*',
      targetService: 'legacy',
      trafficPercentage: 0,
      shadowMode: true,  // Record modern responses for comparison
    },
  ],
};
```

## Phase 2: Shadow Mode

Before switching traffic, run in shadow mode to validate behavior:

### Shadow Recorder

```typescript
// strangler-facade/src/shadow-recorder.ts

import axios from 'axios';
import { DifferentialAnalyzer } from './differential-analyzer';

export class ShadowRecorder {
  private analyzer: DifferentialAnalyzer;

  constructor() {
    this.analyzer = new DifferentialAnalyzer();
  }

  async record(
    req: express.Request,
    legacyResponse: any,
    modernUrl: string
  ): Promise<void> {
    try {
      // Replay request to modern service
      const modernResponse = await axios({
        method: req.method,
        url: `${modernUrl}${req.path}`,
        headers: this.sanitizeHeaders(req.headers),
        data: req.body,
      });

      // Compare responses
      const comparison = this.analyzer.compare(
        legacyResponse,
        modernResponse.data
      );

      // Log results
      await this.logComparison({
        timestamp: new Date(),
        path: req.path,
        method: req.method,
        legacyStatus: 200,
        modernStatus: modernResponse.status,
        equivalent: comparison.equivalent,
        differences: comparison.differences,
      });

    } catch (error) {
      await this.logError(req, error);
    }
  }

  private sanitizeHeaders(headers: any): Record<string, string> {
    const sanitized = { ...headers };
    delete sanitized['host'];
    delete sanitized['content-length'];
    return sanitized;
  }
}
```

### Differential Analyzer

```typescript
// strangler-facade/src/differential-analyzer.ts

export class DifferentialAnalyzer {
  compare(legacy: any, modern: any): ComparisonResult {
    const differences: Difference[] = [];
    
    this.deepCompare(legacy, modern, '', differences);
    
    return {
      equivalent: differences.length === 0,
      differences,
      similarity: this.calculateSimilarity(legacy, modern),
    };
  }

  private deepCompare(
    legacy: any,
    modern: any,
    path: string,
    differences: Difference[]
  ): void {
    // Handle nulls
    if (legacy === null && modern === null) return;
    if (legacy === null || modern === null) {
      differences.push({ path, legacy, modern, type: 'null_mismatch' });
      return;
    }

    // Handle primitives
    if (typeof legacy !== typeof modern) {
      differences.push({ path, legacy, modern, type: 'type_mismatch' });
      return;
    }

    if (typeof legacy !== 'object') {
      if (!this.valuesEqual(legacy, modern)) {
        differences.push({ path, legacy, modern, type: 'value_mismatch' });
      }
      return;
    }

    // Handle arrays
    if (Array.isArray(legacy)) {
      if (!Array.isArray(modern)) {
        differences.push({ path, legacy, modern, type: 'array_mismatch' });
        return;
      }
      for (let i = 0; i < Math.max(legacy.length, modern.length); i++) {
        this.deepCompare(legacy[i], modern[i], `${path}[${i}]`, differences);
      }
      return;
    }

    // Handle objects
    const allKeys = new Set([
      ...Object.keys(legacy),
      ...Object.keys(modern),
    ]);
    
    for (const key of allKeys) {
      this.deepCompare(
        legacy[key],
        modern[key],
        path ? `${path}.${key}` : key,
        differences
      );
    }
  }

  private valuesEqual(a: any, b: any): boolean {
    // Handle numeric comparison with tolerance
    if (typeof a === 'number' && typeof b === 'number') {
      return Math.abs(a - b) < 0.0001;
    }
    return a === b;
  }
}
```

## Phase 3: Gradual Traffic Shift

### CLI Commands

```bash
# View current routing
migrationpilot strangler status --project banking-migration

# Increase modern traffic for accounts
migrationpilot strangler set-traffic \
  --project banking-migration \
  --route "/api/accounts/*" \
  --percentage 75

# Enable 100% modern for loans
migrationpilot strangler set-traffic \
  --project banking-migration \
  --route "/api/loans/*" \
  --percentage 100
```

### API Endpoints

```bash
# Get routing status
curl http://localhost:4000/api/projects/PROJECT_ID/routing-rules

# Update traffic percentage
curl -X PATCH http://localhost:4000/api/projects/PROJECT_ID/routing-rules/RULE_ID \
  -H "Content-Type: application/json" \
  -d '{"trafficPercentage": 75}'

# Enable shadow mode
curl -X PATCH http://localhost:4000/api/projects/PROJECT_ID/routing-rules/RULE_ID \
  -H "Content-Type: application/json" \
  -d '{"shadowMode": true}'
```

## Phase 4: Monitor and Validate

### Dashboard Metrics

The MigrationPilot dashboard shows:

- **Traffic Distribution**: % of requests to each system
- **Response Time Comparison**: Legacy vs Modern latency
- **Error Rates**: By route and system
- **Equivalence Score**: % of matching responses

### Validation Reports

```bash
# Generate equivalence report
migrationpilot strangler report \
  --project banking-migration \
  --start-date 2024-01-01 \
  --end-date 2024-01-31 \
  --output report.json
```

### Alerting

Configure alerts for:

```yaml
# alerts.yaml
alerts:
  - name: HighErrorRate
    condition: modern_error_rate > 1%
    action: rollback_to_legacy
    
  - name: ResponseTimeDegradation
    condition: modern_p99 > legacy_p99 * 1.5
    action: notify
    
  - name: EquivalenceDrift
    condition: equivalence_score < 95%
    action: pause_traffic_increase
```

## Phase 5: Complete Migration

### Decommission Checklist

1. **All routes at 100% modern** ✓
2. **Shadow mode disabled** ✓
3. **No errors for 30 days** ✓
4. **Equivalence score > 99%** ✓
5. **Performance acceptable** ✓

### Remove Legacy

```bash
# Mark migration complete
migrationpilot strangler complete --project banking-migration

# Archive legacy routing rules
migrationpilot strangler archive --project banking-migration

# Update infrastructure to bypass facade
```

## Best Practices

### 1. Start with Low-Risk Routes

Begin with read-only endpoints or less critical functionality.

### 2. Monitor Closely During Canary

Watch metrics carefully when increasing traffic percentages.

### 3. Have Rollback Ready

Always be prepared to quickly revert to legacy:

```bash
# Emergency rollback
migrationpilot strangler rollback \
  --project banking-migration \
  --route "/api/*"
```

### 4. Use Feature Flags

Combine with feature flags for fine-grained control:

```typescript
if (featureFlag('use_modern_loans') && trafficSelected('modern')) {
  return modernService.calculateLoan(request);
}
return legacyService.calculateLoan(request);
```

### 5. Document Everything

Keep detailed records of:
- Migration decisions
- Traffic changes with dates
- Issues encountered
- Rollback incidents

## Troubleshooting

### Shadow Mode Differences

If shadow mode shows differences:

1. Check data type conversions
2. Verify date/time formatting
3. Compare numeric precision
4. Review null handling

### Performance Issues

If modern service is slower:

1. Check database queries
2. Review N+1 problems
3. Add caching where needed
4. Optimize hot paths

### Rollback Procedures

If issues arise:

```bash
# Immediate rollback (all traffic to legacy)
migrationpilot strangler emergency-rollback --project banking-migration

# Gradual rollback
migrationpilot strangler set-traffic \
  --project banking-migration \
  --route "/api/accounts/*" \
  --percentage 0
```

## Next Steps

- [Set up monitoring](../deployment/kubernetes.md)
- [Configure alerting](../deployment/security.md)
- [Review validation strategies](../concepts/equivalence-testing.md)
