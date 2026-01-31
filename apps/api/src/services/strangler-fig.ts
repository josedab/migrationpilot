/**
 * Strangler Fig Service
 * 
 * Manages incremental migration with traffic routing and shadow mode testing
 */

export interface RoutingRule {
  id: string;
  projectId: string;
  name: string;
  pathPattern: string;
  targetService: 'legacy' | 'modern';
  trafficPercentage: number;
  shadowMode: boolean;
  enabled: boolean;
}

export interface RoutingDecision {
  target: 'legacy' | 'modern';
  shadow?: 'legacy' | 'modern';
  rule?: RoutingRule;
  reason: string;
}

export interface ShadowResult {
  legacyResponse: unknown;
  modernResponse: unknown;
  legacyDuration: number;
  modernDuration: number;
  equivalent: boolean;
  differences?: string[];
}

// In-memory routing rules (use database in production)
const routingRules = new Map<string, RoutingRule>();

export class StranglerFigService {
  
  /**
   * Create a new routing rule
   */
  createRule(rule: Omit<RoutingRule, 'id'>): RoutingRule {
    const id = `rule_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
    const newRule: RoutingRule = { ...rule, id };
    routingRules.set(id, newRule);
    return newRule;
  }

  /**
   * Get all routing rules for a project
   */
  getRules(projectId: string): RoutingRule[] {
    return Array.from(routingRules.values())
      .filter(r => r.projectId === projectId);
  }

  /**
   * Update a routing rule
   */
  updateRule(id: string, updates: Partial<RoutingRule>): RoutingRule | null {
    const rule = routingRules.get(id);
    if (!rule) return null;
    
    const updated = { ...rule, ...updates };
    routingRules.set(id, updated);
    return updated;
  }

  /**
   * Delete a routing rule
   */
  deleteRule(id: string): boolean {
    return routingRules.delete(id);
  }

  /**
   * Determine routing for a request
   */
  routeRequest(projectId: string, path: string, headers?: Record<string, string>): RoutingDecision {
    const rules = this.getRules(projectId)
      .filter(r => r.enabled)
      .sort((a, b) => b.pathPattern.length - a.pathPattern.length); // More specific patterns first

    for (const rule of rules) {
      if (this.matchPath(path, rule.pathPattern)) {
        const target = this.selectTarget(rule, headers);
        
        return {
          target,
          shadow: rule.shadowMode ? (target === 'legacy' ? 'modern' : 'legacy') : undefined,
          rule,
          reason: `Matched rule: ${rule.name}`,
        };
      }
    }

    // Default to legacy if no rules match
    return {
      target: 'legacy',
      reason: 'No matching rules, defaulting to legacy',
    };
  }

  /**
   * Match path against pattern
   */
  private matchPath(path: string, pattern: string): boolean {
    // Convert glob pattern to regex
    const regex = pattern
      .replace(/\*/g, '.*')
      .replace(/\?/g, '.')
      .replace(/\//g, '\\/');
    
    return new RegExp(`^${regex}$`).test(path);
  }

  /**
   * Select target based on traffic percentage
   */
  private selectTarget(rule: RoutingRule, headers?: Record<string, string>): 'legacy' | 'modern' {
    // Check for override header (for testing)
    if (headers?.['x-migrationpilot-target']) {
      const override = headers['x-migrationpilot-target'] as 'legacy' | 'modern';
      if (override === 'legacy' || override === 'modern') {
        return override;
      }
    }

    // Use sticky routing based on user/session ID for consistency
    const stickiness = headers?.['x-user-id'] || headers?.['x-session-id'] || Math.random().toString();
    const hash = this.hashString(stickiness);
    const bucket = hash % 100;

    return bucket < rule.trafficPercentage ? 'modern' : 'legacy';
  }

  /**
   * Simple string hash
   */
  private hashString(str: string): number {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32bit integer
    }
    return Math.abs(hash);
  }

  /**
   * Execute shadow mode request
   */
  async executeShadow(
    legacyEndpoint: string,
    modernEndpoint: string,
    request: {
      method: string;
      path: string;
      headers: Record<string, string>;
      body?: unknown;
    }
  ): Promise<ShadowResult> {
    const fetchOptions = {
      method: request.method,
      headers: { ...request.headers, 'Content-Type': 'application/json' },
      body: request.body ? JSON.stringify(request.body) : undefined,
    };

    // Execute both requests in parallel
    const [legacyResult, modernResult] = await Promise.all([
      this.timedFetch(legacyEndpoint + request.path, fetchOptions),
      this.timedFetch(modernEndpoint + request.path, fetchOptions),
    ]);

    // Compare responses
    const equivalent = this.compareResponses(legacyResult.data, modernResult.data);

    return {
      legacyResponse: legacyResult.data,
      modernResponse: modernResult.data,
      legacyDuration: legacyResult.duration,
      modernDuration: modernResult.duration,
      equivalent: equivalent.equal,
      differences: equivalent.differences,
    };
  }

  /**
   * Fetch with timing
   */
  private async timedFetch(
    url: string,
    options: RequestInit
  ): Promise<{ data: unknown; duration: number }> {
    const start = Date.now();
    
    try {
      const response = await fetch(url, {
        ...options,
        signal: AbortSignal.timeout(30000),
      });
      const data = await response.json();
      return { data, duration: Date.now() - start };
    } catch (error) {
      return {
        data: { error: error instanceof Error ? error.message : 'Request failed' },
        duration: Date.now() - start,
      };
    }
  }

  /**
   * Compare two responses for equivalence
   */
  private compareResponses(
    legacy: unknown,
    modern: unknown
  ): { equal: boolean; differences: string[] } {
    const differences: string[] = [];

    const compare = (path: string, a: unknown, b: unknown): void => {
      if (typeof a !== typeof b) {
        differences.push(`${path}: type mismatch (${typeof a} vs ${typeof b})`);
        return;
      }

      if (a === null || b === null) {
        if (a !== b) differences.push(`${path}: null mismatch`);
        return;
      }

      if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length) {
          differences.push(`${path}: array length mismatch (${a.length} vs ${b.length})`);
        }
        const minLen = Math.min(a.length, b.length);
        for (let i = 0; i < minLen; i++) {
          compare(`${path}[${i}]`, a[i], b[i]);
        }
        return;
      }

      if (typeof a === 'object' && typeof b === 'object') {
        const keysA = Object.keys(a as Record<string, unknown>);
        const keysB = Object.keys(b as Record<string, unknown>);
        const allKeys = new Set([...keysA, ...keysB]);
        
        for (const key of allKeys) {
          compare(
            `${path}.${key}`,
            (a as Record<string, unknown>)[key],
            (b as Record<string, unknown>)[key]
          );
        }
        return;
      }

      // Primitive comparison
      if (a !== b) {
        // Allow numeric tolerance
        if (typeof a === 'number' && typeof b === 'number') {
          const tolerance = 0.0001;
          if (Math.abs(a - b) > tolerance) {
            differences.push(`${path}: ${a} !== ${b}`);
          }
        } else {
          differences.push(`${path}: ${JSON.stringify(a)} !== ${JSON.stringify(b)}`);
        }
      }
    };

    compare('$', legacy, modern);

    return { equal: differences.length === 0, differences };
  }

  /**
   * Generate API facade code for gradual migration
   */
  generateFacade(projectId: string, targetLanguage: 'java' | 'python' | 'typescript'): string {
    const rules = this.getRules(projectId);
    
    switch (targetLanguage) {
      case 'typescript':
        return this.generateTypeScriptFacade(rules);
      case 'java':
        return this.generateJavaFacade(rules);
      case 'python':
        return this.generatePythonFacade(rules);
      default:
        return '// Unsupported language';
    }
  }

  private generateTypeScriptFacade(rules: RoutingRule[]): string {
    return `/**
 * API Facade for Strangler Fig Migration
 * Generated by MigrationPilot
 */

import express from 'express';
import { createProxyMiddleware } from 'http-proxy-middleware';

const app = express();

const LEGACY_URL = process.env.LEGACY_URL || 'http://legacy:8080';
const MODERN_URL = process.env.MODERN_URL || 'http://modern:3000';

// Routing configuration
const routes = ${JSON.stringify(rules, null, 2)};

// Route decision function
function getTarget(path: string, userId?: string): string {
  for (const rule of routes) {
    if (!rule.enabled) continue;
    if (new RegExp(rule.pathPattern.replace(/\\*/g, '.*')).test(path)) {
      // Use consistent hashing for user stickiness
      const hash = userId ? hashCode(userId) : Math.random() * 100;
      return (hash % 100) < rule.trafficPercentage ? MODERN_URL : LEGACY_URL;
    }
  }
  return LEGACY_URL;
}

function hashCode(str: string): number {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    hash = ((hash << 5) - hash) + str.charCodeAt(i);
  }
  return Math.abs(hash);
}

// Dynamic proxy middleware
app.use('/', (req, res, next) => {
  const target = getTarget(req.path, req.headers['x-user-id'] as string);
  createProxyMiddleware({ target, changeOrigin: true })(req, res, next);
});

app.listen(8000, () => console.log('Facade running on :8000'));
`;
  }

  private generateJavaFacade(rules: RoutingRule[]): string {
    return `/**
 * API Facade for Strangler Fig Migration
 * Generated by MigrationPilot
 */
package com.migrationpilot.facade;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.gateway.route.RouteLocator;
import org.springframework.cloud.gateway.route.builder.RouteLocatorBuilder;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class FacadeApplication {

    private static final String LEGACY_URL = System.getenv().getOrDefault("LEGACY_URL", "http://legacy:8080");
    private static final String MODERN_URL = System.getenv().getOrDefault("MODERN_URL", "http://modern:3000");

    public static void main(String[] args) {
        SpringApplication.run(FacadeApplication.class, args);
    }

    @Bean
    public RouteLocator customRouteLocator(RouteLocatorBuilder builder) {
        return builder.routes()
${rules.filter(r => r.enabled).map(r => `            .route("${r.name}", p -> p
                .path("${r.pathPattern}")
                .filters(f -> f.filter(new TrafficSplitFilter(${r.trafficPercentage})))
                .uri(${r.trafficPercentage > 50 ? 'MODERN_URL' : 'LEGACY_URL'}))`).join('\n')}
            .build();
    }
}
`;
  }

  private generatePythonFacade(rules: RoutingRule[]): string {
    return `"""
API Facade for Strangler Fig Migration
Generated by MigrationPilot
"""
import os
import re
import hashlib
from fastapi import FastAPI, Request
import httpx

app = FastAPI()

LEGACY_URL = os.getenv("LEGACY_URL", "http://legacy:8080")
MODERN_URL = os.getenv("MODERN_URL", "http://modern:3000")

routes = ${JSON.stringify(rules, null, 4)}

def get_target(path: str, user_id: str = None) -> str:
    for rule in routes:
        if not rule["enabled"]:
            continue
        pattern = rule["pathPattern"].replace("*", ".*")
        if re.match(pattern, path):
            if user_id:
                hash_val = int(hashlib.md5(user_id.encode()).hexdigest(), 16) % 100
            else:
                import random
                hash_val = random.randint(0, 99)
            return MODERN_URL if hash_val < rule["trafficPercentage"] else LEGACY_URL
    return LEGACY_URL

@app.api_route("/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH"])
async def proxy(request: Request, path: str):
    target = get_target(f"/{path}", request.headers.get("x-user-id"))
    async with httpx.AsyncClient() as client:
        resp = await client.request(
            method=request.method,
            url=f"{target}/{path}",
            headers=dict(request.headers),
            content=await request.body()
        )
        return resp.json()
`;
  }
}

export const stranglerFigService = new StranglerFigService();
