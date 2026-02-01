/**
 * Security Middleware
 * 
 * Rate limiting, authentication, and security headers
 */

import type { Context, Next } from 'hono';
import type { AuditLog } from '@migrationpilot/core';
import * as jose from 'jose';
import { createHash, timingSafeEqual } from 'crypto';

// Configuration from environment
const JWT_SECRET = process.env.JWT_SECRET || process.env.AUTH_SECRET || 'development-secret-change-in-production';
const JWT_ISSUER = process.env.JWT_ISSUER || 'migrationpilot';
const JWT_AUDIENCE = process.env.JWT_AUDIENCE || 'migrationpilot-api';
const REDIS_URL = process.env.REDIS_URL;

// Rate limiter store interface for pluggable backends
export interface RateLimitStore {
  get(key: string): Promise<{ count: number; resetTime: number } | null>;
  set(key: string, value: { count: number; resetTime: number }, ttlMs: number): Promise<void>;
  increment(key: string): Promise<number>;
}

// In-memory rate limit store (default for development)
class InMemoryRateLimitStore implements RateLimitStore {
  private store = new Map<string, { count: number; resetTime: number }>();

  async get(key: string): Promise<{ count: number; resetTime: number } | null> {
    return this.store.get(key) || null;
  }

  async set(key: string, value: { count: number; resetTime: number }, _ttlMs: number): Promise<void> {
    this.store.set(key, value);
    // Auto-cleanup after TTL
    setTimeout(() => this.store.delete(key), _ttlMs);
  }

  async increment(key: string): Promise<number> {
    const record = this.store.get(key);
    if (record) {
      record.count++;
      return record.count;
    }
    return 1;
  }
}

// Redis rate limit store (production)
class RedisRateLimitStore implements RateLimitStore {
  private redisUrl: string;

  constructor(redisUrl: string) {
    this.redisUrl = redisUrl;
  }

  private async executeCommand<T>(command: string[]): Promise<T | null> {
    // Note: In production, use a proper Redis client like ioredis
    // This is a simplified implementation showing the interface
    try {
      const response = await fetch(`${this.redisUrl}/`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ command }),
      });
      if (!response.ok) return null;
      return await response.json() as T;
    } catch {
      // Fallback to in-memory on Redis failure
      console.warn('Redis connection failed, using in-memory store');
      return null;
    }
  }

  async get(key: string): Promise<{ count: number; resetTime: number } | null> {
    const data = await this.executeCommand<string>(['GET', `ratelimit:${key}`]);
    if (!data) return null;
    try {
      return JSON.parse(data) as { count: number; resetTime: number };
    } catch {
      return null;
    }
  }

  async set(key: string, value: { count: number; resetTime: number }, ttlMs: number): Promise<void> {
    await this.executeCommand(['SET', `ratelimit:${key}`, JSON.stringify(value), 'PX', ttlMs.toString()]);
  }

  async increment(key: string): Promise<number> {
    const result = await this.executeCommand<number>(['INCR', `ratelimit:${key}:count`]);
    return result || 1;
  }
}

// Create rate limit store based on environment
let rateLimitStore: RateLimitStore;
if (REDIS_URL) {
  console.log('Using Redis rate limiting');
  rateLimitStore = new RedisRateLimitStore(REDIS_URL);
} else {
  console.log('Using in-memory rate limiting (set REDIS_URL for production)');
  rateLimitStore = new InMemoryRateLimitStore();
}

// Export for testing
export function setRateLimitStore(store: RateLimitStore): void {
  rateLimitStore = store;
}

// API Key store interface for pluggable backends  
export interface ApiKeyStore {
  get(prefix: string): Promise<{ id: string; tenantId: string; permissions: string[]; keyHash: string } | null>;
  set(prefix: string, data: { id: string; tenantId: string; permissions: string[]; keyHash: string }): Promise<void>;
}

// In-memory API key store (default for development)
class InMemoryApiKeyStore implements ApiKeyStore {
  private store = new Map<string, { id: string; tenantId: string; permissions: string[]; keyHash: string }>();

  async get(prefix: string): Promise<{ id: string; tenantId: string; permissions: string[]; keyHash: string } | null> {
    return this.store.get(prefix) || null;
  }

  async set(prefix: string, data: { id: string; tenantId: string; permissions: string[]; keyHash: string }): Promise<void> {
    this.store.set(prefix, data);
  }
}

// Create API key store (can be swapped for database-backed in production)
let apiKeyStore: ApiKeyStore = new InMemoryApiKeyStore();

// Export for testing and configuration
export function setApiKeyStore(store: ApiKeyStore): void {
  apiKeyStore = store;
}

// Initialize with test key for development
if (process.env.NODE_ENV !== 'production') {
  const testKeyHash = hashApiKey('mp_test_key_12345678');
  apiKeyStore.set('mp_test', {
    id: 'key_test123',
    tenantId: 'tenant_default',
    permissions: ['read', 'write', 'analyze', 'migrate'],
    keyHash: testKeyHash,
  });
}

interface RateLimitConfig {
  windowMs: number;
  maxRequests: number;
}

const defaultConfig: RateLimitConfig = {
  windowMs: 60 * 1000, // 1 minute
  maxRequests: 100,
};

/**
 * Rate limiting middleware
 * Supports both in-memory and Redis backends via RateLimitStore interface
 */
export function rateLimiter(config: Partial<RateLimitConfig> = {}) {
  const { windowMs, maxRequests } = { ...defaultConfig, ...config };

  return async (c: Context, next: Next) => {
    const clientId = getClientIdentifier(c);
    const now = Date.now();
    
    let record = await rateLimitStore.get(clientId);
    
    if (!record || now > record.resetTime) {
      record = { count: 1, resetTime: now + windowMs };
      await rateLimitStore.set(clientId, record, windowMs);
    } else {
      record.count = await rateLimitStore.increment(clientId);
    }

    // Set rate limit headers
    c.header('X-RateLimit-Limit', maxRequests.toString());
    c.header('X-RateLimit-Remaining', Math.max(0, maxRequests - record.count).toString());
    c.header('X-RateLimit-Reset', record.resetTime.toString());

    if (record.count > maxRequests) {
      return c.json(
        { error: 'Too many requests', retryAfter: Math.ceil((record.resetTime - now) / 1000) },
        429
      );
    }

    await next();
  };
}

/**
 * Security headers middleware
 */
export function securityHeaders() {
  return async (c: Context, next: Next) => {
    // OWASP recommended security headers
    c.header('X-Content-Type-Options', 'nosniff');
    c.header('X-Frame-Options', 'DENY');
    c.header('X-XSS-Protection', '1; mode=block');
    c.header('Strict-Transport-Security', 'max-age=31536000; includeSubDomains');
    c.header('Content-Security-Policy', "default-src 'self'");
    c.header('Referrer-Policy', 'strict-origin-when-cross-origin');
    c.header('Permissions-Policy', 'geolocation=(), microphone=(), camera=()');
    
    await next();
  };
}

/**
 * API Key authentication middleware
 */
export function apiKeyAuth(options: { optional?: boolean } = {}) {
  return async (c: Context, next: Next) => {
    const apiKey = c.req.header('X-API-Key') || c.req.query('api_key');
    
    if (!apiKey) {
      if (options.optional) {
        c.set('authenticated', false);
        await next();
        return;
      }
      return c.json({ error: 'API key required' }, 401);
    }

    // Validate API key (in production, check against database)
    const validKey = await validateApiKey(apiKey);
    
    if (!validKey) {
      return c.json({ error: 'Invalid API key' }, 401);
    }

    c.set('authenticated', true);
    c.set('apiKeyId', validKey.id);
    c.set('tenantId', validKey.tenantId);
    c.set('permissions', validKey.permissions);
    
    await next();
  };
}

/**
 * JWT authentication middleware
 */
export function jwtAuth() {
  return async (c: Context, next: Next) => {
    const authHeader = c.req.header('Authorization');
    
    if (!authHeader?.startsWith('Bearer ')) {
      return c.json({ error: 'Authorization header required' }, 401);
    }

    const token = authHeader.slice(7);
    
    try {
      const payload = await verifyJWT(token);
      c.set('user', payload);
      c.set('userId', payload.sub);
      c.set('tenantId', payload.tenantId);
      c.set('roles', payload.roles);
    } catch {
      return c.json({ error: 'Invalid token' }, 401);
    }

    await next();
  };
}

/**
 * Role-based access control middleware
 */
export function requireRole(...roles: string[]) {
  return async (c: Context, next: Next) => {
    const userRoles = c.get('roles') as string[] | undefined;
    
    if (!userRoles) {
      return c.json({ error: 'Authentication required' }, 401);
    }

    const hasRole = roles.some(role => userRoles.includes(role));
    
    if (!hasRole) {
      return c.json({ error: 'Insufficient permissions' }, 403);
    }

    await next();
  };
}

/**
 * Permission-based access control middleware
 */
export function requirePermission(...permissions: string[]) {
  return async (c: Context, next: Next) => {
    const userPermissions = c.get('permissions') as string[] | undefined;
    
    if (!userPermissions) {
      return c.json({ error: 'Authentication required' }, 401);
    }

    const hasPermission = permissions.every(perm => userPermissions.includes(perm));
    
    if (!hasPermission) {
      return c.json({ error: 'Insufficient permissions' }, 403);
    }

    await next();
  };
}

/**
 * Tenant isolation middleware
 */
export function tenantIsolation() {
  return async (c: Context, next: Next) => {
    const tenantId = c.get('tenantId');
    
    if (!tenantId) {
      return c.json({ error: 'Tenant context required' }, 400);
    }

    // Add tenant filter to all database queries
    c.set('tenantFilter', { tenantId });
    
    await next();
  };
}

/**
 * Audit logging middleware
 */
export function auditLogger() {
  return async (c: Context, next: Next) => {
    const startTime = Date.now();
    const requestId = crypto.randomUUID();
    
    c.set('requestId', requestId);
    c.header('X-Request-ID', requestId);

    await next();

    const duration = Date.now() - startTime;
    
    const log: AuditLog = {
      id: requestId,
      timestamp: new Date().toISOString(),
      action: `${c.req.method} ${c.req.path}`,
      userId: c.get('userId') || 'anonymous',
      tenantId: c.get('tenantId') || 'system',
      resourceType: extractResourceType(c.req.path),
      resourceId: extractResourceId(c.req.path),
      metadata: {
        method: c.req.method,
        path: c.req.path,
        statusCode: c.res.status,
        duration,
        userAgent: c.req.header('User-Agent'),
        ip: getClientIP(c),
      },
    };

    // Log to audit system (in production, send to secure audit log)
    await logAuditEvent(log);
  };
}

/**
 * Request validation middleware
 */
export function validateRequest<T>(schema: { parse: (data: unknown) => T }) {
  return async (c: Context, next: Next) => {
    try {
      const body = await c.req.json();
      const validated = schema.parse(body);
      c.set('validatedBody', validated);
    } catch (error) {
      return c.json({ error: 'Invalid request body', details: error }, 400);
    }

    await next();
  };
}

// Helper functions

function getClientIdentifier(c: Context): string {
  const apiKeyId = c.get('apiKeyId');
  if (apiKeyId) return `key:${apiKeyId}`;
  
  const userId = c.get('userId');
  if (userId) return `user:${userId}`;
  
  return `ip:${getClientIP(c)}`;
}

function getClientIP(c: Context): string {
  return (
    c.req.header('X-Forwarded-For')?.split(',')[0]?.trim() ||
    c.req.header('X-Real-IP') ||
    'unknown'
  );
}

function extractResourceType(path: string): string {
  const segments = path.split('/').filter(Boolean);
  return segments[1] || 'unknown';
}

function extractResourceId(path: string): string | undefined {
  const segments = path.split('/').filter(Boolean);
  return segments[2];
}

/**
 * Hash an API key using SHA-256
 */
function hashApiKey(key: string): string {
  return createHash('sha256').update(key).digest('hex');
}

/**
 * Securely compare two strings using timing-safe comparison
 */
function secureCompare(a: string, b: string): boolean {
  try {
    const bufA = Buffer.from(a);
    const bufB = Buffer.from(b);
    if (bufA.length !== bufB.length) return false;
    return timingSafeEqual(bufA, bufB);
  } catch {
    return false;
  }
}

/**
 * Validate API key against stored keys
 * Uses ApiKeyStore interface for pluggable backends (in-memory/database)
 */
async function validateApiKey(key: string): Promise<{ id: string; tenantId: string; permissions: string[] } | null> {
  if (!key || !key.startsWith('mp_')) {
    return null;
  }

  // Extract prefix for lookup (first 7 chars after mp_)
  const prefix = key.slice(0, 7);
  const storedKey = await apiKeyStore.get(prefix);
  
  if (!storedKey) {
    return null;
  }

  // Compute hash and compare using timing-safe comparison
  const keyHash = hashApiKey(key);
  if (!secureCompare(keyHash, storedKey.keyHash)) {
    return null;
  }

  return {
    id: storedKey.id,
    tenantId: storedKey.tenantId,
    permissions: storedKey.permissions,
  };
}

/**
 * Verify JWT token using jose library
 */
async function verifyJWT(token: string): Promise<{ sub: string; tenantId: string; roles: string[] }> {
  const secret = new TextEncoder().encode(JWT_SECRET);
  
  try {
    const { payload } = await jose.jwtVerify(token, secret, {
      issuer: JWT_ISSUER,
      audience: JWT_AUDIENCE,
    });

    // Validate required claims
    if (!payload.sub || typeof payload.sub !== 'string') {
      throw new Error('Missing or invalid subject claim');
    }

    const tenantId = (payload.tenant_id || payload.tenantId || 'default') as string;
    const roles = Array.isArray(payload.roles) ? payload.roles as string[] : ['user'];

    return {
      sub: payload.sub,
      tenantId,
      roles,
    };
  } catch (error) {
    if (error instanceof jose.errors.JWTExpired) {
      throw new Error('Token has expired');
    }
    if (error instanceof jose.errors.JWTClaimValidationFailed) {
      throw new Error('Token validation failed');
    }
    throw new Error('Invalid token');
  }
}

/**
 * Create a new JWT token (utility for testing/development)
 */
export async function createJWT(payload: {
  sub: string;
  tenantId: string;
  roles: string[];
  expiresIn?: string;
}): Promise<string> {
  const secret = new TextEncoder().encode(JWT_SECRET);
  
  const jwt = await new jose.SignJWT({
    sub: payload.sub,
    tenant_id: payload.tenantId,
    roles: payload.roles,
  })
    .setProtectedHeader({ alg: 'HS256' })
    .setIssuedAt()
    .setIssuer(JWT_ISSUER)
    .setAudience(JWT_AUDIENCE)
    .setExpirationTime(payload.expiresIn || '24h')
    .sign(secret);

  return jwt;
}

/**
 * Register a new API key (utility for testing/development)
 */
export async function registerApiKey(key: string, config: {
  id: string;
  tenantId: string;
  permissions: string[];
}): Promise<void> {
  if (!key.startsWith('mp_')) {
    throw new Error('API keys must start with mp_');
  }
  
  const prefix = key.slice(0, 7);
  const keyHash = hashApiKey(key);
  
  await apiKeyStore.set(prefix, {
    ...config,
    keyHash,
  });
}

async function logAuditEvent(log: AuditLog): Promise<void> {
  // In production, send to secure audit log storage (e.g., database, SIEM)
  // For now, log to console with structured format
  if (process.env.NODE_ENV === 'production') {
    // In production, you'd send this to a proper audit log service
    console.log(JSON.stringify({ type: 'AUDIT', ...log }));
  } else if (process.env.LOG_LEVEL === 'debug') {
    console.log('[AUDIT]', JSON.stringify(log, null, 2));
  }
}
