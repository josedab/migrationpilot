/**
 * Security Middleware Tests
 * 
 * Tests for JWT verification, API key validation, and rate limiting
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Hono, type Context } from 'hono';
import {
  rateLimiter,
  apiKeyAuth,
  jwtAuth,
  securityHeaders,
  requireRole,
  requirePermission,
  createJWT,
  registerApiKey,
  setRateLimitStore,
  setApiKeyStore,
  type RateLimitStore,
  type ApiKeyStore,
} from '../middleware/security.js';

// Helper to get context value with proper typing
const getContextValue = (c: Context, key: string): unknown => {
  return (c as unknown as { get: (key: string) => unknown }).get(key);
};

// Mock rate limit store for testing
class MockRateLimitStore implements RateLimitStore {
  private store = new Map<string, { count: number; resetTime: number }>();

  async get(key: string) {
    return this.store.get(key) || null;
  }

  async set(key: string, value: { count: number; resetTime: number }) {
    this.store.set(key, value);
  }

  async increment(key: string) {
    const record = this.store.get(key);
    if (record) {
      record.count++;
      return record.count;
    }
    return 1;
  }

  clear() {
    this.store.clear();
  }
}

// Mock API key store for testing
class MockApiKeyStore implements ApiKeyStore {
  private store = new Map<string, { id: string; tenantId: string; permissions: string[]; keyHash: string }>();

  async get(prefix: string) {
    return this.store.get(prefix) || null;
  }

  async set(prefix: string, data: { id: string; tenantId: string; permissions: string[]; keyHash: string }) {
    this.store.set(prefix, data);
  }

  clear() {
    this.store.clear();
  }
}

describe('Security Middleware', () => {
  let mockRateLimitStore: MockRateLimitStore;
  let mockApiKeyStore: MockApiKeyStore;

  beforeEach(() => {
    mockRateLimitStore = new MockRateLimitStore();
    mockApiKeyStore = new MockApiKeyStore();
    setRateLimitStore(mockRateLimitStore);
    setApiKeyStore(mockApiKeyStore);
  });

  describe('JWT Authentication', () => {
    it('should reject requests without authorization header', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      const res = await app.request('/api/test');
      expect(res.status).toBe(401);
      
      const body = await res.json();
      expect(body.error).toBe('Authorization header required');
    });

    it('should reject requests with invalid bearer format', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      const res = await app.request('/api/test', {
        headers: { 'Authorization': 'Basic xyz' },
      });
      expect(res.status).toBe(401);
    });

    it('should reject invalid JWT tokens', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      const res = await app.request('/api/test', {
        headers: { 'Authorization': 'Bearer invalid.token.here' },
      });
      expect(res.status).toBe(401);
    });

    it('should accept valid JWT tokens and set context', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.get('/api/test', (c) => {
        return c.json({
          userId: getContextValue(c, 'userId'),
          tenantId: getContextValue(c, 'tenantId'),
          roles: getContextValue(c, 'roles'),
        });
      });

      const token = await createJWT({
        sub: 'user123',
        tenantId: 'tenant456',
        roles: ['admin', 'developer'],
      });

      const res = await app.request('/api/test', {
        headers: { 'Authorization': `Bearer ${token}` },
      });
      
      expect(res.status).toBe(200);
      const body = await res.json();
      expect(body.userId).toBe('user123');
      expect(body.tenantId).toBe('tenant456');
      expect(body.roles).toEqual(['admin', 'developer']);
    });

    it('should reject expired JWT tokens', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      // Create an expired token
      const token = await createJWT({
        sub: 'user123',
        tenantId: 'tenant456',
        roles: ['user'],
        expiresIn: '0s', // Immediately expired
      });

      // Wait a moment for the token to be expired
      await new Promise(resolve => setTimeout(resolve, 100));

      const res = await app.request('/api/test', {
        headers: { 'Authorization': `Bearer ${token}` },
      });
      
      expect(res.status).toBe(401);
    });
  });

  describe('API Key Authentication', () => {
    beforeEach(async () => {
      // Register a test API key
      await registerApiKey('mp_test_abcd1234', {
        id: 'key_test',
        tenantId: 'tenant_test',
        permissions: ['read', 'write'],
      });
    });

    it('should reject requests without API key', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      const res = await app.request('/api/test');
      expect(res.status).toBe(401);
      
      const body = await res.json();
      expect(body.error).toBe('API key required');
    });

    it('should allow optional API key when configured', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth({ optional: true }));
      app.get('/api/test', (c) => c.json({ authenticated: getContextValue(c, 'authenticated') }));

      const res = await app.request('/api/test');
      expect(res.status).toBe(200);
      
      const body = await res.json();
      expect(body.authenticated).toBe(false);
    });

    it('should reject invalid API keys', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      const res = await app.request('/api/test', {
        headers: { 'X-API-Key': 'mp_wrong_key12345' },
      });
      expect(res.status).toBe(401);
    });

    it('should reject API keys without mp_ prefix', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.get('/api/test', (c) => c.json({ success: true }));

      const res = await app.request('/api/test', {
        headers: { 'X-API-Key': 'invalid_key_format' },
      });
      expect(res.status).toBe(401);
    });

    it('should accept valid API keys and set context', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.get('/api/test', (c) => {
        return c.json({
          authenticated: getContextValue(c, 'authenticated'),
          apiKeyId: getContextValue(c, 'apiKeyId'),
          tenantId: getContextValue(c, 'tenantId'),
          permissions: getContextValue(c, 'permissions'),
        });
      });

      const res = await app.request('/api/test', {
        headers: { 'X-API-Key': 'mp_test_abcd1234' },
      });
      
      expect(res.status).toBe(200);
      const body = await res.json();
      expect(body.authenticated).toBe(true);
      expect(body.apiKeyId).toBe('key_test');
      expect(body.tenantId).toBe('tenant_test');
      expect(body.permissions).toEqual(['read', 'write']);
    });

    it('should accept API key from query parameter', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.get('/api/test', (c) => c.json({ authenticated: getContextValue(c, 'authenticated') }));

      const res = await app.request('/api/test?api_key=mp_test_abcd1234');
      expect(res.status).toBe(200);
    });
  });

  describe('Rate Limiting', () => {
    it('should allow requests under the limit', async () => {
      const app = new Hono();
      app.use('/*', rateLimiter({ windowMs: 60000, maxRequests: 10 }));
      app.get('/test', (c) => c.json({ success: true }));

      // Make 5 requests - all should succeed
      for (let i = 0; i < 5; i++) {
        const res = await app.request('/test', {
          headers: { 'X-Forwarded-For': '192.168.1.1' },
        });
        expect(res.status).toBe(200);
      }
    });

    it('should block requests over the limit', async () => {
      const app = new Hono();
      app.use('/*', rateLimiter({ windowMs: 60000, maxRequests: 3 }));
      app.get('/test', (c) => c.json({ success: true }));

      // Make requests up to the limit
      for (let i = 0; i < 3; i++) {
        const res = await app.request('/test', {
          headers: { 'X-Forwarded-For': '192.168.1.2' },
        });
        expect(res.status).toBe(200);
      }

      // Next request should be blocked
      const res = await app.request('/test', {
        headers: { 'X-Forwarded-For': '192.168.1.2' },
      });
      expect(res.status).toBe(429);
      
      const body = await res.json();
      expect(body.error).toBe('Too many requests');
    });

    it('should include rate limit headers', async () => {
      const app = new Hono();
      app.use('/*', rateLimiter({ windowMs: 60000, maxRequests: 10 }));
      app.get('/test', (c) => c.json({ success: true }));

      const res = await app.request('/test', {
        headers: { 'X-Forwarded-For': '192.168.1.3' },
      });
      
      expect(res.headers.get('X-RateLimit-Limit')).toBe('10');
      expect(res.headers.get('X-RateLimit-Remaining')).toBeDefined();
    });

    it('should track different clients separately', async () => {
      const app = new Hono();
      app.use('/*', rateLimiter({ windowMs: 60000, maxRequests: 2 }));
      app.get('/test', (c) => c.json({ success: true }));

      // Client 1 uses up their limit
      for (let i = 0; i < 2; i++) {
        await app.request('/test', {
          headers: { 'X-Forwarded-For': '10.0.0.1' },
        });
      }

      // Client 1 is now blocked
      const res1 = await app.request('/test', {
        headers: { 'X-Forwarded-For': '10.0.0.1' },
      });
      expect(res1.status).toBe(429);

      // Client 2 should still be allowed
      const res2 = await app.request('/test', {
        headers: { 'X-Forwarded-For': '10.0.0.2' },
      });
      expect(res2.status).toBe(200);
    });
  });

  describe('Security Headers', () => {
    it('should set security headers on responses', async () => {
      const app = new Hono();
      app.use('/*', securityHeaders());
      app.get('/test', (c) => c.json({ success: true }));

      const res = await app.request('/test');
      
      expect(res.headers.get('X-Content-Type-Options')).toBe('nosniff');
      expect(res.headers.get('X-Frame-Options')).toBe('DENY');
      expect(res.headers.get('X-XSS-Protection')).toBe('1; mode=block');
      expect(res.headers.get('Strict-Transport-Security')).toContain('max-age=');
      expect(res.headers.get('Content-Security-Policy')).toContain("default-src 'self'");
      expect(res.headers.get('Referrer-Policy')).toBe('strict-origin-when-cross-origin');
    });
  });

  describe('Role-Based Access Control', () => {
    it('should reject unauthenticated requests', async () => {
      const app = new Hono();
      app.use('/admin/*', requireRole('admin'));
      app.get('/admin/dashboard', (c) => c.json({ success: true }));

      const res = await app.request('/admin/dashboard');
      expect(res.status).toBe(401);
    });

    it('should reject users without required role', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.use('/api/admin/*', requireRole('admin'));
      app.get('/api/admin/dashboard', (c) => c.json({ success: true }));

      const token = await createJWT({
        sub: 'user123',
        tenantId: 'tenant1',
        roles: ['user', 'developer'],
      });

      const res = await app.request('/api/admin/dashboard', {
        headers: { 'Authorization': `Bearer ${token}` },
      });
      expect(res.status).toBe(403);
    });

    it('should allow users with required role', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.use('/api/admin/*', requireRole('admin'));
      app.get('/api/admin/dashboard', (c) => c.json({ success: true }));

      const token = await createJWT({
        sub: 'admin123',
        tenantId: 'tenant1',
        roles: ['user', 'admin'],
      });

      const res = await app.request('/api/admin/dashboard', {
        headers: { 'Authorization': `Bearer ${token}` },
      });
      expect(res.status).toBe(200);
    });

    it('should accept any of multiple roles', async () => {
      const app = new Hono();
      app.use('/api/*', jwtAuth());
      app.use('/api/manage/*', requireRole('admin', 'manager'));
      app.get('/api/manage/users', (c) => c.json({ success: true }));

      const token = await createJWT({
        sub: 'manager1',
        tenantId: 'tenant1',
        roles: ['manager'],
      });

      const res = await app.request('/api/manage/users', {
        headers: { 'Authorization': `Bearer ${token}` },
      });
      expect(res.status).toBe(200);
    });
  });

  describe('Permission-Based Access Control', () => {
    beforeEach(async () => {
      await registerApiKey('mp_full_permissions', {
        id: 'key_full',
        tenantId: 'tenant1',
        permissions: ['read', 'write', 'delete', 'admin'],
      });
      
      await registerApiKey('mp_read_only_key', {
        id: 'key_readonly',
        tenantId: 'tenant1',
        permissions: ['read'],
      });
    });

    it('should reject users without required permission', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.use('/api/delete/*', requirePermission('delete'));
      app.delete('/api/delete/resource', (c) => c.json({ success: true }));

      const res = await app.request('/api/delete/resource', {
        method: 'DELETE',
        headers: { 'X-API-Key': 'mp_read_only_key' },
      });
      expect(res.status).toBe(403);
    });

    it('should allow users with required permission', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.use('/api/delete/*', requirePermission('delete'));
      app.delete('/api/delete/resource', (c) => c.json({ success: true }));

      const res = await app.request('/api/delete/resource', {
        method: 'DELETE',
        headers: { 'X-API-Key': 'mp_full_permissions' },
      });
      expect(res.status).toBe(200);
    });

    it('should require all specified permissions', async () => {
      const app = new Hono();
      app.use('/api/*', apiKeyAuth());
      app.use('/api/admin/*', requirePermission('admin', 'write'));
      app.post('/api/admin/config', (c) => c.json({ success: true }));

      // User with only read permission
      const res1 = await app.request('/api/admin/config', {
        method: 'POST',
        headers: { 'X-API-Key': 'mp_read_only_key' },
      });
      expect(res1.status).toBe(403);

      // User with all permissions
      const res2 = await app.request('/api/admin/config', {
        method: 'POST',
        headers: { 'X-API-Key': 'mp_full_permissions' },
      });
      expect(res2.status).toBe(200);
    });
  });
});
