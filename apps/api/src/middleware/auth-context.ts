/**
 * Authentication Context Helper
 * 
 * Provides typed access to auth information from middleware
 */

import type { Context } from 'hono';

export interface AuthUser {
  id: string;
  email?: string;
  name?: string;
  tenantId: string;
  organizationId: string;
  roles: string[];
  permissions: string[];
}

export interface AuthContext {
  user: AuthUser;
  isAuthenticated: boolean;
  requestId: string;
}

/**
 * Get authenticated user from context
 * Returns null if not authenticated
 */
export function getAuthUser(c: Context): AuthUser | null {
  const userId = c.get('userId');
  if (!userId) {
    return null;
  }

  return {
    id: userId,
    email: c.get('userEmail'),
    name: c.get('userName'),
    tenantId: c.get('tenantId') || 'default',
    organizationId: c.get('tenantId') || 'default',
    roles: c.get('roles') || [],
    permissions: c.get('permissions') || [],
  };
}

/**
 * Get authenticated user or throw error
 */
export function requireAuthUser(c: Context): AuthUser {
  const user = getAuthUser(c);
  if (!user) {
    throw new Error('Authentication required');
  }
  return user;
}

/**
 * Get full auth context
 */
export function getAuthContext(c: Context): AuthContext {
  const user = getAuthUser(c);
  return {
    user: user || {
      id: 'anonymous',
      tenantId: 'public',
      organizationId: 'public',
      roles: [],
      permissions: [],
    },
    isAuthenticated: !!user,
    requestId: c.get('requestId') || 'unknown',
  };
}

/**
 * Check if user has a specific role
 */
export function hasRole(c: Context, role: string): boolean {
  const user = getAuthUser(c);
  return user?.roles.includes(role) ?? false;
}

/**
 * Check if user has a specific permission
 */
export function hasPermission(c: Context, permission: string): boolean {
  const user = getAuthUser(c);
  return user?.permissions.includes(permission) ?? false;
}

/**
 * Get tenant ID for data isolation
 */
export function getTenantId(c: Context): string {
  return c.get('tenantId') || 'default';
}

/**
 * Get user ID for audit trails
 */
export function getUserId(c: Context): string {
  return c.get('userId') || 'anonymous';
}

/**
 * Get organization ID
 */
export function getOrganizationId(c: Context): string {
  return c.get('tenantId') || 'default';
}
