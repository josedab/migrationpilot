/**
 * Feature Flags Service
 * 
 * Manage feature flags for gradual rollout and A/B testing
 */

export interface FeatureFlag {
  id: string;
  name: string;
  description?: string;
  enabled: boolean;
  rolloutPercentage: number;
  allowedTenants: string[];
  allowedUsers: string[];
  metadata: Record<string, unknown>;
  createdAt: string;
  updatedAt: string;
}

export interface FlagContext {
  userId?: string;
  tenantId?: string;
  attributes?: Record<string, unknown>;
}

// In-memory store (use database in production)
const flags = new Map<string, FeatureFlag>();

// Initialize with default flags
const defaultFlags: Omit<FeatureFlag, 'id' | 'createdAt' | 'updatedAt'>[] = [
  {
    name: 'new_parser_engine',
    description: 'Use the new parser engine for COBOL',
    enabled: false,
    rolloutPercentage: 0,
    allowedTenants: [],
    allowedUsers: [],
    metadata: { category: 'parser' },
  },
  {
    name: 'ai_code_review',
    description: 'Enable AI-powered code review before generation',
    enabled: true,
    rolloutPercentage: 50,
    allowedTenants: [],
    allowedUsers: [],
    metadata: { category: 'ai' },
  },
  {
    name: 'strangler_fig_mode',
    description: 'Enable Strangler Fig migration pattern',
    enabled: true,
    rolloutPercentage: 100,
    allowedTenants: [],
    allowedUsers: [],
    metadata: { category: 'migration' },
  },
  {
    name: 'confluence_export',
    description: 'Enable export to Confluence',
    enabled: false,
    rolloutPercentage: 0,
    allowedTenants: [],
    allowedUsers: [],
    metadata: { category: 'export' },
  },
];

// Initialize default flags
for (const flag of defaultFlags) {
  const id = `flag_${flag.name}`;
  flags.set(id, {
    ...flag,
    id,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  });
}

export class FeatureFlagService {
  
  /**
   * Get all feature flags
   */
  getAllFlags(): FeatureFlag[] {
    return Array.from(flags.values());
  }

  /**
   * Get a flag by name
   */
  getFlag(name: string): FeatureFlag | null {
    for (const flag of flags.values()) {
      if (flag.name === name) return flag;
    }
    return null;
  }

  /**
   * Create a new flag
   */
  createFlag(data: Omit<FeatureFlag, 'id' | 'createdAt' | 'updatedAt'>): FeatureFlag {
    const id = `flag_${data.name}`;
    const flag: FeatureFlag = {
      ...data,
      id,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    };
    flags.set(id, flag);
    return flag;
  }

  /**
   * Update a flag
   */
  updateFlag(name: string, updates: Partial<FeatureFlag>): FeatureFlag | null {
    const flag = this.getFlag(name);
    if (!flag) return null;
    
    const updated = {
      ...flag,
      ...updates,
      updatedAt: new Date().toISOString(),
    };
    flags.set(flag.id, updated);
    return updated;
  }

  /**
   * Delete a flag
   */
  deleteFlag(name: string): boolean {
    const flag = this.getFlag(name);
    if (!flag) return false;
    return flags.delete(flag.id);
  }

  /**
   * Check if a feature is enabled for a context
   */
  isEnabled(name: string, context?: FlagContext): boolean {
    const flag = this.getFlag(name);
    if (!flag) return false;
    if (!flag.enabled) return false;

    // Check allowed users (override)
    if (context?.userId && flag.allowedUsers.includes(context.userId)) {
      return true;
    }

    // Check allowed tenants (override)
    if (context?.tenantId && flag.allowedTenants.includes(context.tenantId)) {
      return true;
    }

    // Check rollout percentage
    if (flag.rolloutPercentage === 100) return true;
    if (flag.rolloutPercentage === 0) return false;

    // Consistent hash for user/tenant
    const identifier = context?.userId || context?.tenantId || Math.random().toString();
    const hash = this.hashString(identifier + name);
    return (hash % 100) < flag.rolloutPercentage;
  }

  /**
   * Get all enabled features for a context
   */
  getEnabledFeatures(context?: FlagContext): string[] {
    return this.getAllFlags()
      .filter(flag => this.isEnabled(flag.name, context))
      .map(flag => flag.name);
  }

  /**
   * Evaluate multiple flags at once
   */
  evaluateFlags(flagNames: string[], context?: FlagContext): Record<string, boolean> {
    const results: Record<string, boolean> = {};
    for (const name of flagNames) {
      results[name] = this.isEnabled(name, context);
    }
    return results;
  }

  /**
   * Gradually increase rollout percentage
   */
  incrementRollout(name: string, increment: number = 10): FeatureFlag | null {
    const flag = this.getFlag(name);
    if (!flag) return null;
    
    const newPercentage = Math.min(100, flag.rolloutPercentage + increment);
    return this.updateFlag(name, { rolloutPercentage: newPercentage });
  }

  /**
   * Simple string hash
   */
  private hashString(str: string): number {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash;
    }
    return Math.abs(hash);
  }
}

export const featureFlagService = new FeatureFlagService();

// Express/Hono middleware for feature flags
export function featureFlag(flagName: string) {
  return async (c: { req: { header: (name: string) => string | undefined }; json: (data: unknown, status?: number) => unknown }, next: () => Promise<void>) => {
    const userId = c.req.header('x-user-id');
    const tenantId = c.req.header('x-tenant-id');
    
    const isEnabled = featureFlagService.isEnabled(flagName, { userId, tenantId });
    
    if (!isEnabled) {
      return c.json({ error: 'Feature not available' }, 403);
    }
    
    await next();
  };
}
