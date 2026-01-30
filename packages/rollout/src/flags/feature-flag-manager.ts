/**
 * Feature Flag Manager
 * Manages feature flags for gradual rollouts and A/B testing
 */

import {
  FeatureFlagConfig,
  FlagRule,
  FlagCondition,
  FlagValue,
} from '../types';

export class FeatureFlagManager {
  private flags: Map<string, FeatureFlagConfig> = new Map();
  private evaluationCache: Map<string, CacheEntry> = new Map();
  private readonly cacheTTL: number;

  constructor(options: { cacheTTL?: number } = {}) {
    this.cacheTTL = options.cacheTTL ?? 60000; // Default 1 minute
  }

  registerFlag(flag: FeatureFlagConfig): void {
    this.flags.set(flag.id, flag);
    this.invalidateCache(flag.id);
  }

  unregisterFlag(flagId: string): void {
    this.flags.delete(flagId);
    this.invalidateCache(flagId);
  }

  getFlag(flagId: string): FeatureFlagConfig | undefined {
    return this.flags.get(flagId);
  }

  listFlags(): FeatureFlagConfig[] {
    return Array.from(this.flags.values());
  }

  updateFlag(flagId: string, updates: Partial<FeatureFlagConfig>): void {
    const flag = this.flags.get(flagId);
    if (!flag) throw new Error(`Flag ${flagId} not found`);

    Object.assign(flag, updates);
    this.invalidateCache(flagId);
  }

  evaluate(flagId: string, context: EvaluationContext): FlagValue {
    const flag = this.flags.get(flagId);
    if (!flag) return false;
    if (!flag.enabled) return flag.defaultValue;

    // Check cache
    const cacheKey = this.buildCacheKey(flagId, context);
    const cached = this.evaluationCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < this.cacheTTL) {
      return cached.value;
    }

    // Evaluate rules in priority order
    const sortedRules = [...flag.rules].sort((a, b) => a.priority - b.priority);
    
    for (const rule of sortedRules) {
      if (this.evaluateRule(rule, context)) {
        const variant = flag.variants.find(v => v.id === rule.variant);
        if (variant) {
          this.cacheResult(cacheKey, variant.value);
          return variant.value;
        }
      }
    }

    // Check for percentage-based distribution
    const percentageVariant = this.evaluatePercentageDistribution(flag, context);
    if (percentageVariant !== null) {
      this.cacheResult(cacheKey, percentageVariant);
      return percentageVariant;
    }

    this.cacheResult(cacheKey, flag.defaultValue);
    return flag.defaultValue;
  }

  evaluateBool(flagId: string, context: EvaluationContext): boolean {
    const value = this.evaluate(flagId, context);
    return typeof value === 'boolean' ? value : Boolean(value);
  }

  evaluateString(flagId: string, context: EvaluationContext): string {
    const value = this.evaluate(flagId, context);
    return typeof value === 'string' ? value : String(value);
  }

  evaluateNumber(flagId: string, context: EvaluationContext): number {
    const value = this.evaluate(flagId, context);
    return typeof value === 'number' ? value : 0;
  }

  private evaluateRule(rule: FlagRule, context: EvaluationContext): boolean {
    return rule.conditions.every(condition => 
      this.evaluateCondition(condition, context)
    );
  }

  private evaluateCondition(condition: FlagCondition, context: EvaluationContext): boolean {
    const contextValue = this.getContextValue(context, condition.attribute);
    const conditionValue = condition.value;

    switch (condition.operator) {
      case 'equals':
        return contextValue === conditionValue;
      
      case 'not-equals':
        return contextValue !== conditionValue;
      
      case 'contains':
        return typeof contextValue === 'string' && 
               typeof conditionValue === 'string' &&
               contextValue.includes(conditionValue);
      
      case 'not-contains':
        return typeof contextValue === 'string' && 
               typeof conditionValue === 'string' &&
               !contextValue.includes(conditionValue);
      
      case 'in':
        return Array.isArray(conditionValue) && (conditionValue as unknown[]).includes(contextValue);
      
      case 'not-in':
        return Array.isArray(conditionValue) && !(conditionValue as unknown[]).includes(contextValue);
      
      case 'matches':
        if (typeof contextValue !== 'string' || typeof conditionValue !== 'string') {
          return false;
        }
        try {
          return new RegExp(conditionValue).test(contextValue);
        } catch {
          return false;
        }
      
      case 'gt':
        return typeof contextValue === 'number' && 
               typeof conditionValue === 'number' &&
               contextValue > conditionValue;
      
      case 'gte':
        return typeof contextValue === 'number' && 
               typeof conditionValue === 'number' &&
               contextValue >= conditionValue;
      
      case 'lt':
        return typeof contextValue === 'number' && 
               typeof conditionValue === 'number' &&
               contextValue < conditionValue;
      
      case 'lte':
        return typeof contextValue === 'number' && 
               typeof conditionValue === 'number' &&
               contextValue <= conditionValue;
      
      case 'percentage':
        if (typeof conditionValue !== 'number') return false;
        const hash = this.hashContext(context);
        return (hash % 100) < conditionValue;
      
      default:
        return false;
    }
  }

  private getContextValue(context: EvaluationContext, attribute: string): unknown {
    const parts = attribute.split('.');
    let value: unknown = context;
    
    for (const part of parts) {
      if (value && typeof value === 'object') {
        value = (value as Record<string, unknown>)[part];
      } else {
        return undefined;
      }
    }
    
    return value;
  }

  private evaluatePercentageDistribution(
    flag: FeatureFlagConfig,
    context: EvaluationContext
  ): FlagValue | null {
    const totalWeight = flag.variants.reduce((sum, v) => sum + v.weight, 0);
    if (totalWeight === 0) return null;

    const hash = this.hashContext(context);
    const bucket = hash % totalWeight;
    
    let accumulated = 0;
    for (const variant of flag.variants) {
      accumulated += variant.weight;
      if (bucket < accumulated) {
        return variant.value;
      }
    }
    
    return null;
  }

  private hashContext(context: EvaluationContext): number {
    const key = context.userId || context.sessionId || JSON.stringify(context);
    let hash = 0;
    for (let i = 0; i < key.length; i++) {
      const char = key.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32bit integer
    }
    return Math.abs(hash);
  }

  private buildCacheKey(flagId: string, context: EvaluationContext): string {
    const contextKey = context.userId || context.sessionId || JSON.stringify(context);
    return `${flagId}:${contextKey}`;
  }

  private cacheResult(key: string, value: FlagValue): void {
    this.evaluationCache.set(key, {
      value,
      timestamp: Date.now(),
    });
  }

  private invalidateCache(flagId: string): void {
    for (const key of this.evaluationCache.keys()) {
      if (key.startsWith(`${flagId}:`)) {
        this.evaluationCache.delete(key);
      }
    }
  }

  clearCache(): void {
    this.evaluationCache.clear();
  }

  exportFlags(): FeatureFlagConfig[] {
    return Array.from(this.flags.values());
  }

  importFlags(flags: FeatureFlagConfig[]): void {
    for (const flag of flags) {
      this.registerFlag(flag);
    }
  }
}

export interface EvaluationContext {
  userId?: string;
  sessionId?: string;
  environment?: string;
  version?: string;
  region?: string;
  customAttributes?: Record<string, unknown>;
}

interface CacheEntry {
  value: FlagValue;
  timestamp: number;
}

export function createFeatureFlagManager(options?: { cacheTTL?: number }): FeatureFlagManager {
  return new FeatureFlagManager(options);
}

// Pre-built flag templates
export const FlagTemplates = {
  booleanFlag(id: string, name: string, defaultValue = false): FeatureFlagConfig {
    return {
      id,
      name,
      description: '',
      enabled: true,
      rules: [],
      defaultValue,
      variants: [
        { id: 'on', name: 'Enabled', value: true, weight: 0 },
        { id: 'off', name: 'Disabled', value: false, weight: 0 },
      ],
    };
  },

  percentageRollout(id: string, name: string, percentage: number): FeatureFlagConfig {
    return {
      id,
      name,
      description: `${percentage}% rollout`,
      enabled: true,
      rules: [{
        id: `rule_${id}`,
        priority: 1,
        conditions: [{
          attribute: '_percentage',
          operator: 'percentage',
          value: percentage,
        }],
        variant: 'treatment',
      }],
      defaultValue: false,
      variants: [
        { id: 'treatment', name: 'Treatment', value: true, weight: percentage },
        { id: 'control', name: 'Control', value: false, weight: 100 - percentage },
      ],
    };
  },

  abTest(id: string, name: string, variants: string[]): FeatureFlagConfig {
    const weight = Math.floor(100 / variants.length);
    return {
      id,
      name,
      description: 'A/B test configuration',
      enabled: true,
      rules: [],
      defaultValue: variants[0] || '',
      variants: variants.map((v, i) => ({
        id: `variant_${i}`,
        name: v,
        value: v,
        weight: i === variants.length - 1 ? 100 - (weight * (variants.length - 1)) : weight,
      })),
    };
  },

  targetedRollout(
    id: string,
    name: string,
    targetAttribute: string,
    targetValues: string[]
  ): FeatureFlagConfig {
    return {
      id,
      name,
      description: `Targeted rollout for ${targetAttribute}`,
      enabled: true,
      rules: [{
        id: `rule_${id}`,
        priority: 1,
        conditions: [{
          attribute: targetAttribute,
          operator: 'in',
          value: targetValues,
        }],
        variant: 'enabled',
      }],
      defaultValue: false,
      variants: [
        { id: 'enabled', name: 'Enabled', value: true, weight: 0 },
        { id: 'disabled', name: 'Disabled', value: false, weight: 0 },
      ],
    };
  },
};
