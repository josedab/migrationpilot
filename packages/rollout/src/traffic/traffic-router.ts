/**
 * Traffic Router
 * Manages traffic splitting and routing for progressive deployments
 */

import { TrafficSplit, TrafficRoute, RouteCondition } from '../types';

export class TrafficRouter {
  private splits: Map<string, TrafficSplit> = new Map();
  private sessionAffinity: Map<string, string> = new Map(); // sessionKey -> targetId

  registerSplit(split: TrafficSplit): void {
    this.validateSplit(split);
    this.splits.set(split.id, split);
  }

  unregisterSplit(splitId: string): void {
    this.splits.delete(splitId);
    // Clean up affinity entries for this split
    for (const key of this.sessionAffinity.keys()) {
      if (key.startsWith(`${splitId}:`)) {
        this.sessionAffinity.delete(key);
      }
    }
  }

  getSplit(splitId: string): TrafficSplit | undefined {
    return this.splits.get(splitId);
  }

  listSplits(): TrafficSplit[] {
    return Array.from(this.splits.values());
  }

  updateSplit(splitId: string, updates: Partial<TrafficSplit>): void {
    const split = this.splits.get(splitId);
    if (!split) throw new Error(`Split ${splitId} not found`);

    const updated = { ...split, ...updates };
    this.validateSplit(updated);
    this.splits.set(splitId, updated);
  }

  updateRouteWeight(splitId: string, routeId: string, newWeight: number): void {
    const split = this.splits.get(splitId);
    if (!split) throw new Error(`Split ${splitId} not found`);

    const route = split.routes.find(r => r.id === routeId);
    if (!route) throw new Error(`Route ${routeId} not found in split ${splitId}`);

    route.weight = newWeight;
    this.normalizeWeights(split);
  }

  route(splitId: string, request: RoutingRequest): RoutingResult {
    const split = this.splits.get(splitId);
    if (!split) {
      return {
        target: 'default',
        reason: 'split-not-found',
      };
    }

    // Check sticky session
    if (split.stickySession && request.sessionKey) {
      const affinityKey = `${splitId}:${request.sessionKey}`;
      const stickyTarget = this.sessionAffinity.get(affinityKey);
      if (stickyTarget) {
        return {
          target: stickyTarget,
          reason: 'sticky-session',
          metadata: { sessionKey: request.sessionKey },
        };
      }
    }

    // Check condition-based routes first
    for (const route of split.routes) {
      if (route.conditions && this.matchesConditions(route.conditions, request)) {
        this.recordAffinity(split, request, route.target);
        return {
          target: route.target,
          reason: 'condition-match',
          metadata: route.metadata,
        };
      }
    }

    // Weight-based routing
    const selectedRoute = this.selectByWeight(split.routes, request, split.hashKey);
    this.recordAffinity(split, request, selectedRoute.target);

    return {
      target: selectedRoute.target,
      reason: 'weight-based',
      weight: selectedRoute.weight,
      metadata: selectedRoute.metadata,
    };
  }

  private validateSplit(split: TrafficSplit): void {
    if (!split.id) throw new Error('Split ID is required');
    if (!split.routes || split.routes.length === 0) {
      throw new Error('At least one route is required');
    }

    const totalWeight = split.routes.reduce((sum, r) => sum + r.weight, 0);
    if (totalWeight === 0) {
      throw new Error('Total weight must be greater than 0');
    }
  }

  private normalizeWeights(split: TrafficSplit): void {
    const totalWeight = split.routes.reduce((sum, r) => sum + r.weight, 0);
    if (totalWeight > 0) {
      for (const route of split.routes) {
        route.weight = (route.weight / totalWeight) * 100;
      }
    }
  }

  private matchesConditions(conditions: RouteCondition[], request: RoutingRequest): boolean {
    return conditions.every(condition => this.matchesCondition(condition, request));
  }

  private matchesCondition(condition: RouteCondition, request: RoutingRequest): boolean {
    let value: string | undefined;

    switch (condition.type) {
      case 'header':
        value = request.headers?.[condition.key];
        break;
      case 'query':
        value = request.query?.[condition.key];
        break;
      case 'cookie':
        value = request.cookies?.[condition.key];
        break;
      case 'path':
        value = request.path;
        break;
      case 'user-agent':
        value = request.userAgent;
        break;
    }

    if (value === undefined) return false;

    switch (condition.operator) {
      case 'equals':
        return value === condition.value;
      case 'contains':
        return value.includes(condition.value);
      case 'matches':
        try {
          return new RegExp(condition.value).test(value);
        } catch {
          return false;
        }
      default:
        return false;
    }
  }

  private selectByWeight(
    routes: TrafficRoute[],
    request: RoutingRequest,
    hashKey?: string
  ): TrafficRoute {
    const totalWeight = routes.reduce((sum, r) => sum + r.weight, 0);
    
    // Deterministic routing based on hash key
    let bucket: number;
    if (hashKey && request.sessionKey) {
      const hash = this.hash(`${hashKey}:${request.sessionKey}`);
      bucket = hash % totalWeight;
    } else {
      bucket = Math.random() * totalWeight;
    }

    let accumulated = 0;
    for (const route of routes) {
      accumulated += route.weight;
      if (bucket < accumulated) {
        return route;
      }
    }

    // Fallback to first route
    return routes[0]!;
  }

  private recordAffinity(split: TrafficSplit, request: RoutingRequest, target: string): void {
    if (split.stickySession && request.sessionKey) {
      const affinityKey = `${split.id}:${request.sessionKey}`;
      this.sessionAffinity.set(affinityKey, target);
    }
  }

  private hash(input: string): number {
    let hash = 0;
    for (let i = 0; i < input.length; i++) {
      const char = input.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash;
    }
    return Math.abs(hash);
  }

  clearAffinities(): void {
    this.sessionAffinity.clear();
  }

  getStats(splitId: string): TrafficStats | undefined {
    const split = this.splits.get(splitId);
    if (!split) return undefined;

    return {
      splitId,
      routes: split.routes.map(r => ({
        routeId: r.id,
        target: r.target,
        weight: r.weight,
        normalizedWeight: r.weight / split.routes.reduce((s, r2) => s + r2.weight, 0) * 100,
      })),
      stickySessionCount: Array.from(this.sessionAffinity.keys())
        .filter(k => k.startsWith(`${splitId}:`)).length,
    };
  }
}

export interface RoutingRequest {
  path?: string;
  method?: string;
  headers?: Record<string, string>;
  query?: Record<string, string>;
  cookies?: Record<string, string>;
  userAgent?: string;
  sessionKey?: string;
  userId?: string;
}

export interface RoutingResult {
  target: string;
  reason: string;
  weight?: number;
  metadata?: Record<string, string>;
}

export interface TrafficStats {
  splitId: string;
  routes: Array<{
    routeId: string;
    target: string;
    weight: number;
    normalizedWeight: number;
  }>;
  stickySessionCount: number;
}

export function createTrafficRouter(): TrafficRouter {
  return new TrafficRouter();
}

// Helper functions for common split configurations
export const SplitTemplates = {
  canary(
    id: string,
    primaryTarget: string,
    canaryTarget: string,
    canaryPercentage: number
  ): TrafficSplit {
    return {
      id,
      name: `Canary: ${canaryPercentage}%`,
      routes: [
        { id: 'primary', target: primaryTarget, weight: 100 - canaryPercentage },
        { id: 'canary', target: canaryTarget, weight: canaryPercentage },
      ],
      stickySession: true,
      hashKey: 'userId',
    };
  },

  blueGreen(id: string, blueTarget: string, greenTarget: string, activeColor: 'blue' | 'green'): TrafficSplit {
    return {
      id,
      name: `Blue-Green: ${activeColor} active`,
      routes: [
        { id: 'blue', target: blueTarget, weight: activeColor === 'blue' ? 100 : 0 },
        { id: 'green', target: greenTarget, weight: activeColor === 'green' ? 100 : 0 },
      ],
      stickySession: false,
    };
  },

  abTest(id: string, controlTarget: string, treatmentTarget: string, treatmentPercentage: number): TrafficSplit {
    return {
      id,
      name: `A/B Test: ${treatmentPercentage}% treatment`,
      routes: [
        { id: 'control', target: controlTarget, weight: 100 - treatmentPercentage },
        { id: 'treatment', target: treatmentTarget, weight: treatmentPercentage },
      ],
      stickySession: true,
      hashKey: 'userId',
    };
  },

  headerBased(
    id: string,
    defaultTarget: string,
    routes: Array<{ target: string; headerName: string; headerValue: string }>
  ): TrafficSplit {
    return {
      id,
      name: 'Header-based routing',
      routes: [
        ...routes.map((r, i) => ({
          id: `header_route_${i}`,
          target: r.target,
          weight: 0,
          conditions: [{
            type: 'header' as const,
            key: r.headerName,
            operator: 'equals' as const,
            value: r.headerValue,
          }],
        })),
        { id: 'default', target: defaultTarget, weight: 100 },
      ],
      stickySession: false,
    };
  },
};
