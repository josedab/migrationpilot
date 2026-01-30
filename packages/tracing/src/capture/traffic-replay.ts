/**
 * Traffic Replay System
 * 
 * Replays captured traffic against both legacy and modern systems
 */

import { generateId } from '@migrationpilot/core';
import type { ShadowCapture, ComparisonResult } from '../types.js';

export interface ReplayConfig {
  projectId: string;
  
  // Endpoints
  legacyEndpoint?: string;
  modernEndpoint?: string;
  
  // Replay settings
  concurrency: number;
  delayBetweenRequestsMs: number;
  timeoutMs: number;
  retryCount: number;
  
  // Callbacks
  onRequestStart?: (captureId: string) => void;
  onRequestComplete?: (result: ReplayResult) => void;
  onError?: (captureId: string, error: Error) => void;
}

export interface ReplayResult {
  captureId: string;
  originalCapture: ShadowCapture;
  
  // Replay responses
  legacyReplayResponse?: unknown;
  legacyReplayDurationMs?: number;
  legacyReplayError?: string;
  
  modernReplayResponse?: unknown;
  modernReplayDurationMs?: number;
  modernReplayError?: string;
  
  // Comparisons
  legacyConsistency?: ComparisonResult; // Original vs replay
  modernComparison?: ComparisonResult; // Legacy vs modern
  
  replayedAt: Date;
}

export interface ReplayBatchResult {
  batchId: string;
  startedAt: Date;
  completedAt: Date;
  durationMs: number;
  
  totalCaptures: number;
  successful: number;
  failed: number;
  
  results: ReplayResult[];
  
  summary: ReplaySummary;
}

export interface ReplaySummary {
  legacyConsistencyRate: number;
  modernEquivalenceRate: number;
  averageLegacyLatencyMs: number;
  averageModernLatencyMs: number;
  latencyImprovement: number; // percentage
  commonDifferences: DifferencePattern[];
}

export interface DifferencePattern {
  path: string;
  frequency: number;
  exampleLegacyValue: unknown;
  exampleModernValue: unknown;
}

export class TrafficReplay {
  private config: ReplayConfig;
  private results: Map<string, ReplayResult> = new Map();

  constructor(config: ReplayConfig) {
    this.config = {
      ...config,
      concurrency: config.concurrency ?? 10,
      delayBetweenRequestsMs: config.delayBetweenRequestsMs ?? 100,
      timeoutMs: config.timeoutMs ?? 30000,
      retryCount: config.retryCount ?? 3,
    };
  }

  /**
   * Replay a single capture
   */
  async replaySingle(capture: ShadowCapture): Promise<ReplayResult> {
    const result: ReplayResult = {
      captureId: capture.id,
      originalCapture: capture,
      replayedAt: new Date(),
    };

    this.config.onRequestStart?.(capture.id);

    try {
      // Replay against legacy system if endpoint configured
      if (this.config.legacyEndpoint) {
        const legacyResult = await this.executeRequest(
          this.config.legacyEndpoint,
          capture.requestData
        );
        result.legacyReplayResponse = legacyResult.response;
        result.legacyReplayDurationMs = legacyResult.durationMs;
        result.legacyReplayError = legacyResult.error;

        // Compare original legacy response with replay
        if (capture.legacyResponse && legacyResult.response) {
          result.legacyConsistency = this.compareResponses(
            capture.legacyResponse,
            legacyResult.response
          );
        }
      }

      // Replay against modern system if endpoint configured
      if (this.config.modernEndpoint) {
        const modernResult = await this.executeRequest(
          this.config.modernEndpoint,
          capture.requestData
        );
        result.modernReplayResponse = modernResult.response;
        result.modernReplayDurationMs = modernResult.durationMs;
        result.modernReplayError = modernResult.error;

        // Compare legacy vs modern
        const legacyResponse = result.legacyReplayResponse || capture.legacyResponse;
        if (legacyResponse && modernResult.response) {
          result.modernComparison = this.compareResponses(
            legacyResponse,
            modernResult.response
          );
        }
      }

      this.results.set(capture.id, result);
      this.config.onRequestComplete?.(result);
    } catch (error) {
      this.config.onError?.(capture.id, error as Error);
    }

    return result;
  }

  /**
   * Replay a batch of captures with concurrency control
   */
  async replayBatch(captures: ShadowCapture[]): Promise<ReplayBatchResult> {
    const batchId = generateId();
    const startedAt = new Date();
    const results: ReplayResult[] = [];
    let successful = 0;
    let failed = 0;

    // Process in batches based on concurrency
    const queue = [...captures];
    
    while (queue.length > 0) {
      const batch = queue.splice(0, this.config.concurrency);
      const batchPromises = batch.map(async (capture) => {
        await this.delay(this.config.delayBetweenRequestsMs);
        return this.replaySingle(capture);
      });

      const batchResults = await Promise.allSettled(batchPromises);
      
      for (const result of batchResults) {
        if (result.status === 'fulfilled') {
          results.push(result.value);
          if (!result.value.legacyReplayError && !result.value.modernReplayError) {
            successful++;
          } else {
            failed++;
          }
        } else {
          failed++;
        }
      }
    }

    const completedAt = new Date();

    return {
      batchId,
      startedAt,
      completedAt,
      durationMs: completedAt.getTime() - startedAt.getTime(),
      totalCaptures: captures.length,
      successful,
      failed,
      results,
      summary: this.generateSummary(results),
    };
  }

  /**
   * Execute a request against an endpoint
   */
  private async executeRequest(
    endpoint: string,
    requestData: unknown,
    attempt = 1
  ): Promise<{ response?: unknown; durationMs: number; error?: string }> {
    const startTime = Date.now();

    try {
      // In production, this would use fetch or a specific protocol adapter
      // For now, we simulate the request
      const response = await this.simulateRequest(endpoint, requestData);
      
      return {
        response,
        durationMs: Date.now() - startTime,
      };
    } catch (error) {
      if (attempt < this.config.retryCount) {
        await this.delay(1000 * attempt); // Exponential backoff
        return this.executeRequest(endpoint, requestData, attempt + 1);
      }
      
      return {
        durationMs: Date.now() - startTime,
        error: error instanceof Error ? error.message : 'Unknown error',
      };
    }
  }

  /**
   * Simulate a request (placeholder for real implementation)
   */
  private async simulateRequest(
    _endpoint: string,
    _requestData: unknown
  ): Promise<unknown> {
    // In production, this would:
    // 1. For legacy: Use mainframe connector, CICS adapter, or protocol bridge
    // 2. For modern: Use HTTP/REST/gRPC call
    await this.delay(Math.random() * 100 + 50);
    return { simulated: true, timestamp: Date.now() };
  }

  /**
   * Compare two responses
   */
  private compareResponses(legacy: unknown, modern: unknown): ComparisonResult {
    const differences: { path: string; legacyValue: unknown; modernValue: unknown; differenceType: 'value' | 'type' | 'missing' | 'extra' | 'tolerance'; severity: 'critical' | 'warning' | 'info' }[] = [];
    const legacyOnly: string[] = [];
    const modernOnly: string[] = [];
    let matchedFields = 0;
    let totalFields = 0;

    this.compareObjects(legacy, modern, '', differences, legacyOnly, modernOnly, {
      matched: matchedFields,
      total: totalFields,
    });

    totalFields = differences.length + legacyOnly.length + modernOnly.length + matchedFields;

    return {
      equivalent: differences.length === 0 && legacyOnly.length === 0 && modernOnly.length === 0,
      differences,
      legacyOnly,
      modernOnly,
      matchedFields,
      totalFields,
      confidence: totalFields > 0 ? matchedFields / totalFields : 1.0,
    };
  }

  private compareObjects(
    legacy: unknown,
    modern: unknown,
    path: string,
    differences: { path: string; legacyValue: unknown; modernValue: unknown; differenceType: 'value' | 'type' | 'missing' | 'extra' | 'tolerance'; severity: 'critical' | 'warning' | 'info' }[],
    legacyOnly: string[],
    modernOnly: string[],
    counts: { matched: number; total: number }
  ): void {
    if (legacy === modern) {
      counts.matched++;
      counts.total++;
      return;
    }

    if (legacy === null || legacy === undefined || modern === null || modern === undefined) {
      if (legacy !== modern) {
        differences.push({
          path,
          legacyValue: legacy,
          modernValue: modern,
          differenceType: legacy === null || legacy === undefined ? 'missing' : 'extra',
          severity: 'warning',
        });
      }
      counts.total++;
      return;
    }

    if (typeof legacy !== typeof modern) {
      differences.push({
        path,
        legacyValue: legacy,
        modernValue: modern,
        differenceType: 'type',
        severity: 'critical',
      });
      counts.total++;
      return;
    }

    if (typeof legacy === 'object' && !Array.isArray(legacy)) {
      const legacyKeys = Object.keys(legacy as object);
      const modernKeys = Object.keys(modern as object);
      const allKeys = new Set([...legacyKeys, ...modernKeys]);

      for (const key of allKeys) {
        const childPath = path ? `${path}.${key}` : key;
        if (!modernKeys.includes(key)) {
          legacyOnly.push(childPath);
        } else if (!legacyKeys.includes(key)) {
          modernOnly.push(childPath);
        } else {
          this.compareObjects(
            (legacy as Record<string, unknown>)[key],
            (modern as Record<string, unknown>)[key],
            childPath,
            differences,
            legacyOnly,
            modernOnly,
            counts
          );
        }
      }
    } else if (Array.isArray(legacy) && Array.isArray(modern)) {
      const maxLen = Math.max(legacy.length, modern.length);
      for (let i = 0; i < maxLen; i++) {
        this.compareObjects(
          legacy[i],
          modern[i],
          `${path}[${i}]`,
          differences,
          legacyOnly,
          modernOnly,
          counts
        );
      }
    } else if (legacy !== modern) {
      differences.push({
        path,
        legacyValue: legacy,
        modernValue: modern,
        differenceType: 'value',
        severity: 'warning',
      });
      counts.total++;
    }
  }

  /**
   * Generate summary statistics from results
   */
  private generateSummary(results: ReplayResult[]): ReplaySummary {
    const withLegacyConsistency = results.filter(r => r.legacyConsistency);
    const withModernComparison = results.filter(r => r.modernComparison);

    const legacyConsistent = withLegacyConsistency.filter(
      r => r.legacyConsistency?.equivalent
    );
    const modernEquivalent = withModernComparison.filter(
      r => r.modernComparison?.equivalent
    );

    const avgLegacyLatency = results.length > 0
      ? results.reduce((sum, r) => sum + (r.legacyReplayDurationMs || 0), 0) / results.length
      : 0;
    const avgModernLatency = results.length > 0
      ? results.reduce((sum, r) => sum + (r.modernReplayDurationMs || 0), 0) / results.length
      : 0;

    // Find common difference patterns
    const differenceMap = new Map<string, { count: number; legacyValue: unknown; modernValue: unknown }>();
    for (const result of withModernComparison) {
      for (const diff of result.modernComparison?.differences || []) {
        const existing = differenceMap.get(diff.path);
        if (existing) {
          existing.count++;
        } else {
          differenceMap.set(diff.path, {
            count: 1,
            legacyValue: diff.legacyValue,
            modernValue: diff.modernValue,
          });
        }
      }
    }

    const commonDifferences: DifferencePattern[] = Array.from(differenceMap.entries())
      .sort((a, b) => b[1].count - a[1].count)
      .slice(0, 10)
      .map(([path, data]) => ({
        path,
        frequency: data.count,
        exampleLegacyValue: data.legacyValue,
        exampleModernValue: data.modernValue,
      }));

    return {
      legacyConsistencyRate: withLegacyConsistency.length > 0
        ? legacyConsistent.length / withLegacyConsistency.length
        : 1.0,
      modernEquivalenceRate: withModernComparison.length > 0
        ? modernEquivalent.length / withModernComparison.length
        : 0,
      averageLegacyLatencyMs: avgLegacyLatency,
      averageModernLatencyMs: avgModernLatency,
      latencyImprovement: avgLegacyLatency > 0
        ? ((avgLegacyLatency - avgModernLatency) / avgLegacyLatency) * 100
        : 0,
      commonDifferences,
    };
  }

  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
