/**
 * Shadow Traffic Capture System
 * 
 * Captures production traffic for replay and comparison testing
 */

import { generateId } from '@migrationpilot/core';
import type {
  ShadowConfig,
  ShadowCapture,
  ComparisonResult,
  FieldDifference,
  ToleranceConfig,
} from '../types.js';

const DEFAULT_TOLERANCE_CONFIG: ToleranceConfig = {
  numericTolerance: 0.0001,
  stringNormalization: 'trim',
  dateToleranceMs: 1000,
  ignoreFields: [],
};

export class ShadowTrafficCapture {
  private config: ShadowConfig;
  private captures: Map<string, ShadowCapture> = new Map();
  private captureCount = 0;
  private storageUsedBytes = 0;

  constructor(config: ShadowConfig) {
    this.config = {
      ...config,
      toleranceConfig: { ...DEFAULT_TOLERANCE_CONFIG, ...config.toleranceConfig },
    };
  }

  /**
   * Check if we should capture this request based on sampling and patterns
   */
  shouldCapture(requestPath?: string): boolean {
    if (!this.config.enabled) return false;
    
    // Check storage limits
    if (this.storageUsedBytes >= this.config.maxStorageMB * 1024 * 1024) {
      return false;
    }
    
    // Check sampling rate
    if (Math.random() > this.config.samplingRate) {
      return false;
    }
    
    // Check include/exclude patterns
    if (requestPath) {
      if (this.config.excludePatterns?.some(p => requestPath.match(p))) {
        return false;
      }
      if (this.config.includePatterns?.length && 
          !this.config.includePatterns.some(p => requestPath.match(p))) {
        return false;
      }
    }
    
    return true;
  }

  /**
   * Capture a request/response pair from the legacy system
   */
  captureRequest(
    requestData: unknown,
    requestMetadata?: Record<string, unknown>
  ): string {
    const captureId = generateId();
    const sessionId = generateId();
    
    const capture: ShadowCapture = {
      id: captureId,
      projectId: this.config.projectId,
      sessionId,
      requestTimestamp: new Date(),
      requestData: this.config.captureRequest ? requestData : undefined,
      requestMetadata,
      legacyResponse: undefined,
      legacyDurationMs: 0,
      capturedAt: new Date(),
      processed: false,
    };
    
    this.captures.set(captureId, capture);
    this.captureCount++;
    
    return captureId;
  }

  /**
   * Record the legacy system's response
   */
  recordLegacyResponse(
    captureId: string,
    response: unknown,
    durationMs: number,
    error?: string
  ): void {
    const capture = this.captures.get(captureId);
    if (!capture) return;
    
    capture.legacyResponse = this.config.captureResponse ? response : undefined;
    capture.legacyDurationMs = durationMs;
    capture.legacyError = error;
    
    this.updateStorageUsage(capture);
  }

  /**
   * Record the modern system's response for comparison
   */
  recordModernResponse(
    captureId: string,
    response: unknown,
    durationMs: number,
    error?: string
  ): void {
    const capture = this.captures.get(captureId);
    if (!capture) return;
    
    capture.modernResponse = response;
    capture.modernDurationMs = durationMs;
    capture.modernError = error;
    
    // Perform comparison if enabled
    if (this.config.enableComparison && capture.legacyResponse !== undefined) {
      capture.comparisonResult = this.compare(
        capture.legacyResponse,
        capture.modernResponse
      );
    }
    
    capture.processed = true;
    this.updateStorageUsage(capture);
  }

  /**
   * Compare legacy and modern responses
   */
  compare(legacyResponse: unknown, modernResponse: unknown): ComparisonResult {
    const differences: FieldDifference[] = [];
    const legacyOnly: string[] = [];
    const modernOnly: string[] = [];
    let matchedFields = 0;
    let totalFields = 0;
    
    this.compareValues(
      legacyResponse,
      modernResponse,
      '',
      differences,
      legacyOnly,
      modernOnly,
      { matched: 0, total: 0 }
    );
    
    // Calculate totals from recursive comparison
    totalFields = differences.length + legacyOnly.length + modernOnly.length + matchedFields;
    
    const equivalent = differences.length === 0 && 
                      legacyOnly.length === 0 && 
                      modernOnly.length === 0;
    
    const confidence = totalFields > 0 
      ? matchedFields / totalFields 
      : 1.0;
    
    return {
      equivalent,
      differences,
      legacyOnly,
      modernOnly,
      matchedFields,
      totalFields,
      confidence,
    };
  }

  private compareValues(
    legacy: unknown,
    modern: unknown,
    path: string,
    differences: FieldDifference[],
    legacyOnly: string[],
    modernOnly: string[],
    counts: { matched: number; total: number }
  ): void {
    const tolerance = this.config.toleranceConfig!;
    
    // Check if field should be ignored
    if (tolerance.ignoreFields.includes(path)) {
      return;
    }
    
    counts.total++;
    
    // Handle null/undefined
    if (legacy === null || legacy === undefined) {
      if (modern !== null && modern !== undefined) {
        differences.push({
          path,
          legacyValue: legacy,
          modernValue: modern,
          differenceType: 'missing',
          severity: 'warning',
        });
      } else {
        counts.matched++;
      }
      return;
    }
    
    if (modern === null || modern === undefined) {
      differences.push({
        path,
        legacyValue: legacy,
        modernValue: modern,
        differenceType: 'extra',
        severity: 'warning',
      });
      return;
    }
    
    // Type mismatch
    if (typeof legacy !== typeof modern) {
      differences.push({
        path,
        legacyValue: legacy,
        modernValue: modern,
        differenceType: 'type',
        severity: 'critical',
      });
      return;
    }
    
    // Compare by type
    if (typeof legacy === 'number') {
      if (Math.abs(legacy - (modern as number)) > tolerance.numericTolerance) {
        differences.push({
          path,
          legacyValue: legacy,
          modernValue: modern,
          differenceType: 'tolerance',
          severity: 'warning',
          toleranceExceeded: true,
        });
      } else {
        counts.matched++;
      }
    } else if (typeof legacy === 'string') {
      const normalizedLegacy = this.normalizeString(legacy, tolerance.stringNormalization);
      const normalizedModern = this.normalizeString(modern as string, tolerance.stringNormalization);
      
      if (normalizedLegacy !== normalizedModern) {
        differences.push({
          path,
          legacyValue: legacy,
          modernValue: modern,
          differenceType: 'value',
          severity: 'warning',
        });
      } else {
        counts.matched++;
      }
    } else if (legacy instanceof Date || this.isDateString(legacy)) {
      const legacyTime = legacy instanceof Date ? legacy.getTime() : new Date(legacy as string).getTime();
      const modernTime = modern instanceof Date ? modern.getTime() : new Date(modern as string).getTime();
      
      if (Math.abs(legacyTime - modernTime) > tolerance.dateToleranceMs) {
        differences.push({
          path,
          legacyValue: legacy,
          modernValue: modern,
          differenceType: 'tolerance',
          severity: 'warning',
          toleranceExceeded: true,
        });
      } else {
        counts.matched++;
      }
    } else if (Array.isArray(legacy)) {
      if (!Array.isArray(modern)) {
        differences.push({
          path,
          legacyValue: legacy,
          modernValue: modern,
          differenceType: 'type',
          severity: 'critical',
        });
        return;
      }
      
      if (legacy.length !== modern.length) {
        differences.push({
          path: `${path}.length`,
          legacyValue: legacy.length,
          modernValue: modern.length,
          differenceType: 'value',
          severity: 'warning',
        });
      }
      
      const maxLen = Math.max(legacy.length, modern.length);
      for (let i = 0; i < maxLen; i++) {
        this.compareValues(
          legacy[i],
          modern[i],
          `${path}[${i}]`,
          differences,
          legacyOnly,
          modernOnly,
          counts
        );
      }
    } else if (typeof legacy === 'object') {
      const legacyKeys = Object.keys(legacy as object);
      const modernKeys = Object.keys(modern as object);
      
      const allKeys = new Set([...legacyKeys, ...modernKeys]);
      
      for (const key of allKeys) {
        const childPath = path ? `${path}.${key}` : key;
        
        if (!legacyKeys.includes(key)) {
          modernOnly.push(childPath);
          counts.total++;
        } else if (!modernKeys.includes(key)) {
          legacyOnly.push(childPath);
          counts.total++;
        } else {
          this.compareValues(
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
    } else if (legacy !== modern) {
      differences.push({
        path,
        legacyValue: legacy,
        modernValue: modern,
        differenceType: 'value',
        severity: 'warning',
      });
    } else {
      counts.matched++;
    }
  }

  private normalizeString(
    str: string,
    normalization: ToleranceConfig['stringNormalization']
  ): string {
    switch (normalization) {
      case 'trim':
        return str.trim();
      case 'lowercase':
        return str.toLowerCase().trim();
      case 'alphanumeric':
        return str.replace(/[^a-zA-Z0-9]/g, '').toLowerCase();
      default:
        return str;
    }
  }

  private isDateString(value: unknown): boolean {
    if (typeof value !== 'string') return false;
    const date = new Date(value);
    return !isNaN(date.getTime());
  }

  private updateStorageUsage(capture: ShadowCapture): void {
    // Estimate storage usage
    const json = JSON.stringify(capture);
    this.storageUsedBytes += json.length * 2; // UTF-16
  }

  /**
   * Get a specific capture by ID
   */
  getCapture(captureId: string): ShadowCapture | undefined {
    return this.captures.get(captureId);
  }

  /**
   * List captures with optional filtering
   */
  listCaptures(filter?: {
    startDate?: Date;
    endDate?: Date;
    hasErrors?: boolean;
    processed?: boolean;
    limit?: number;
  }): ShadowCapture[] {
    let captures = Array.from(this.captures.values());
    
    if (filter) {
      if (filter.startDate) {
        captures = captures.filter(c => c.capturedAt >= filter.startDate!);
      }
      if (filter.endDate) {
        captures = captures.filter(c => c.capturedAt <= filter.endDate!);
      }
      if (filter.hasErrors !== undefined) {
        captures = captures.filter(c => 
          (!!c.legacyError || !!c.modernError) === filter.hasErrors
        );
      }
      if (filter.processed !== undefined) {
        captures = captures.filter(c => c.processed === filter.processed);
      }
      if (filter.limit) {
        captures = captures.slice(0, filter.limit);
      }
    }
    
    return captures;
  }

  /**
   * Get comparison statistics
   */
  getStatistics(): ShadowCaptureStatistics {
    const captures = Array.from(this.captures.values());
    const processed = captures.filter(c => c.processed);
    const withComparison = processed.filter(c => c.comparisonResult);
    const equivalent = withComparison.filter(c => c.comparisonResult?.equivalent);
    
    const avgLegacyDuration = captures.length > 0
      ? captures.reduce((sum, c) => sum + c.legacyDurationMs, 0) / captures.length
      : 0;
    
    const avgModernDuration = processed.length > 0
      ? processed.reduce((sum, c) => sum + (c.modernDurationMs || 0), 0) / processed.length
      : 0;
    
    return {
      totalCaptures: this.captureCount,
      processedCaptures: processed.length,
      equivalentResponses: equivalent.length,
      differingResponses: withComparison.length - equivalent.length,
      averageLegacyDurationMs: avgLegacyDuration,
      averageModernDurationMs: avgModernDuration,
      storageUsedMB: this.storageUsedBytes / (1024 * 1024),
      storageMaxMB: this.config.maxStorageMB,
    };
  }

  /**
   * Clean up old captures based on retention policy
   */
  cleanup(): number {
    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - this.config.retentionDays);
    
    let removed = 0;
    for (const [id, capture] of this.captures) {
      if (capture.capturedAt < cutoffDate) {
        this.captures.delete(id);
        removed++;
      }
    }
    
    return removed;
  }

  /**
   * Export captures for analysis
   */
  exportCaptures(format: 'json' | 'csv' = 'json'): string {
    const captures = Array.from(this.captures.values());
    
    if (format === 'csv') {
      const headers = [
        'id', 'sessionId', 'requestTimestamp', 'legacyDurationMs', 
        'modernDurationMs', 'equivalent', 'differenceCount'
      ];
      const rows = captures.map(c => [
        c.id,
        c.sessionId,
        c.requestTimestamp.toISOString(),
        c.legacyDurationMs,
        c.modernDurationMs || '',
        c.comparisonResult?.equivalent ?? '',
        c.comparisonResult?.differences.length ?? '',
      ]);
      return [headers.join(','), ...rows.map(r => r.join(','))].join('\n');
    }
    
    return JSON.stringify(captures, null, 2);
  }
}

export interface ShadowCaptureStatistics {
  totalCaptures: number;
  processedCaptures: number;
  equivalentResponses: number;
  differingResponses: number;
  averageLegacyDurationMs: number;
  averageModernDurationMs: number;
  storageUsedMB: number;
  storageMaxMB: number;
}
