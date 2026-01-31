/**
 * Side Effect Comparator - Compares side effects from execution
 */

import type { SideEffect, SideEffectDifference } from '../types.js';

// ============================================================================
// Side Effect Comparator
// ============================================================================

export class SideEffectComparator {
  /**
   * Compare two sets of side effects
   */
  compare(
    legacy: SideEffect[],
    migrated: SideEffect[]
  ): SideEffectDifference[] {
    const differences: SideEffectDifference[] = [];
    const migratedMatched = new Set<number>();

    // Match legacy side effects to migrated
    for (const legacyEffect of legacy) {
      const matchIndex = this.findMatch(legacyEffect, migrated, migratedMatched);

      if (matchIndex === -1) {
        differences.push({
          type: legacyEffect.type,
          operation: legacyEffect.operation,
          legacy: legacyEffect,
          migrated: undefined,
          diff: 'missing',
        });
      } else {
        migratedMatched.add(matchIndex);
        const migratedEffect = migrated[matchIndex]!;

        if (!this.effectsEqual(legacyEffect, migratedEffect)) {
          differences.push({
            type: legacyEffect.type,
            operation: legacyEffect.operation,
            legacy: legacyEffect,
            migrated: migratedEffect,
            diff: 'modified',
          });
        }
      }
    }

    // Find unmatched migrated side effects
    for (let i = 0; i < migrated.length; i++) {
      if (!migratedMatched.has(i)) {
        const migratedEffect = migrated[i]!;
        differences.push({
          type: migratedEffect.type,
          operation: migratedEffect.operation,
          legacy: undefined,
          migrated: migratedEffect,
          diff: 'added',
        });
      }
    }

    return differences;
  }

  /**
   * Find a matching side effect
   */
  private findMatch(
    effect: SideEffect,
    candidates: SideEffect[],
    excluded: Set<number>
  ): number {
    for (let i = 0; i < candidates.length; i++) {
      if (excluded.has(i)) continue;

      const candidate = candidates[i]!;
      if (
        effect.type === candidate.type &&
        effect.operation === candidate.operation &&
        effect.target === candidate.target
      ) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Check if two side effects are equal
   */
  private effectsEqual(a: SideEffect, b: SideEffect): boolean {
    if (a.type !== b.type) return false;
    if (a.operation !== b.operation) return false;
    if (a.target !== b.target) return false;

    // Deep compare data
    return JSON.stringify(a.data) === JSON.stringify(b.data);
  }

  /**
   * Group side effects by type
   */
  groupByType(effects: SideEffect[]): Record<SideEffect['type'], SideEffect[]> {
    const groups: Record<SideEffect['type'], SideEffect[]> = {
      database: [],
      file: [],
      network: [],
      queue: [],
      log: [],
    };

    for (const effect of effects) {
      groups[effect.type].push(effect);
    }

    return groups;
  }

  /**
   * Categorize differences by severity
   */
  categorizeDifferences(
    differences: SideEffectDifference[]
  ): CategorizedDifferences {
    const result: CategorizedDifferences = {
      critical: [],
      warning: [],
      info: [],
    };

    for (const diff of differences) {
      const severity = this.getSeverity(diff);
      result[severity].push(diff);
    }

    return result;
  }

  /**
   * Get severity of a side effect difference
   */
  private getSeverity(
    diff: SideEffectDifference
  ): 'critical' | 'warning' | 'info' {
    // Missing database operations are critical
    if (diff.type === 'database' && diff.diff === 'missing') {
      return 'critical';
    }

    // Missing network calls are critical
    if (diff.type === 'network' && diff.diff === 'missing') {
      return 'critical';
    }

    // Added operations are warnings
    if (diff.diff === 'added') {
      return 'warning';
    }

    // Modified operations severity depends on type
    if (diff.diff === 'modified') {
      if (diff.type === 'database' || diff.type === 'network') {
        return 'critical';
      }
      return 'warning';
    }

    // Log differences are info
    if (diff.type === 'log') {
      return 'info';
    }

    return 'warning';
  }
}

// ============================================================================
// Supporting Types
// ============================================================================

export interface CategorizedDifferences {
  critical: SideEffectDifference[];
  warning: SideEffectDifference[];
  info: SideEffectDifference[];
}

// ============================================================================
// Factory
// ============================================================================

export function createSideEffectComparator(): SideEffectComparator {
  return new SideEffectComparator();
}
