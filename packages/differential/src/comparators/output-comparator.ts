/**
 * Output Comparator - Compares execution outputs
 */

import type {
  DataValue,
  OutputDifference,
  DifferentialConfig,
} from '../types.js';

// ============================================================================
// Output Comparator
// ============================================================================

export class OutputComparator {
  private config: DifferentialConfig;

  constructor(config: Partial<DifferentialConfig> = {}) {
    this.config = { ...defaultConfig(), ...config };
  }

  /**
   * Compare two output value sets
   */
  compare(
    legacy: Record<string, DataValue>,
    migrated: Record<string, DataValue>
  ): OutputDifference[] {
    const differences: OutputDifference[] = [];
    const allKeys = new Set([...Object.keys(legacy), ...Object.keys(migrated)]);

    for (const key of allKeys) {
      const legacyValue = legacy[key];
      const migratedValue = migrated[key];

      if (!legacyValue && migratedValue) {
        differences.push({
          path: key,
          legacyValue: { type: 'null', value: null },
          migratedValue,
          diffType: 'added',
          severity: 'warning',
          explanation: `New output field "${key}" in migrated code`,
        });
      } else if (legacyValue && !migratedValue) {
        differences.push({
          path: key,
          legacyValue,
          migratedValue: { type: 'null', value: null },
          diffType: 'missing',
          severity: 'critical',
          explanation: `Output field "${key}" missing in migrated code`,
        });
      } else if (legacyValue && migratedValue) {
        const valueDiff = this.compareValues(key, legacyValue, migratedValue);
        if (valueDiff) {
          differences.push(valueDiff);
        }
      }
    }

    return differences;
  }

  /**
   * Compare two individual values
   */
  private compareValues(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    // Type mismatch
    if (legacy.type !== migrated.type) {
      // Check for equivalent types
      if (this.areTypesEquivalent(legacy.type, migrated.type)) {
        // Types are logically equivalent, continue with value comparison
      } else {
        return {
          path,
          legacyValue: legacy,
          migratedValue: migrated,
          diffType: 'type-mismatch',
          severity: 'critical',
          explanation: `Type changed from ${legacy.type} to ${migrated.type}`,
        };
      }
    }

    // Value comparison based on type
    switch (legacy.type) {
      case 'number':
      case 'decimal':
        return this.compareNumeric(path, legacy, migrated);

      case 'string':
        return this.compareString(path, legacy, migrated);

      case 'boolean':
        return this.compareBoolean(path, legacy, migrated);

      case 'date':
        return this.compareDate(path, legacy, migrated);

      case 'array':
        return this.compareArray(path, legacy, migrated);

      case 'object':
        return this.compareObject(path, legacy, migrated);

      case 'null':
        return this.compareNull(path, legacy, migrated);

      default:
        return null;
    }
  }

  /**
   * Compare numeric values
   */
  private compareNumeric(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    const legacyNum = Number(legacy.value);
    const migratedNum = Number(migrated.value);

    // Handle NaN
    if (isNaN(legacyNum) && isNaN(migratedNum)) {
      return null;
    }

    if (isNaN(legacyNum) !== isNaN(migratedNum)) {
      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity: 'critical',
        explanation: `One value is NaN while the other is not`,
      };
    }

    // Check within tolerance
    const diff = Math.abs(legacyNum - migratedNum);
    const tolerance = this.config.numericTolerance ?? 0.0001;

    if (diff > tolerance) {
      const severity = diff > 1 ? 'critical' : 'warning';
      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity,
        explanation: `Numeric difference: ${diff.toFixed(6)} (tolerance: ${tolerance})`,
      };
    }

    return null;
  }

  /**
   * Compare string values
   */
  private compareString(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    let legacyStr = String(legacy.value);
    let migratedStr = String(migrated.value);

    // Check for null equivalents
    const nullEquivs = this.config.nullEquivalents ?? [];
    const legacyIsNull = nullEquivs.includes(legacyStr);
    const migratedIsNull = nullEquivs.includes(migratedStr);

    if (legacyIsNull && migratedIsNull) {
      return null;
    }

    // Normalize strings if configured
    if (this.config.stringComparison === 'normalized') {
      legacyStr = this.normalizeString(legacyStr);
      migratedStr = this.normalizeString(migratedStr);
    }

    if (legacyStr !== migratedStr) {
      // Determine severity
      const severity = this.config.ignoreWhitespace && 
        legacyStr.replace(/\s/g, '') === migratedStr.replace(/\s/g, '')
        ? 'info'
        : 'critical';

      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity,
        explanation: `String values differ`,
      };
    }

    return null;
  }

  /**
   * Compare boolean values
   */
  private compareBoolean(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    if (Boolean(legacy.value) !== Boolean(migrated.value)) {
      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity: 'critical',
        explanation: `Boolean value changed from ${legacy.value} to ${migrated.value}`,
      };
    }
    return null;
  }

  /**
   * Compare date values
   */
  private compareDate(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    const legacyDate = new Date(legacy.value as string);
    const migratedDate = new Date(migrated.value as string);

    if (isNaN(legacyDate.getTime()) || isNaN(migratedDate.getTime())) {
      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity: 'critical',
        explanation: `Invalid date value detected`,
      };
    }

    // Compare timestamps
    if (legacyDate.getTime() !== migratedDate.getTime()) {
      // If date format tolerance is enabled, compare date parts only
      if (this.config.dateFormatTolerance) {
        const sameDay = 
          legacyDate.getFullYear() === migratedDate.getFullYear() &&
          legacyDate.getMonth() === migratedDate.getMonth() &&
          legacyDate.getDate() === migratedDate.getDate();
        
        if (sameDay) {
          return {
            path,
            legacyValue: legacy,
            migratedValue: migrated,
            diffType: 'changed',
            severity: 'info',
            explanation: `Time component differs but date is the same`,
          };
        }
      }

      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity: 'critical',
        explanation: `Date values differ by ${Math.abs(legacyDate.getTime() - migratedDate.getTime())}ms`,
      };
    }

    return null;
  }

  /**
   * Compare array values
   */
  private compareArray(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    const legacyArr = legacy.value as unknown[];
    const migratedArr = migrated.value as unknown[];

    if (legacyArr.length !== migratedArr.length) {
      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity: 'critical',
        explanation: `Array length changed from ${legacyArr.length} to ${migratedArr.length}`,
      };
    }

    // Compare elements
    for (let i = 0; i < legacyArr.length; i++) {
      const legacyEl = this.toDataValue(legacyArr[i]);
      const migratedEl = this.toDataValue(migratedArr[i]);
      const elDiff = this.compareValues(`${path}[${i}]`, legacyEl, migratedEl);
      if (elDiff) {
        return elDiff;
      }
    }

    return null;
  }

  /**
   * Compare object values
   */
  private compareObject(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    const legacyObj = legacy.value as Record<string, unknown>;
    const migratedObj = migrated.value as Record<string, unknown>;
    const allKeys = new Set([...Object.keys(legacyObj), ...Object.keys(migratedObj)]);

    for (const key of allKeys) {
      const newPath = `${path}.${key}`;
      const legacyVal = this.toDataValue(legacyObj[key]);
      const migratedVal = this.toDataValue(migratedObj[key]);

      if (!(key in legacyObj)) {
        return {
          path: newPath,
          legacyValue: { type: 'null', value: null },
          migratedValue: migratedVal,
          diffType: 'added',
          severity: 'warning',
          explanation: `New property "${key}" in migrated object`,
        };
      }

      if (!(key in migratedObj)) {
        return {
          path: newPath,
          legacyValue: legacyVal,
          migratedValue: { type: 'null', value: null },
          diffType: 'missing',
          severity: 'critical',
          explanation: `Property "${key}" missing in migrated object`,
        };
      }

      const propDiff = this.compareValues(newPath, legacyVal, migratedVal);
      if (propDiff) {
        return propDiff;
      }
    }

    return null;
  }

  /**
   * Compare null values
   */
  private compareNull(
    path: string,
    legacy: DataValue,
    migrated: DataValue
  ): OutputDifference | null {
    const legacyIsNull = legacy.value === null || legacy.value === undefined;
    const migratedIsNull = migrated.value === null || migrated.value === undefined;

    if (legacyIsNull !== migratedIsNull) {
      return {
        path,
        legacyValue: legacy,
        migratedValue: migrated,
        diffType: 'changed',
        severity: 'critical',
        explanation: `Null/undefined status changed`,
      };
    }

    return null;
  }

  // ============================================================================
  // Helper Methods
  // ============================================================================

  private areTypesEquivalent(type1: string, type2: string): boolean {
    const equivalentGroups = [
      ['number', 'decimal'],
      ['null', 'undefined'],
    ];

    for (const group of equivalentGroups) {
      if (group.includes(type1) && group.includes(type2)) {
        return true;
      }
    }

    return false;
  }

  private normalizeString(str: string): string {
    return str
      .trim()
      .replace(/\s+/g, ' ')
      .toLowerCase();
  }

  private toDataValue(value: unknown): DataValue {
    if (value === null || value === undefined) {
      return { type: 'null', value: null };
    }
    if (typeof value === 'string') {
      return { type: 'string', value };
    }
    if (typeof value === 'number') {
      return { type: 'number', value };
    }
    if (typeof value === 'boolean') {
      return { type: 'boolean', value };
    }
    if (Array.isArray(value)) {
      return { type: 'array', value };
    }
    if (typeof value === 'object') {
      return { type: 'object', value };
    }
    return { type: 'string', value: String(value) };
  }
}

// ============================================================================
// Default Config Helper
// ============================================================================

function defaultConfig(): DifferentialConfig {
  return {
    maxIterations: 1000,
    timeout: 30000,
    fuzzingDepth: 3,
    numericTolerance: 0.0001,
    stringComparison: 'normalized',
    dateFormatTolerance: true,
    nullEquivalents: ['', 'null', 'NULL', 'None', 'undefined', 'N/A'],
    ignoreWhitespace: true,
    parallelExecution: true,
    maxConcurrency: 4,
  };
}

// ============================================================================
// Factory
// ============================================================================

export function createOutputComparator(
  config?: Partial<DifferentialConfig>
): OutputComparator {
  return new OutputComparator(config);
}
