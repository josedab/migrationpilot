/**
 * Property-Based Tester
 * 
 * Implements property-based testing for discovering edge cases
 */

import type { PropertyDefinition, SystemAdapter } from './types.js';

export interface PropertyTestConfig {
  maxIterations: number;
  shrinkAttempts: number;
  seed?: number;
  timeout?: number;
}

export interface PropertyTestResult {
  property: PropertyDefinition;
  passed: boolean;
  iterations: number;
  failingInput?: Record<string, unknown>;
  shrunkInput?: Record<string, unknown>;
  error?: string;
  duration: number;
}

export class PropertyTester {
  private legacyAdapter: SystemAdapter;
  private modernAdapter: SystemAdapter;
  private config: PropertyTestConfig;

  constructor(
    legacyAdapter: SystemAdapter,
    modernAdapter: SystemAdapter,
    config?: Partial<PropertyTestConfig>
  ) {
    this.legacyAdapter = legacyAdapter;
    this.modernAdapter = modernAdapter;
    this.config = {
      maxIterations: 100,
      shrinkAttempts: 50,
      timeout: 30000,
      ...config,
    };
  }

  /**
   * Test a property
   */
  async testProperty(property: PropertyDefinition): Promise<PropertyTestResult> {
    const startTime = Date.now();
    const iterations = property.iterations ?? this.config.maxIterations;
    
    for (let i = 0; i < iterations; i++) {
      const inputs = property.inputGenerator();
      
      try {
        const [legacyResult, modernResult] = await Promise.all([
          this.legacyAdapter.execute(inputs),
          this.modernAdapter.execute(inputs),
        ]);

        const holds = property.property(
          legacyResult.output,
          modernResult.output,
          inputs
        );

        if (!holds) {
          // Try to shrink the failing input
          const shrunkInput = await this.shrink(property, inputs);
          
          return {
            property,
            passed: false,
            iterations: i + 1,
            failingInput: inputs,
            shrunkInput,
            duration: Date.now() - startTime,
          };
        }
      } catch (error) {
        return {
          property,
          passed: false,
          iterations: i + 1,
          failingInput: inputs,
          error: error instanceof Error ? error.message : String(error),
          duration: Date.now() - startTime,
        };
      }
    }

    return {
      property,
      passed: true,
      iterations,
      duration: Date.now() - startTime,
    };
  }

  /**
   * Test multiple properties
   */
  async testProperties(properties: PropertyDefinition[]): Promise<PropertyTestResult[]> {
    const results: PropertyTestResult[] = [];
    
    for (const property of properties) {
      const result = await this.testProperty(property);
      results.push(result);
      
      // Stop early if critical property fails
      if (!result.passed && property.name.includes('critical')) {
        break;
      }
    }
    
    return results;
  }

  /**
   * Shrink a failing input to find minimal reproduction
   */
  private async shrink(
    property: PropertyDefinition,
    failingInput: Record<string, unknown>
  ): Promise<Record<string, unknown>> {
    let smallestFailure = failingInput;
    
    for (let i = 0; i < this.config.shrinkAttempts; i++) {
      const shrunk = this.shrinkInput(smallestFailure);
      
      try {
        const [legacyResult, modernResult] = await Promise.all([
          this.legacyAdapter.execute(shrunk),
          this.modernAdapter.execute(shrunk),
        ]);

        const holds = property.property(
          legacyResult.output,
          modernResult.output,
          shrunk
        );

        if (!holds) {
          // Found a smaller failing case
          smallestFailure = shrunk;
        }
      } catch {
        // Error during shrinking, keep current smallest
      }
    }
    
    return smallestFailure;
  }

  /**
   * Shrink an input value
   */
  private shrinkInput(input: Record<string, unknown>): Record<string, unknown> {
    const shrunk: Record<string, unknown> = {};
    
    for (const [key, value] of Object.entries(input)) {
      shrunk[key] = this.shrinkValue(value);
    }
    
    return shrunk;
  }

  private shrinkValue(value: unknown): unknown {
    if (typeof value === 'number') {
      // Shrink towards zero
      if (value === 0) return 0;
      if (value > 0) return Math.floor(value / 2);
      return Math.ceil(value / 2);
    }
    
    if (typeof value === 'string') {
      // Shrink string length
      if (value.length <= 1) return value;
      return value.substring(0, Math.floor(value.length / 2));
    }
    
    if (Array.isArray(value)) {
      // Shrink array length
      if (value.length <= 1) return value;
      return value.slice(0, Math.floor(value.length / 2));
    }
    
    return value;
  }

  /**
   * Generate common property definitions
   */
  static commonProperties = {
    /**
     * Outputs should be equal for same inputs
     */
    outputEquality: (
      inputGenerator: () => Record<string, unknown>,
      tolerance: number = 0.001
    ): PropertyDefinition => ({
      name: 'Output Equality',
      description: 'Modern output should equal legacy output',
      inputGenerator,
      property: (legacy, modern) => {
        return PropertyTester.deepEquals(legacy, modern, tolerance);
      },
    }),

    /**
     * Both systems should handle the same input types
     */
    inputTypeConsistency: (
      inputGenerator: () => Record<string, unknown>
    ): PropertyDefinition => ({
      name: 'Input Type Consistency',
      description: 'Both systems accept same input types',
      inputGenerator,
      property: (legacy, modern) => {
        // Both succeed or both fail
        const legacyFailed = legacy === null || legacy === undefined;
        const modernFailed = modern === null || modern === undefined;
        return legacyFailed === modernFailed;
      },
    }),

    /**
     * Monotonic relationship preserved
     */
    monotonicPreservation: (
      inputGenerator: () => Record<string, unknown>,
      outputKey: string,
      inputKey: string,
      direction: 'increasing' | 'decreasing'
    ): PropertyDefinition => ({
      name: 'Monotonic Preservation',
      description: `${outputKey} should be ${direction} with ${inputKey}`,
      inputGenerator,
      property: (legacy, modern) => {
        // Check if monotonic relationship is preserved
        const legacyObj = legacy as Record<string, number>;
        const modernObj = modern as Record<string, number>;
        return Math.sign(legacyObj[outputKey] ?? 0) === Math.sign(modernObj[outputKey] ?? 0);
      },
    }),
  };

  /**
   * Deep equality check with tolerance
   */
  private static deepEquals(a: unknown, b: unknown, tolerance: number): boolean {
    if (a === b) return true;
    if (a === null || b === null) return false;
    if (a === undefined || b === undefined) return false;

    if (typeof a === 'number' && typeof b === 'number') {
      return Math.abs(a - b) <= tolerance;
    }

    if (typeof a !== typeof b) return false;

    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false;
      return a.every((item, i) => PropertyTester.deepEquals(item, b[i], tolerance));
    }

    if (typeof a === 'object' && typeof b === 'object') {
      const aObj = a as Record<string, unknown>;
      const bObj = b as Record<string, unknown>;
      const keys = new Set([...Object.keys(aObj), ...Object.keys(bObj)]);
      return Array.from(keys).every(key => 
        PropertyTester.deepEquals(aObj[key], bObj[key], tolerance)
      );
    }

    return a === b;
  }
}

/**
 * Input generators for common data types
 */
export const generators = {
  integer: (min = -1000000, max = 1000000) => (): number => {
    return Math.floor(Math.random() * (max - min + 1)) + min;
  },

  decimal: (min = -1000000, max = 1000000, precision = 2) => (): number => {
    const value = Math.random() * (max - min) + min;
    return Number(value.toFixed(precision));
  },

  money: (min = 0, max = 1000000) => (): number => {
    return Number((Math.random() * (max - min) + min).toFixed(2));
  },

  percentage: () => (): number => {
    return Number((Math.random() * 100).toFixed(4));
  },

  boolean: () => (): boolean => {
    return Math.random() > 0.5;
  },

  choice: <T>(options: T[]) => (): T | undefined => {
    if (options.length === 0) return undefined;
    return options[Math.floor(Math.random() * options.length)];
  },

  string: (minLen = 0, maxLen = 100) => (): string => {
    const length = Math.floor(Math.random() * (maxLen - minLen + 1)) + minLen;
    return Array.from({ length }, () => 
      String.fromCharCode(Math.floor(Math.random() * 26) + 97)
    ).join('');
  },

  date: (start = new Date(2000, 0, 1), end = new Date()) => (): Date => {
    return new Date(start.getTime() + Math.random() * (end.getTime() - start.getTime()));
  },

  combine: (generators: Record<string, () => unknown>) => (): Record<string, unknown> => {
    const result: Record<string, unknown> = {};
    for (const [key, gen] of Object.entries(generators)) {
      result[key] = gen();
    }
    return result;
  },
};
