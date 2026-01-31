/**
 * Input Fuzzer - Generates test inputs for differential analysis
 */

import type {
  InputGenerator,
  InputStrategy,
  InputConstraint,
  GeneratedInput,
  DataValue,
} from '../types.js';

// ============================================================================
// Input Fuzzer
// ============================================================================

export class InputFuzzer implements InputGenerator {
  id: string;
  strategy: InputStrategy;
  constraints: InputConstraint[];

  private rng: () => number;

  constructor(
    strategy: InputStrategy = 'boundary',
    constraints: InputConstraint[] = []
  ) {
    this.id = `fuzzer_${Date.now()}`;
    this.strategy = strategy;
    this.constraints = constraints;
    this.rng = Math.random;
  }

  /**
   * Set custom random number generator for reproducibility
   */
  setRng(rng: () => number): void {
    this.rng = rng;
  }

  /**
   * Generate a batch of test inputs
   */
  generate(count: number): GeneratedInput[] {
    const inputs: GeneratedInput[] = [];

    switch (this.strategy) {
      case 'boundary':
        inputs.push(...this.generateBoundaryInputs(count));
        break;
      case 'random':
        inputs.push(...this.generateRandomInputs(count));
        break;
      case 'symbolic':
        inputs.push(...this.generateSymbolicInputs(count));
        break;
      case 'property-based':
        inputs.push(...this.generatePropertyBasedInputs(count));
        break;
      case 'mutation':
        inputs.push(...this.generateMutationInputs(count));
        break;
      default:
        inputs.push(...this.generateRandomInputs(count));
    }

    return inputs;
  }

  /**
   * Generate boundary value test inputs
   */
  private generateBoundaryInputs(count: number): GeneratedInput[] {
    const inputs: GeneratedInput[] = [];

    for (const constraint of this.constraints) {
      // Min boundary
      if (constraint.constraints.min !== undefined) {
        inputs.push(this.createInput(constraint, constraint.constraints.min, 'min-boundary'));
        inputs.push(this.createInput(constraint, constraint.constraints.min - 1, 'below-min'));
        inputs.push(this.createInput(constraint, constraint.constraints.min + 1, 'above-min'));
      }

      // Max boundary
      if (constraint.constraints.max !== undefined) {
        inputs.push(this.createInput(constraint, constraint.constraints.max, 'max-boundary'));
        inputs.push(this.createInput(constraint, constraint.constraints.max + 1, 'above-max'));
        inputs.push(this.createInput(constraint, constraint.constraints.max - 1, 'below-max'));
      }

      // Null/empty
      if (constraint.constraints.nullable) {
        inputs.push(this.createNullInput(constraint));
      }

      // Empty for strings/arrays
      if (constraint.type === 'string' || constraint.type === 'array') {
        inputs.push(this.createEmptyInput(constraint));
      }

      // Length boundaries
      if (constraint.constraints.length) {
        const { min: minLen, max: maxLen } = constraint.constraints.length;
        if (minLen !== undefined) {
          inputs.push(this.createInput(constraint, this.generateOfLength(constraint.type, minLen), 'min-length'));
        }
        if (maxLen !== undefined) {
          inputs.push(this.createInput(constraint, this.generateOfLength(constraint.type, maxLen), 'max-length'));
        }
      }
    }

    // Fill remaining with random
    while (inputs.length < count) {
      inputs.push(...this.generateRandomInputs(1));
    }

    return inputs.slice(0, count);
  }

  /**
   * Generate random test inputs
   */
  private generateRandomInputs(count: number): GeneratedInput[] {
    const inputs: GeneratedInput[] = [];

    for (let i = 0; i < count; i++) {
      const values: Record<string, DataValue> = {};

      for (const constraint of this.constraints) {
        values[constraint.field] = this.generateRandomValue(constraint);
      }

      inputs.push({
        id: `random_${Date.now()}_${i}`,
        strategy: 'random',
        values,
        rationale: 'Randomly generated input',
      });
    }

    return inputs;
  }

  /**
   * Generate symbolic execution inspired inputs
   */
  private generateSymbolicInputs(count: number): GeneratedInput[] {
    const inputs: GeneratedInput[] = [];

    // Generate inputs that exercise different code paths
    // This is a simplified version - real symbolic execution would analyze code
    for (let i = 0; i < count; i++) {
      const values: Record<string, DataValue> = {};

      for (const constraint of this.constraints) {
        // Alternate between different value categories
        const category = i % 5;
        values[constraint.field] = this.generateCategorizedValue(constraint, category);
      }

      inputs.push({
        id: `symbolic_${Date.now()}_${i}`,
        strategy: 'symbolic',
        values,
        rationale: `Path exploration category ${i % 5}`,
      });
    }

    return inputs;
  }

  /**
   * Generate property-based test inputs
   */
  private generatePropertyBasedInputs(count: number): GeneratedInput[] {
    const inputs: GeneratedInput[] = [];

    // Generate inputs with specific properties
    const properties = ['positive', 'negative', 'zero', 'large', 'small'];

    for (let i = 0; i < count; i++) {
      const property = properties[i % properties.length]!;
      const values: Record<string, DataValue> = {};

      for (const constraint of this.constraints) {
        values[constraint.field] = this.generateWithProperty(constraint, property);
      }

      inputs.push({
        id: `property_${Date.now()}_${i}`,
        strategy: 'property-based',
        values,
        rationale: `Property: ${property}`,
      });
    }

    return inputs;
  }

  /**
   * Generate mutation-based inputs
   */
  private generateMutationInputs(count: number): GeneratedInput[] {
    const inputs: GeneratedInput[] = [];
    const baseInputs = this.generateRandomInputs(Math.ceil(count / 5));

    for (const baseInput of baseInputs) {
      // Generate mutations
      inputs.push(baseInput);

      for (const field of Object.keys(baseInput.values)) {
        const mutated = this.mutateValue(baseInput.values[field]!);
        inputs.push({
          id: `mutation_${Date.now()}_${inputs.length}`,
          strategy: 'mutation',
          values: { ...baseInput.values, [field]: mutated },
          rationale: `Mutated field: ${field}`,
        });

        if (inputs.length >= count) break;
      }

      if (inputs.length >= count) break;
    }

    return inputs.slice(0, count);
  }

  // ============================================================================
  // Helper Methods
  // ============================================================================

  private createInput(
    constraint: InputConstraint,
    value: unknown,
    rationale: string
  ): GeneratedInput {
    const values: Record<string, DataValue> = {};
    
    for (const c of this.constraints) {
      if (c.field === constraint.field) {
        values[c.field] = { type: c.type, value };
      } else {
        values[c.field] = this.generateRandomValue(c);
      }
    }

    return {
      id: `${rationale}_${Date.now()}`,
      strategy: this.strategy,
      values,
      rationale,
    };
  }

  private createNullInput(constraint: InputConstraint): GeneratedInput {
    const values: Record<string, DataValue> = {};
    
    for (const c of this.constraints) {
      if (c.field === constraint.field) {
        values[c.field] = { type: 'null', value: null };
      } else {
        values[c.field] = this.generateRandomValue(c);
      }
    }

    return {
      id: `null_${constraint.field}_${Date.now()}`,
      strategy: this.strategy,
      values,
      rationale: `Null value for ${constraint.field}`,
    };
  }

  private createEmptyInput(constraint: InputConstraint): GeneratedInput {
    const values: Record<string, DataValue> = {};
    
    for (const c of this.constraints) {
      if (c.field === constraint.field) {
        values[c.field] = { 
          type: c.type, 
          value: c.type === 'string' ? '' : [] 
        };
      } else {
        values[c.field] = this.generateRandomValue(c);
      }
    }

    return {
      id: `empty_${constraint.field}_${Date.now()}`,
      strategy: this.strategy,
      values,
      rationale: `Empty value for ${constraint.field}`,
    };
  }

  private generateRandomValue(constraint: InputConstraint): DataValue {
    const { type, constraints: c } = constraint;

    switch (type) {
      case 'number':
        const min = c.min ?? -1000000;
        const max = c.max ?? 1000000;
        return { type: 'number', value: min + this.rng() * (max - min) };

      case 'string':
        const length = c.length?.max ?? 50;
        return { type: 'string', value: this.generateRandomString(length) };

      case 'boolean':
        return { type: 'boolean', value: this.rng() > 0.5 };

      case 'decimal':
        const dMin = c.min ?? -1000000;
        const dMax = c.max ?? 1000000;
        const precision = constraint.constraints.length?.max ?? 2;
        const val = dMin + this.rng() * (dMax - dMin);
        return { 
          type: 'decimal', 
          value: Number(val.toFixed(precision)),
          precision 
        };

      case 'date':
        const start = Date.now() - 365 * 24 * 60 * 60 * 1000;
        const end = Date.now() + 365 * 24 * 60 * 60 * 1000;
        return { 
          type: 'date', 
          value: new Date(start + this.rng() * (end - start)).toISOString() 
        };

      case 'array':
        const arrLen = c.length?.max ?? 10;
        return { 
          type: 'array', 
          value: Array.from({ length: Math.floor(this.rng() * arrLen) }, () => this.rng()) 
        };

      case 'object':
        return { type: 'object', value: {} };

      case 'null':
        return { type: 'null', value: null };

      default:
        return { type: 'string', value: '' };
    }
  }

  private generateCategorizedValue(constraint: InputConstraint, category: number): DataValue {
    const { type, constraints: c } = constraint;

    if (type === 'number' || type === 'decimal') {
      const categories = [
        c.min ?? 0,
        c.max ?? 1000,
        0,
        (c.min ?? 0) + ((c.max ?? 1000) - (c.min ?? 0)) / 2,
        c.min ?? 0 > 0 ? -1 : (c.max ?? 0) + 1,
      ];
      return { type, value: categories[category] ?? 0 };
    }

    return this.generateRandomValue(constraint);
  }

  private generateWithProperty(constraint: InputConstraint, property: string): DataValue {
    const { type, constraints: c } = constraint;

    if (type === 'number' || type === 'decimal') {
      switch (property) {
        case 'positive':
          return { type, value: Math.abs(c.max ?? 100) };
        case 'negative':
          return { type, value: -(Math.abs(c.min ?? 100)) };
        case 'zero':
          return { type, value: 0 };
        case 'large':
          return { type, value: c.max ?? Number.MAX_SAFE_INTEGER / 2 };
        case 'small':
          return { type, value: c.min ?? Number.MIN_SAFE_INTEGER / 2 };
      }
    }

    return this.generateRandomValue(constraint);
  }

  private mutateValue(value: DataValue): DataValue {
    switch (value.type) {
      case 'number':
      case 'decimal':
        const numVal = value.value as number;
        const mutations = [numVal + 1, numVal - 1, numVal * 2, numVal / 2, -numVal, 0];
        return { ...value, value: mutations[Math.floor(this.rng() * mutations.length)] };

      case 'string':
        const strVal = value.value as string;
        const strMutations = [
          strVal.toUpperCase(),
          strVal.toLowerCase(),
          strVal + '_mutated',
          strVal.slice(1),
          strVal.slice(0, -1),
          '',
        ];
        return { ...value, value: strMutations[Math.floor(this.rng() * strMutations.length)] };

      case 'boolean':
        return { ...value, value: !(value.value as boolean) };

      default:
        return value;
    }
  }

  private generateRandomString(length: number): string {
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let result = '';
    for (let i = 0; i < length; i++) {
      result += chars.charAt(Math.floor(this.rng() * chars.length));
    }
    return result;
  }

  private generateOfLength(type: string, length: number): unknown {
    if (type === 'string') {
      return this.generateRandomString(length);
    }
    if (type === 'array') {
      return Array.from({ length }, () => this.rng());
    }
    return null;
  }
}

// ============================================================================
// Factory
// ============================================================================

export function createInputFuzzer(
  strategy: InputStrategy = 'boundary',
  constraints: InputConstraint[] = []
): InputFuzzer {
  return new InputFuzzer(strategy, constraints);
}
