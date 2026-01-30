/**
 * Test Case Generator
 * 
 * Generates test cases from business rules using various strategies
 */

import type { TestCase, TestGenerationStrategy, TestBusinessRule } from './types.js';
import { randomUUID } from 'crypto';

export class TestCaseGenerator {
  
  /**
   * Generate test cases from business rules
   */
  generateFromRules(rules: TestBusinessRule[]): TestCase[] {
    const testCases: TestCase[] = [];
    
    for (const rule of rules) {
      // Boundary value tests
      testCases.push(...this.generateBoundaryTests(rule));
      
      // Equivalence partition tests
      testCases.push(...this.generatePartitionTests(rule));
      
      // Edge case tests
      testCases.push(...this.generateEdgeCaseTests(rule));
    }
    
    return testCases;
  }

  /**
   * Generate boundary value tests
   */
  generateBoundaryTests(rule: TestBusinessRule): TestCase[] {
    const tests: TestCase[] = [];
    const inputs = rule.inputs || [];
    
    for (const input of inputs) {
      const inputName = input.name;
      const inputType = input.type;
      
      if (inputType === 'decimal' || inputType === 'number' || inputType === 'integer') {
        // Zero boundary
        tests.push(this.createTestCase({
          name: `${rule.name} - ${inputName} at zero`,
          description: `Test ${rule.name} with ${inputName} = 0`,
          inputs: { [inputName]: 0 },
          sourceRule: rule.id,
          strategy: 'boundary',
          priority: 'high',
        }));
        
        // Minimum boundary
        if (input.constraints?.min !== undefined) {
          tests.push(this.createTestCase({
            name: `${rule.name} - ${inputName} at minimum`,
            description: `Test ${rule.name} with ${inputName} = ${input.constraints.min}`,
            inputs: { [inputName]: input.constraints.min },
            sourceRule: rule.id,
            strategy: 'boundary',
            priority: 'high',
          }));
          
          // Just below minimum
          tests.push(this.createTestCase({
            name: `${rule.name} - ${inputName} below minimum`,
            description: `Test ${rule.name} with ${inputName} just below minimum`,
            inputs: { [inputName]: input.constraints.min - 0.01 },
            sourceRule: rule.id,
            strategy: 'boundary',
            priority: 'high',
          }));
        }
        
        // Maximum boundary
        if (input.constraints?.max !== undefined) {
          tests.push(this.createTestCase({
            name: `${rule.name} - ${inputName} at maximum`,
            description: `Test ${rule.name} with ${inputName} = ${input.constraints.max}`,
            inputs: { [inputName]: input.constraints.max },
            sourceRule: rule.id,
            strategy: 'boundary',
            priority: 'high',
          }));
          
          // Just above maximum
          tests.push(this.createTestCase({
            name: `${rule.name} - ${inputName} above maximum`,
            description: `Test ${rule.name} with ${inputName} just above maximum`,
            inputs: { [inputName]: input.constraints.max + 0.01 },
            sourceRule: rule.id,
            strategy: 'boundary',
            priority: 'high',
          }));
        }
        
        // Negative value
        tests.push(this.createTestCase({
          name: `${rule.name} - ${inputName} negative`,
          description: `Test ${rule.name} with negative ${inputName}`,
          inputs: { [inputName]: -100 },
          sourceRule: rule.id,
          strategy: 'boundary',
          priority: 'medium',
        }));
        
        // Large value
        tests.push(this.createTestCase({
          name: `${rule.name} - ${inputName} large value`,
          description: `Test ${rule.name} with large ${inputName}`,
          inputs: { [inputName]: 999999999.99 },
          sourceRule: rule.id,
          strategy: 'boundary',
          priority: 'medium',
        }));
      }
      
      if (inputType === 'string') {
        // Empty string
        tests.push(this.createTestCase({
          name: `${rule.name} - ${inputName} empty`,
          description: `Test ${rule.name} with empty ${inputName}`,
          inputs: { [inputName]: '' },
          sourceRule: rule.id,
          strategy: 'boundary',
          priority: 'high',
        }));
        
        // Whitespace only
        tests.push(this.createTestCase({
          name: `${rule.name} - ${inputName} whitespace`,
          description: `Test ${rule.name} with whitespace ${inputName}`,
          inputs: { [inputName]: '   ' },
          sourceRule: rule.id,
          strategy: 'boundary',
          priority: 'medium',
        }));
        
        // Very long string
        tests.push(this.createTestCase({
          name: `${rule.name} - ${inputName} long string`,
          description: `Test ${rule.name} with very long ${inputName}`,
          inputs: { [inputName]: 'A'.repeat(10000) },
          sourceRule: rule.id,
          strategy: 'boundary',
          priority: 'low',
        }));
      }
    }
    
    return tests;
  }

  /**
   * Generate equivalence partition tests
   */
  generatePartitionTests(rule: TestBusinessRule): TestCase[] {
    const tests: TestCase[] = [];
    const inputs = rule.inputs || [];
    
    for (const input of inputs) {
      const inputName = input.name;
      const inputType = input.type;
      
      if (inputType === 'decimal' || inputType === 'number' || inputType === 'integer') {
        // Representative values from different partitions
        const partitions = [
          { value: 1, desc: 'small positive' },
          { value: 100, desc: 'medium positive' },
          { value: 10000, desc: 'large positive' },
          { value: 0.01, desc: 'small decimal' },
          { value: 123.45, desc: 'typical decimal' },
        ];
        
        for (const partition of partitions) {
          tests.push(this.createTestCase({
            name: `${rule.name} - ${inputName} ${partition.desc}`,
            description: `Test ${rule.name} with ${partition.desc} ${inputName}`,
            inputs: { [inputName]: partition.value },
            sourceRule: rule.id,
            strategy: 'equivalence-partition',
            priority: 'medium',
          }));
        }
      }
      
      if (input.constraints?.enum) {
        // Test each enum value
        for (const enumValue of input.constraints.enum) {
          tests.push(this.createTestCase({
            name: `${rule.name} - ${inputName} = ${enumValue}`,
            description: `Test ${rule.name} with ${inputName} = ${enumValue}`,
            inputs: { [inputName]: enumValue },
            sourceRule: rule.id,
            strategy: 'equivalence-partition',
            priority: 'high',
          }));
        }
      }
    }
    
    return tests;
  }

  /**
   * Generate edge case tests
   */
  generateEdgeCaseTests(rule: TestBusinessRule): TestCase[] {
    const tests: TestCase[] = [];
    
    // Extract edge cases from rule metadata
    const edgeCases = rule.edgeCases || [];
    
    for (let i = 0; i < edgeCases.length; i++) {
      const edgeCase = edgeCases[i] ?? `Edge case ${i + 1}`;
      tests.push(this.createTestCase({
        name: `${rule.name} - Edge case ${i + 1}`,
        description: edgeCase,
        inputs: {}, // Would need SME input or AI generation
        sourceRule: rule.id,
        strategy: 'manual',
        priority: 'high',
        tags: ['edge-case', 'requires-input'],
      }));
    }
    
    // Common edge cases
    tests.push(this.createTestCase({
      name: `${rule.name} - All zeros`,
      description: `Test ${rule.name} with all numeric inputs as zero`,
      inputs: this.createZeroInputs(rule),
      sourceRule: rule.id,
      strategy: 'boundary',
      priority: 'high',
    }));
    
    return tests;
  }

  /**
   * Generate random test cases for fuzzing
   */
  generateRandomTests(rule: TestBusinessRule, count: number = 100): TestCase[] {
    const tests: TestCase[] = [];
    
    for (let i = 0; i < count; i++) {
      tests.push(this.createTestCase({
        name: `${rule.name} - Random test ${i + 1}`,
        description: `Randomly generated test case for ${rule.name}`,
        inputs: this.generateRandomInputs(rule),
        sourceRule: rule.id,
        strategy: 'random',
        priority: 'low',
        tags: ['random', 'fuzzing'],
      }));
    }
    
    return tests;
  }

  private createTestCase(params: {
    name: string;
    description: string;
    inputs: Record<string, unknown>;
    sourceRule?: string;
    strategy: TestGenerationStrategy;
    priority: TestCase['priority'];
    tags?: string[];
  }): TestCase {
    return {
      id: `tc_${randomUUID()}`,
      name: params.name,
      description: params.description,
      inputs: params.inputs,
      tolerance: { numeric: 0.01, string: 'exact' },
      tags: params.tags || [],
      sourceRule: params.sourceRule,
      generationStrategy: params.strategy,
      priority: params.priority,
    };
  }

  private createZeroInputs(rule: TestBusinessRule): Record<string, unknown> {
    const inputs: Record<string, unknown> = {};
    for (const input of rule.inputs || []) {
      if (['decimal', 'number', 'integer'].includes(input.type)) {
        inputs[input.name] = 0;
      } else if (input.type === 'string') {
        inputs[input.name] = '';
      }
    }
    return inputs;
  }

  private generateRandomInputs(rule: TestBusinessRule): Record<string, unknown> {
    const inputs: Record<string, unknown> = {};
    for (const input of rule.inputs || []) {
      if (['decimal', 'number'].includes(input.type)) {
        const min = input.constraints?.min ?? 0;
        const max = input.constraints?.max ?? 1000000;
        inputs[input.name] = Math.random() * (max - min) + min;
      } else if (input.type === 'integer') {
        const min = input.constraints?.min ?? 0;
        const max = input.constraints?.max ?? 1000000;
        inputs[input.name] = Math.floor(Math.random() * (max - min) + min);
      } else if (input.type === 'string') {
        if (input.constraints?.enum) {
          const enumValues = input.constraints.enum;
          inputs[input.name] = enumValues[Math.floor(Math.random() * enumValues.length)];
        } else {
          inputs[input.name] = `test_${Math.random().toString(36).substring(7)}`;
        }
      } else if (input.type === 'boolean') {
        inputs[input.name] = Math.random() > 0.5;
      }
    }
    return inputs;
  }
}
