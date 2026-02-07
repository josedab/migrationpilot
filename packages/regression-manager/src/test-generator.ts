/**
 * Test Generator
 * Generates test cases from business rules
 */

import type { BusinessRule } from '@migrationpilot/core';
import type {
  TestCase,
  TestInput,
  ExpectedOutput,
  GenerationConfig,
  BoundaryCondition,
  EquivalenceClass,
} from './types.js';
import { DEFAULT_GENERATION_CONFIG } from './types.js';

export class TestGenerator {
  private config: GenerationConfig;

  constructor(config: Partial<GenerationConfig> = {}) {
    this.config = { ...DEFAULT_GENERATION_CONFIG, ...config };
  }

  /**
   * Generate test cases for a set of business rules
   */
  generateTestCases(rules: BusinessRule[]): TestCase[] {
    const testCases: TestCase[] = [];

    for (const rule of rules) {
      // Generate happy path tests
      const happyPathTests = this.generateHappyPathTests(rule);
      testCases.push(...happyPathTests);

      // Generate boundary tests
      if (this.config.generateBoundaryTests) {
        const boundaryTests = this.generateBoundaryTests(rule);
        testCases.push(...boundaryTests);
      }

      // Generate equivalence class tests
      if (this.config.generateEquivalenceTests) {
        const equivalenceTests = this.generateEquivalenceTests(rule);
        testCases.push(...equivalenceTests);
      }

      // Generate negative tests
      if (this.config.generateNegativeTests) {
        const negativeTests = this.generateNegativeTests(rule);
        testCases.push(...negativeTests);
      }

      // Generate edge case tests
      if (this.config.includeEdgeCases && rule.edgeCases) {
        const edgeCaseTests = this.generateEdgeCaseTests(rule);
        testCases.push(...edgeCaseTests);
      }

      // Limit tests per rule
      if (testCases.length > this.config.maxTestsPerRule * rules.length) {
        break;
      }
    }

    return testCases;
  }

  /**
   * Generate happy path test cases
   */
  private generateHappyPathTests(rule: BusinessRule): TestCase[] {
    const tests: TestCase[] = [];

    // Generate standard happy path test
    const inputs = this.generateStandardInputs(rule);
    const outputs = this.calculateExpectedOutputs(rule, inputs);

    tests.push({
      id: `tc_${rule.id}_happy_${Date.now()}`,
      name: this.formatTestName('calculate correctly', 'valid inputs', rule),
      description: `Verify ${rule.name} produces correct output with standard inputs`,
      type: 'unit',
      priority: 'high',
      relatedRuleIds: [rule.id],
      inputs,
      expectedOutputs: outputs,
      preconditions: rule.assumptions || [],
      postconditions: [],
      tags: ['happy-path', rule.category],
      status: 'ready',
    });

    return tests;
  }

  /**
   * Generate boundary condition tests
   */
  private generateBoundaryTests(rule: BusinessRule): TestCase[] {
    const tests: TestCase[] = [];
    const boundaries = this.extractBoundaryConditions(rule);

    for (const boundary of boundaries) {
      // Test at boundary
      const atBoundaryInputs = this.generateBoundaryInputs(rule, boundary, 'at');
      const atBoundaryOutputs = this.calculateExpectedOutputs(rule, atBoundaryInputs);

      tests.push({
        id: `tc_${rule.id}_boundary_at_${boundary.variable}_${Date.now()}`,
        name: this.formatTestName('handle boundary value', `${boundary.variable} = ${boundary.boundary}`, rule),
        description: `Verify ${rule.name} handles boundary value for ${boundary.variable}`,
        type: 'boundary',
        priority: 'high',
        relatedRuleIds: [rule.id],
        inputs: atBoundaryInputs,
        expectedOutputs: atBoundaryOutputs,
        preconditions: [],
        postconditions: [],
        tags: ['boundary', boundary.variable, rule.category],
        status: 'ready',
      });

      // Test just below boundary
      const belowBoundaryInputs = this.generateBoundaryInputs(rule, boundary, 'below');
      const belowBoundaryOutputs = this.calculateExpectedOutputs(rule, belowBoundaryInputs);

      tests.push({
        id: `tc_${rule.id}_boundary_below_${boundary.variable}_${Date.now()}`,
        name: this.formatTestName('handle value below boundary', `${boundary.variable} < ${boundary.boundary}`, rule),
        description: `Verify ${rule.name} handles value just below boundary for ${boundary.variable}`,
        type: 'boundary',
        priority: 'medium',
        relatedRuleIds: [rule.id],
        inputs: belowBoundaryInputs,
        expectedOutputs: belowBoundaryOutputs,
        preconditions: [],
        postconditions: [],
        tags: ['boundary', boundary.variable, rule.category],
        status: 'ready',
      });

      // Test just above boundary
      const aboveBoundaryInputs = this.generateBoundaryInputs(rule, boundary, 'above');
      const aboveBoundaryOutputs = this.calculateExpectedOutputs(rule, aboveBoundaryInputs);

      tests.push({
        id: `tc_${rule.id}_boundary_above_${boundary.variable}_${Date.now()}`,
        name: this.formatTestName('handle value above boundary', `${boundary.variable} > ${boundary.boundary}`, rule),
        description: `Verify ${rule.name} handles value just above boundary for ${boundary.variable}`,
        type: 'boundary',
        priority: 'medium',
        relatedRuleIds: [rule.id],
        inputs: aboveBoundaryInputs,
        expectedOutputs: aboveBoundaryOutputs,
        preconditions: [],
        postconditions: [],
        tags: ['boundary', boundary.variable, rule.category],
        status: 'ready',
      });
    }

    return tests;
  }

  /**
   * Generate equivalence class tests
   */
  private generateEquivalenceTests(rule: BusinessRule): TestCase[] {
    const tests: TestCase[] = [];
    const classes = this.extractEquivalenceClasses(rule);

    for (const eqClass of classes) {
      // Pick representative value from each class
      const representativeValue = eqClass.values[0];
      const inputs = this.generateInputsWithValue(rule, eqClass.variable, representativeValue);
      const outputs = this.calculateExpectedOutputs(rule, inputs);

      tests.push({
        id: `tc_${rule.id}_eq_${eqClass.className}_${Date.now()}`,
        name: this.formatTestName('process correctly', `${eqClass.variable} in ${eqClass.className}`, rule),
        description: `Verify ${rule.name} handles ${eqClass.variable} in equivalence class "${eqClass.className}"`,
        type: 'equivalence',
        priority: 'medium',
        relatedRuleIds: [rule.id],
        inputs,
        expectedOutputs: outputs,
        preconditions: [],
        postconditions: [],
        tags: ['equivalence', eqClass.className, rule.category],
        status: 'ready',
      });
    }

    return tests;
  }

  /**
   * Generate negative tests
   */
  private generateNegativeTests(rule: BusinessRule): TestCase[] {
    const tests: TestCase[] = [];

    // Generate tests with invalid inputs
    for (const input of rule.inputs) {
      const invalidInputs = this.generateInvalidInputs(rule, input.name);

      tests.push({
        id: `tc_${rule.id}_negative_${input.name}_${Date.now()}`,
        name: this.formatTestName('reject invalid input', `invalid ${input.name}`, rule),
        description: `Verify ${rule.name} handles invalid ${input.name} correctly`,
        type: 'unit',
        priority: 'medium',
        relatedRuleIds: [rule.id],
        inputs: invalidInputs,
        expectedOutputs: [{
          name: 'error',
          type: 'string',
          value: 'ValidationError',
          description: 'Expected validation error',
        }],
        preconditions: [],
        postconditions: [],
        tags: ['negative', input.name, rule.category],
        status: 'ready',
      });
    }

    return tests;
  }

  /**
   * Generate edge case tests
   */
  private generateEdgeCaseTests(rule: BusinessRule): TestCase[] {
    const tests: TestCase[] = [];

    for (const edgeCase of rule.edgeCases || []) {
      const inputs = this.generateEdgeCaseInputs(rule, edgeCase);
      const outputs = this.calculateExpectedOutputs(rule, inputs);

      tests.push({
        id: `tc_${rule.id}_edge_${this.slugify(edgeCase)}_${Date.now()}`,
        name: this.formatTestName('handle edge case', edgeCase, rule),
        description: `Verify ${rule.name} handles edge case: ${edgeCase}`,
        type: 'business-rule',
        priority: 'high',
        relatedRuleIds: [rule.id],
        inputs,
        expectedOutputs: outputs,
        preconditions: [edgeCase],
        postconditions: [],
        tags: ['edge-case', rule.category],
        status: 'ready',
      });
    }

    return tests;
  }

  // ============================================================================
  // HELPER METHODS
  // ============================================================================

  private generateStandardInputs(rule: BusinessRule): TestInput[] {
    return rule.inputs.map(input => ({
      name: input.name,
      type: input.type,
      value: this.generateStandardValue(input.type, input.name),
      description: input.source,
    }));
  }

  private generateStandardValue(type: string, name: string): unknown {
    const nameLower = name.toLowerCase();

    switch (type) {
      case 'decimal':
      case 'number':
        if (nameLower.includes('rate')) return 0.05;
        if (nameLower.includes('amount') || nameLower.includes('principal')) return 10000;
        if (nameLower.includes('ratio')) return 0.35;
        return 100;
      case 'integer':
        if (nameLower.includes('score')) return 720;
        if (nameLower.includes('year')) return 5;
        if (nameLower.includes('day')) return 30;
        if (nameLower.includes('count')) return 10;
        return 10;
      case 'string':
        return 'test_value';
      case 'boolean':
        return true;
      case 'date':
        return '2024-01-15';
      default:
        return null;
    }
  }

  private calculateExpectedOutputs(rule: BusinessRule, inputs: TestInput[]): ExpectedOutput[] {
    // Create input map for lookup
    const inputMap = new Map(inputs.map(i => [i.name.toLowerCase(), i.value]));

    return rule.outputs.map(output => {
      let value: unknown = null;

      // Try to calculate based on formula if available
      if (rule.formula) {
        value = this.evaluateFormula(rule.formula, inputMap);
      }

      // If no formula or calculation failed, use placeholder
      if (value === null) {
        value = this.generateExpectedValue(output.type);
      }

      return {
        name: output.name,
        type: output.type,
        value,
        tolerance: output.type === 'decimal' ? 0.01 : undefined,
        description: output.description,
      };
    });
  }

  private evaluateFormula(formula: string, inputs: Map<string, unknown>): unknown {
    // Simple formula evaluation for common patterns
    // Interest = Principal × Rate × Years (I = P × R × T)
    if (formula.toLowerCase().includes('p') && formula.toLowerCase().includes('r') && formula.toLowerCase().includes('t')) {
      const p = inputs.get('principal') as number || inputs.get('p') as number || 0;
      const r = inputs.get('rate') as number || inputs.get('r') as number || 0;
      const t = inputs.get('years') as number || inputs.get('t') as number || inputs.get('term') as number || 0;
      if (p && r && t) return p * r * t;
    }

    // Risk score calculation
    if (formula.toLowerCase().includes('credit') && formula.toLowerCase().includes('dti')) {
      const credit = inputs.get('creditscore') as number || 720;
      const dti = inputs.get('dtiratio') as number || 0.35;
      return (1000 - credit) / 10 + dti * 5;
    }

    return null;
  }

  private generateExpectedValue(type: string): unknown {
    switch (type) {
      case 'decimal':
      case 'number':
        return 0;
      case 'integer':
        return 0;
      case 'string':
        return '';
      case 'boolean':
        return true;
      case 'array':
        return [];
      default:
        return null;
    }
  }

  private extractBoundaryConditions(rule: BusinessRule): BoundaryCondition[] {
    const boundaries: BoundaryCondition[] = [];

    // Extract from rule logic/formula
    if (rule.logic) {
      const comparisonMatch = rule.logic.match(/(\w+)\s*(>=?|<=?|==|!=)\s*(\d+\.?\d*)/g);
      if (comparisonMatch) {
        for (const match of comparisonMatch) {
          const parts = match.match(/(\w+)\s*(>=?|<=?|==|!=)\s*(\d+\.?\d*)/);
          if (parts) {
            boundaries.push({
              ruleId: rule.id,
              variable: parts[1] || '',
              boundary: parts[3] || '',
              coveredByTestIds: [],
            });
          }
        }
      }
    }

    // Add standard boundaries for numeric inputs
    for (const input of rule.inputs) {
      const numericTypes = ['decimal', 'integer'];
      if (numericTypes.includes(input.type)) {
        boundaries.push({
          ruleId: rule.id,
          variable: input.name,
          boundary: '0',
          coveredByTestIds: [],
        });
      }
    }

    return boundaries;
  }

  private extractEquivalenceClasses(rule: BusinessRule): EquivalenceClass[] {
    const classes: EquivalenceClass[] = [];

    for (const input of rule.inputs) {
      if (input.type === 'integer' && input.name.toLowerCase().includes('score')) {
        // Credit score equivalence classes
        classes.push(
          { ruleId: rule.id, variable: input.name, className: 'excellent', values: [800, 850], coveredByTestIds: [] },
          { ruleId: rule.id, variable: input.name, className: 'good', values: [700, 750], coveredByTestIds: [] },
          { ruleId: rule.id, variable: input.name, className: 'fair', values: [600, 650], coveredByTestIds: [] },
          { ruleId: rule.id, variable: input.name, className: 'poor', values: [500, 550], coveredByTestIds: [] }
        );
      } else if (input.type === 'decimal' && input.name.toLowerCase().includes('ratio')) {
        // Ratio equivalence classes
        classes.push(
          { ruleId: rule.id, variable: input.name, className: 'low', values: [0.1, 0.2], coveredByTestIds: [] },
          { ruleId: rule.id, variable: input.name, className: 'medium', values: [0.3, 0.4], coveredByTestIds: [] },
          { ruleId: rule.id, variable: input.name, className: 'high', values: [0.5, 0.6], coveredByTestIds: [] }
        );
      }
    }

    return classes;
  }

  private generateBoundaryInputs(
    rule: BusinessRule,
    boundary: BoundaryCondition,
    position: 'at' | 'below' | 'above'
  ): TestInput[] {
    const boundaryValue = parseFloat(boundary.boundary);
    let value: number;

    switch (position) {
      case 'at':
        value = boundaryValue;
        break;
      case 'below':
        value = boundaryValue - 1;
        break;
      case 'above':
        value = boundaryValue + 1;
        break;
    }

    return rule.inputs.map(input => ({
      name: input.name,
      type: input.type,
      value: input.name.toLowerCase() === boundary.variable.toLowerCase()
        ? value
        : this.generateStandardValue(input.type, input.name),
    }));
  }

  private generateInputsWithValue(rule: BusinessRule, variableName: string, value: unknown): TestInput[] {
    return rule.inputs.map(input => ({
      name: input.name,
      type: input.type,
      value: input.name.toLowerCase() === variableName.toLowerCase()
        ? value
        : this.generateStandardValue(input.type, input.name),
    }));
  }

  private generateInvalidInputs(rule: BusinessRule, invalidInputName: string): TestInput[] {
    return rule.inputs.map(input => ({
      name: input.name,
      type: input.type,
      value: input.name === invalidInputName
        ? this.generateInvalidValue(input.type)
        : this.generateStandardValue(input.type, input.name),
    }));
  }

  private generateInvalidValue(type: string): unknown {
    switch (type) {
      case 'decimal':
      case 'number':
      case 'integer':
        return -999999;
      case 'string':
        return '';
      case 'boolean':
        return null;
      default:
        return null;
    }
  }

  private generateEdgeCaseInputs(rule: BusinessRule, _edgeCase: string): TestInput[] {
    // Generate inputs that might trigger the edge case
    return rule.inputs.map(input => ({
      name: input.name,
      type: input.type,
      value: this.generateEdgeCaseValue(input.type, input.name),
    }));
  }

  private generateEdgeCaseValue(type: string, name: string): unknown {
    const nameLower = name.toLowerCase();

    switch (type) {
      case 'decimal':
      case 'number':
        if (nameLower.includes('rate')) return 0;
        if (nameLower.includes('amount') || nameLower.includes('principal')) return 0;
        return 0;
      case 'integer':
        if (nameLower.includes('score')) return 300;
        if (nameLower.includes('year')) return 0;
        return 0;
      default:
        return this.generateStandardValue(type, name);
    }
  }

  private formatTestName(action: string, condition: string, _rule: BusinessRule): string {
    const pattern = this.config.testNamePattern || 'should {action} when {condition}';
    return pattern
      .replace('{action}', action)
      .replace('{condition}', condition);
  }

  private slugify(text: string): string {
    return text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '_')
      .replace(/^_|_$/g, '');
  }
}
