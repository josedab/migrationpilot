/**
 * Rule Validator - Validates rules for completeness and correctness
 */

import type {
  ParsedRule,
  ValidationResult,
  ValidationError,
  ValidationWarning,
  RuleCoverage,
  TestCase,
  DomainContext,
} from '../types.js';

// ============================================================================
// Rule Validator
// ============================================================================

export class RuleValidator {
  private context: DomainContext;

  constructor(context?: DomainContext) {
    this.context = context || { entities: [], fields: [], functions: [], synonyms: [], patterns: [] };
  }

  /**
   * Validate a parsed rule
   */
  validate(rule: ParsedRule, testCases?: TestCase[]): ValidationResult {
    const errors: ValidationError[] = [];
    const warnings: ValidationWarning[] = [];

    // Structural validation
    this.validateStructure(rule, errors, warnings);

    // Semantic validation
    this.validateSemantics(rule, errors, warnings);

    // Context validation
    this.validateAgainstContext(rule, errors, warnings);

    // Test case coverage
    const coverage = this.calculateCoverage(rule, testCases || []);

    return {
      valid: errors.length === 0,
      errors,
      warnings,
      coverage,
    };
  }

  /**
   * Run test cases against a rule
   */
  runTestCases(
    _rule: ParsedRule,
    testCases: TestCase[],
    evaluator: (inputs: Record<string, unknown>) => unknown
  ): TestCaseResult[] {
    const results: TestCaseResult[] = [];

    for (const testCase of testCases) {
      try {
        const actualResult = evaluator(testCase.inputs);
        const passed = this.compareResults(actualResult, testCase.expectedOutputs, testCase.expectedResult);

        results.push({
          testCase,
          passed,
          actualResult,
          error: undefined,
        });
      } catch (error) {
        results.push({
          testCase,
          passed: testCase.expectedResult === 'exception',
          actualResult: undefined,
          error: error instanceof Error ? error.message : 'Unknown error',
        });
      }
    }

    return results;
  }

  /**
   * Update context
   */
  setContext(context: DomainContext): void {
    this.context = context;
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private validateStructure(rule: ParsedRule, errors: ValidationError[], warnings: ValidationWarning[]): void {
    // Check subject
    if (!rule.subject || !rule.subject.entity) {
      errors.push({
        code: 'MISSING_SUBJECT',
        message: 'Rule must have a subject entity',
        suggestion: 'Specify the entity this rule applies to',
      });
    }

    // Check intent
    if (!rule.intent) {
      errors.push({
        code: 'MISSING_INTENT',
        message: 'Rule must have a defined intent',
        suggestion: 'Specify what the rule does (validation, calculation, etc.)',
      });
    }

    // Check conditions
    if (!rule.conditions || rule.conditions.length === 0) {
      warnings.push({
        code: 'NO_CONDITIONS',
        message: 'Rule has no conditions and will always apply',
      });
    }

    // Check actions
    if (!rule.actions || rule.actions.length === 0) {
      errors.push({
        code: 'NO_ACTIONS',
        message: 'Rule must have at least one action',
        suggestion: 'Specify what should happen when conditions are met',
      });
    }

    // Validate condition structure
    for (const condition of rule.conditions || []) {
      if (!condition.field) {
        errors.push({
          code: 'CONDITION_MISSING_FIELD',
          message: 'Condition is missing a field reference',
          path: 'conditions',
        });
      }
      if (!condition.operator) {
        errors.push({
          code: 'CONDITION_MISSING_OPERATOR',
          message: 'Condition is missing an operator',
          path: 'conditions',
        });
      }
    }

    // Validate action structure
    for (const action of rule.actions || []) {
      if (!action.type) {
        errors.push({
          code: 'ACTION_MISSING_TYPE',
          message: 'Action is missing a type',
          path: 'actions',
        });
      }
      if (!action.target) {
        errors.push({
          code: 'ACTION_MISSING_TARGET',
          message: 'Action is missing a target',
          path: 'actions',
        });
      }
    }
  }

  private validateSemantics(rule: ParsedRule, errors: ValidationError[], warnings: ValidationWarning[]): void {
    // Check for contradictory conditions
    const conditionFields = rule.conditions.map(c => `${c.field}:${c.operator}`);
    const duplicates = conditionFields.filter((item, index) => conditionFields.indexOf(item) !== index);
    if (duplicates.length > 0) {
      warnings.push({
        code: 'DUPLICATE_CONDITIONS',
        message: `Potentially contradictory conditions on: ${[...new Set(duplicates)].join(', ')}`,
      });
    }

    // Check for unreachable conditions
    if (rule.conditions.some(c => c.operator === 'is_null') && 
        rule.conditions.some(c => c.operator === 'is_not_empty' && c.field === rule.conditions.find(x => x.operator === 'is_null')?.field)) {
      errors.push({
        code: 'UNREACHABLE_CONDITION',
        message: 'Rule has mutually exclusive conditions (is_null and is_not_empty on same field)',
      });
    }

    // Validate value types match expected
    for (const condition of rule.conditions) {
      if (condition.operator === 'greater_than' || condition.operator === 'less_than') {
        if (condition.value.type === 'literal' && typeof condition.value.value !== 'number') {
          warnings.push({
            code: 'TYPE_MISMATCH',
            message: `Numeric comparison on field "${condition.field}" uses non-numeric value`,
            path: 'conditions',
          });
        }
      }
    }
  }

  private validateAgainstContext(rule: ParsedRule, _errors: ValidationError[], warnings: ValidationWarning[]): void {
    // Check if subject entity exists in context
    if (this.context.entities.length > 0) {
      const entityExists = this.context.entities.some(
        e => e.name === rule.subject.entity || e.aliases.includes(rule.subject.entity)
      );
      if (!entityExists && rule.subject.entity !== 'unknown') {
        warnings.push({
          code: 'UNKNOWN_ENTITY',
          message: `Entity "${rule.subject.entity}" is not defined in the domain context`,
        });
      }
    }

    // Check if fields exist in context
    for (const condition of rule.conditions) {
      if (this.context.fields.length > 0) {
        const fieldExists = this.context.fields.some(
          f => f.name === condition.field || f.aliases.includes(condition.field)
        );
        if (!fieldExists) {
          warnings.push({
            code: 'UNKNOWN_FIELD',
            message: `Field "${condition.field}" is not defined in the domain context`,
            path: 'conditions',
          });
        }
      }
    }
  }

  private calculateCoverage(rule: ParsedRule, testCases: TestCase[]): RuleCoverage {
    const totalConditions = rule.conditions.length;
    const totalActions = rule.actions.length;
    const totalEdgeCases = this.estimateEdgeCases(rule);

    let conditionsCovered = 0;
    let actionsCovered = 0;
    let edgeCasesCovered = 0;

    // Calculate conditions covered by test cases
    for (const condition of rule.conditions) {
      const isCovered = testCases.some(tc => 
        Object.keys(tc.inputs).includes(condition.field)
      );
      if (isCovered) conditionsCovered++;
    }

    // Calculate actions covered
    for (const action of rule.actions) {
      const isCovered = testCases.some(tc =>
        Object.keys(tc.expectedOutputs).includes(action.target)
      );
      if (isCovered) actionsCovered++;
    }

    // Estimate edge case coverage
    edgeCasesCovered = testCases.filter(tc => 
      tc.tags?.includes('edge-case') || tc.expectedResult === 'exception'
    ).length;

    const totalItems = totalConditions + totalActions + totalEdgeCases;
    const coveredItems = conditionsCovered + actionsCovered + edgeCasesCovered;

    return {
      conditionsCovered,
      totalConditions,
      actionsCovered,
      totalActions,
      edgeCasesCovered,
      totalEdgeCases,
      coveragePercentage: totalItems > 0 ? (coveredItems / totalItems) * 100 : 0,
    };
  }

  private estimateEdgeCases(rule: ParsedRule): number {
    let edgeCases = 0;

    for (const condition of rule.conditions) {
      // Numeric comparisons have boundary cases
      if (['greater_than', 'less_than', 'greater_or_equal', 'less_or_equal'].includes(condition.operator)) {
        edgeCases += 2; // Boundary and just past boundary
      }
      // String operations have empty/null cases
      if (['contains', 'starts_with', 'ends_with'].includes(condition.operator)) {
        edgeCases += 2; // Empty string and null
      }
    }

    return Math.max(edgeCases, 1);
  }

  private compareResults(
    actual: unknown,
    _expected: Record<string, unknown>,
    expectedResult: 'pass' | 'fail' | 'exception'
  ): boolean {
    if (expectedResult === 'pass') {
      return actual === true || (typeof actual === 'object' && actual !== null);
    }
    if (expectedResult === 'fail') {
      return actual === false || actual === null || actual === undefined;
    }
    return false;
  }
}

// ============================================================================
// Types
// ============================================================================

export interface TestCaseResult {
  testCase: TestCase;
  passed: boolean;
  actualResult: unknown;
  error?: string;
}

// ============================================================================
// Factory
// ============================================================================

export function createRuleValidator(context?: DomainContext): RuleValidator {
  return new RuleValidator(context);
}
