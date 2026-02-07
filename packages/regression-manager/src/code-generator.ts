/**
 * Test Code Generator
 * Generates executable test code from test cases
 */

import type {
  TestCase,
  TestFramework,
  GeneratedTestCode,
  GeneratedFile,
} from './types.js';

export class CodeGenerator {
  private framework: TestFramework;
  private language: string;

  constructor(language: string = 'typescript', framework: TestFramework = 'vitest') {
    this.language = language;
    this.framework = framework;
  }

  /**
   * Generate test code for a set of test cases
   */
  generateCode(testCases: TestCase[]): GeneratedFile[] {
    // Group test cases by related rule
    const byRule = new Map<string, TestCase[]>();

    for (const tc of testCases) {
      const ruleId = tc.relatedRuleIds[0] || 'general';
      if (!byRule.has(ruleId)) {
        byRule.set(ruleId, []);
      }
      byRule.get(ruleId)!.push(tc);
    }

    // Generate a file for each rule
    const files: GeneratedFile[] = [];

    for (const [ruleId, cases] of byRule) {
      const code = this.generateTestFile(ruleId, cases);
      files.push({
        path: `${this.slugify(ruleId)}.test.${this.getFileExtension()}`,
        content: code,
        testCaseIds: cases.map(tc => tc.id),
      });
    }

    return files;
  }

  /**
   * Generate code for a single test case
   */
  generateTestCase(testCase: TestCase): GeneratedTestCode {
    const code = this.generateSingleTest(testCase);

    return {
      language: this.language,
      framework: this.framework,
      code,
      dependencies: this.getDependencies(),
    };
  }

  // ============================================================================
  // PRIVATE METHODS
  // ============================================================================

  private generateTestFile(ruleId: string, testCases: TestCase[]): string {
    switch (this.framework) {
      case 'vitest':
      case 'jest':
        return this.generateJestVitestFile(ruleId, testCases);
      case 'pytest':
        return this.generatePytestFile(ruleId, testCases);
      case 'junit':
        return this.generateJunitFile(ruleId, testCases);
      default:
        return this.generateJestVitestFile(ruleId, testCases);
    }
  }

  private generateJestVitestFile(ruleId: string, testCases: TestCase[]): string {
    const isVitest = this.framework === 'vitest';
    const importStatement = isVitest
      ? "import { describe, it, expect } from 'vitest';"
      : "";

    const lines: string[] = [
      '/**',
      ` * Generated test file for rule: ${ruleId}`,
      ` * Generated at: ${new Date().toISOString()}`,
      ` * Framework: ${this.framework}`,
      ' */',
      '',
      importStatement,
      '',
      `// Import the module under test`,
      `// import { calculateResult } from './module-under-test';`,
      '',
      `describe('${this.formatRuleName(ruleId)}', () => {`,
    ];

    // Group by test type
    const byType = new Map<string, TestCase[]>();
    for (const tc of testCases) {
      if (!byType.has(tc.type)) {
        byType.set(tc.type, []);
      }
      byType.get(tc.type)!.push(tc);
    }

    for (const [type, cases] of byType) {
      lines.push('');
      lines.push(`  describe('${type} tests', () => {`);

      for (const tc of cases) {
        lines.push('');
        lines.push(...this.generateJestVitestTest(tc).map(l => '    ' + l));
      }

      lines.push('  });');
    }

    lines.push('});');
    lines.push('');

    return lines.join('\n');
  }

  private generateJestVitestTest(testCase: TestCase): string[] {
    const lines: string[] = [];

    // Add documentation
    lines.push(`/**`);
    lines.push(` * ${testCase.description}`);
    if (testCase.preconditions.length > 0) {
      lines.push(` * Preconditions: ${testCase.preconditions.join(', ')}`);
    }
    lines.push(` * Priority: ${testCase.priority}`);
    lines.push(` */`);

    // Generate test
    lines.push(`it('${testCase.name}', () => {`);

    // Setup inputs
    lines.push('  // Arrange');
    lines.push('  const inputs = {');
    for (const input of testCase.inputs) {
      lines.push(`    ${input.name}: ${this.formatValue(input.value)},`);
    }
    lines.push('  };');
    lines.push('');

    // Expected outputs
    lines.push('  const expected = {');
    for (const output of testCase.expectedOutputs) {
      lines.push(`    ${output.name}: ${this.formatValue(output.value)},`);
    }
    lines.push('  };');
    lines.push('');

    // Act
    lines.push('  // Act');
    lines.push('  // TODO: Replace with actual function call');
    lines.push('  const result = { /* call your function here */ };');
    lines.push('');

    // Assert
    lines.push('  // Assert');
    for (const output of testCase.expectedOutputs) {
      if (output.tolerance !== undefined) {
        lines.push(`  expect(result.${output.name}).toBeCloseTo(expected.${output.name}, 2);`);
      } else if (typeof output.value === 'object') {
        lines.push(`  expect(result.${output.name}).toEqual(expected.${output.name});`);
      } else {
        lines.push(`  expect(result.${output.name}).toBe(expected.${output.name});`);
      }
    }

    lines.push('});');

    return lines;
  }

  private generatePytestFile(ruleId: string, testCases: TestCase[]): string {
    const lines: string[] = [
      '"""',
      `Generated test file for rule: ${ruleId}`,
      `Generated at: ${new Date().toISOString()}`,
      `Framework: pytest`,
      '"""',
      '',
      'import pytest',
      '# from module_under_test import calculate_result',
      '',
    ];

    for (const tc of testCases) {
      lines.push(...this.generatePytestTest(tc));
      lines.push('');
    }

    return lines.join('\n');
  }

  private generatePytestTest(testCase: TestCase): string[] {
    const lines: string[] = [];
    const functionName = this.slugify(testCase.name);

    lines.push(`def test_${functionName}():`);
    lines.push(`    """${testCase.description}"""`);
    lines.push('');
    lines.push('    # Arrange');

    for (const input of testCase.inputs) {
      lines.push(`    ${this.toSnakeCase(input.name)} = ${this.formatPythonValue(input.value)}`);
    }
    lines.push('');

    lines.push('    # Expected');
    for (const output of testCase.expectedOutputs) {
      lines.push(`    expected_${this.toSnakeCase(output.name)} = ${this.formatPythonValue(output.value)}`);
    }
    lines.push('');

    lines.push('    # Act');
    lines.push('    # TODO: Replace with actual function call');
    lines.push('    # result = calculate_result(...)');
    lines.push('');

    lines.push('    # Assert');
    for (const output of testCase.expectedOutputs) {
      const outputVar = this.toSnakeCase(output.name);
      if (output.tolerance !== undefined) {
        lines.push(`    # assert result.${outputVar} == pytest.approx(expected_${outputVar}, rel=${output.tolerance})`);
      } else {
        lines.push(`    # assert result.${outputVar} == expected_${outputVar}`);
      }
    }

    return lines;
  }

  private generateJunitFile(ruleId: string, testCases: TestCase[]): string {
    const className = this.toPascalCase(ruleId) + 'Test';

    const lines: string[] = [
      '/**',
      ` * Generated test file for rule: ${ruleId}`,
      ` * Generated at: ${new Date().toISOString()}`,
      ` * Framework: JUnit`,
      ' */',
      '',
      'import org.junit.jupiter.api.Test;',
      'import org.junit.jupiter.api.DisplayName;',
      'import static org.junit.jupiter.api.Assertions.*;',
      '',
      `public class ${className} {`,
    ];

    for (const tc of testCases) {
      lines.push('');
      lines.push(...this.generateJunitTest(tc).map(l => '    ' + l));
    }

    lines.push('}');
    lines.push('');

    return lines.join('\n');
  }

  private generateJunitTest(testCase: TestCase): string[] {
    const lines: string[] = [];
    const methodName = 'test' + this.toPascalCase(testCase.name);

    lines.push(`@Test`);
    lines.push(`@DisplayName("${testCase.name}")`);
    lines.push(`void ${methodName}() {`);
    lines.push(`    // ${testCase.description}`);
    lines.push('');
    lines.push('    // Arrange');

    for (const input of testCase.inputs) {
      const javaType = this.getJavaType(input.type);
      lines.push(`    ${javaType} ${input.name} = ${this.formatJavaValue(input.value, input.type)};`);
    }
    lines.push('');

    lines.push('    // Act');
    lines.push('    // TODO: Replace with actual method call');
    lines.push('    // var result = calculator.calculate(...);');
    lines.push('');

    lines.push('    // Assert');
    for (const output of testCase.expectedOutputs) {
      const expected = this.formatJavaValue(output.value, output.type);
      if (output.tolerance !== undefined) {
        lines.push(`    // assertEquals(${expected}, result.get${this.toPascalCase(output.name)}(), ${output.tolerance});`);
      } else {
        lines.push(`    // assertEquals(${expected}, result.get${this.toPascalCase(output.name)}());`);
      }
    }

    lines.push('}');

    return lines;
  }

  private generateSingleTest(testCase: TestCase): string {
    switch (this.framework) {
      case 'vitest':
      case 'jest':
        return this.generateJestVitestTest(testCase).join('\n');
      case 'pytest':
        return this.generatePytestTest(testCase).join('\n');
      case 'junit':
        return this.generateJunitTest(testCase).join('\n');
      default:
        return this.generateJestVitestTest(testCase).join('\n');
    }
  }

  private getDependencies(): string[] {
    switch (this.framework) {
      case 'vitest':
        return ['vitest'];
      case 'jest':
        return ['jest', '@types/jest'];
      case 'pytest':
        return ['pytest'];
      case 'junit':
        return ['org.junit.jupiter:junit-jupiter'];
      default:
        return [];
    }
  }

  private getFileExtension(): string {
    switch (this.language) {
      case 'typescript':
        return 'ts';
      case 'javascript':
        return 'js';
      case 'python':
        return 'py';
      case 'java':
        return 'java';
      default:
        return 'ts';
    }
  }

  private formatValue(value: unknown): string {
    if (value === null) return 'null';
    if (value === undefined) return 'undefined';
    if (typeof value === 'string') return `'${value}'`;
    if (typeof value === 'number') return value.toString();
    if (typeof value === 'boolean') return value.toString();
    if (Array.isArray(value)) return JSON.stringify(value);
    if (typeof value === 'object') return JSON.stringify(value);
    return String(value);
  }

  private formatPythonValue(value: unknown): string {
    if (value === null) return 'None';
    if (value === undefined) return 'None';
    if (typeof value === 'string') return `"${value}"`;
    if (typeof value === 'number') return value.toString();
    if (typeof value === 'boolean') return value ? 'True' : 'False';
    if (Array.isArray(value)) return JSON.stringify(value).replace(/null/g, 'None');
    return String(value);
  }

  private formatJavaValue(value: unknown, type: string): string {
    if (value === null) return 'null';
    if (type === 'decimal' || type === 'number') return `${value}d`;
    if (type === 'integer') return value?.toString() || '0';
    if (typeof value === 'string') return `"${value}"`;
    if (typeof value === 'boolean') return value.toString();
    return String(value);
  }

  private getJavaType(type: string): string {
    switch (type) {
      case 'decimal':
      case 'number':
        return 'double';
      case 'integer':
        return 'int';
      case 'string':
        return 'String';
      case 'boolean':
        return 'boolean';
      default:
        return 'Object';
    }
  }

  private formatRuleName(ruleId: string): string {
    return ruleId
      .replace(/_/g, ' ')
      .replace(/^br\s*/i, '')
      .trim();
  }

  private slugify(text: string): string {
    return text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '_')
      .replace(/^_|_$/g, '');
  }

  private toSnakeCase(text: string): string {
    return text
      .replace(/([A-Z])/g, '_$1')
      .toLowerCase()
      .replace(/^_/, '');
  }

  private toPascalCase(text: string): string {
    return text
      .split(/[_\s-]+/)
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }
}
