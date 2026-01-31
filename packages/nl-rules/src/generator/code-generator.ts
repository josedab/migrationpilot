/**
 * Code Generator - Generates code from parsed rules
 */

import type {
  ParsedRule,
  FormalRule,
  GeneratorConfig,
  GeneratedCode,
  GeneratorTargetLanguage,
  TestCase,
  RuleCondition,
} from '../types.js';

// ============================================================================
// Code Generator
// ============================================================================

export class RuleCodeGenerator {
  private config: GeneratorConfig;

  constructor(config?: Partial<GeneratorConfig>) {
    this.config = {
      targetLanguage: config?.targetLanguage || 'typescript',
      style: config?.style || {
        indentation: 'spaces',
        indentSize: 2,
        maxLineLength: 100,
        namingConvention: 'camelCase',
      },
      includeComments: config?.includeComments ?? true,
      includeTestCases: config?.includeTestCases ?? true,
      optimization: config?.optimization || 'basic',
    };
  }

  /**
   * Generate code from a parsed rule
   */
  generate(rule: ParsedRule, ruleId?: string): GeneratedCode {
    const id = ruleId || `rule_${Date.now()}`;

    switch (this.config.targetLanguage) {
      case 'typescript':
        return this.generateTypeScript(rule, id);
      case 'javascript':
        return this.generateJavaScript(rule, id);
      case 'python':
        return this.generatePython(rule, id);
      case 'java':
        return this.generateJava(rule, id);
      case 'sql':
        return this.generateSQL(rule, id);
      default:
        return this.generateTypeScript(rule, id);
    }
  }

  /**
   * Generate formal rule representation
   */
  generateFormalRule(rule: ParsedRule, ruleId?: string): FormalRule {
    const id = ruleId || `rule_${Date.now()}`;

    return {
      id,
      type: 'expression',
      expression: this.generateExpression(rule),
      pseudocode: this.generatePseudocode(rule),
      testCases: this.generateTestCases(rule),
    };
  }

  /**
   * Update generator configuration
   */
  setConfig(config: Partial<GeneratorConfig>): void {
    Object.assign(this.config, config);
  }

  // ============================================================================
  // Language-specific generators
  // ============================================================================

  private generateTypeScript(rule: ParsedRule, id: string): GeneratedCode {
    const indent = this.getIndent();
    const funcName = this.formatName(id, 'camelCase');
    const interfaceName = this.formatName(`${id}Input`, 'PascalCase');

    let code = '';
    const imports: string[] = [];
    const warnings: string[] = [];

    // Generate interface
    code += `interface ${interfaceName} {\n`;
    const fields = this.extractFields(rule);
    for (const field of fields) {
      code += `${indent}${field}: unknown;\n`;
    }
    code += '}\n\n';

    // Generate function
    if (this.config.includeComments) {
      code += `/**\n * ${this.generateDescription(rule)}\n */\n`;
    }

    code += `export function ${funcName}(input: ${interfaceName}): boolean {\n`;

    // Generate conditions
    const conditionCode = this.generateConditionCode(rule.conditions, 'typescript');
    code += `${indent}return ${conditionCode};\n`;
    code += '}\n';

    // Generate test code if enabled
    let testCode: string | undefined;
    if (this.config.includeTestCases) {
      testCode = this.generateTypeScriptTests(rule, funcName, interfaceName);
    }

    return {
      code,
      language: 'typescript',
      testCode,
      imports,
      dependencies: [],
      warnings,
    };
  }

  private generateJavaScript(rule: ParsedRule, id: string): GeneratedCode {
    const indent = this.getIndent();
    const funcName = this.formatName(id, 'camelCase');

    let code = '';

    if (this.config.includeComments) {
      code += `/**\n * ${this.generateDescription(rule)}\n */\n`;
    }

    code += `function ${funcName}(input) {\n`;
    const conditionCode = this.generateConditionCode(rule.conditions, 'javascript');
    code += `${indent}return ${conditionCode};\n`;
    code += '}\n\n';
    code += `module.exports = { ${funcName} };\n`;

    return {
      code,
      language: 'javascript',
      imports: [],
      dependencies: [],
      warnings: [],
    };
  }

  private generatePython(rule: ParsedRule, id: string): GeneratedCode {
    const indent = this.getIndent();
    const funcName = this.formatName(id, 'snake_case');

    let code = '';

    if (this.config.includeComments) {
      code += `def ${funcName}(input_data: dict) -> bool:\n`;
      code += `${indent}"""${this.generateDescription(rule)}"""\n`;
    } else {
      code += `def ${funcName}(input_data: dict) -> bool:\n`;
    }

    const conditionCode = this.generateConditionCode(rule.conditions, 'python');
    code += `${indent}return ${conditionCode}\n`;

    return {
      code,
      language: 'python',
      imports: ['from typing import Dict'],
      dependencies: [],
      warnings: [],
    };
  }

  private generateJava(rule: ParsedRule, id: string): GeneratedCode {
    const indent = this.getIndent();
    const className = this.formatName(id, 'PascalCase');
    const methodName = this.formatName(`evaluate${id}`, 'camelCase');

    let code = '';

    code += `public class ${className} {\n`;
    
    if (this.config.includeComments) {
      code += `${indent}/**\n${indent} * ${this.generateDescription(rule)}\n${indent} */\n`;
    }

    code += `${indent}public static boolean ${methodName}(Map<String, Object> input) {\n`;
    const conditionCode = this.generateConditionCode(rule.conditions, 'java');
    code += `${indent}${indent}return ${conditionCode};\n`;
    code += `${indent}}\n`;
    code += '}\n';

    return {
      code,
      language: 'java',
      imports: ['import java.util.Map;'],
      dependencies: [],
      warnings: [],
    };
  }

  private generateSQL(rule: ParsedRule, id: string): GeneratedCode {
    const whereClause = this.generateSQLConditions(rule.conditions);

    const code = `-- ${this.generateDescription(rule)}\n-- Rule ID: ${id}\nSELECT *\nFROM ${rule.subject.entity || 'table'}\nWHERE ${whereClause};\n`;

    return {
      code,
      language: 'sql',
      imports: [],
      dependencies: [],
      warnings: [],
    };
  }

  // ============================================================================
  // Helper methods
  // ============================================================================

  private generateConditionCode(conditions: RuleCondition[], lang: GeneratorTargetLanguage): string {
    if (conditions.length === 0) return lang === 'python' ? 'True' : 'true';

    const parts = conditions.map(c => this.conditionToCode(c, lang));
    return parts.join(' && ');
  }

  private conditionToCode(condition: RuleCondition, lang: GeneratorTargetLanguage): string {
    const field = this.getFieldAccessor(condition.field, lang);
    const value = this.valueToCode(condition.value, lang);
    const negation = condition.negated ? '!' : '';

    switch (condition.operator) {
      case 'equals':
        return `${negation}(${field} === ${value})`;
      case 'not_equals':
        return `${field} !== ${value}`;
      case 'greater_than':
        return `${field} > ${value}`;
      case 'less_than':
        return `${field} < ${value}`;
      case 'greater_or_equal':
        return `${field} >= ${value}`;
      case 'less_or_equal':
        return `${field} <= ${value}`;
      case 'contains':
        if (lang === 'python') return `${value} in ${field}`;
        return `${field}.includes(${value})`;
      case 'is_empty':
        if (lang === 'python') return `not ${field}`;
        return `!${field} || ${field}.length === 0`;
      case 'is_not_empty':
        if (lang === 'python') return `bool(${field})`;
        return `${field} && ${field}.length > 0`;
      case 'is_null':
        if (lang === 'python') return `${field} is None`;
        return `${field} === null`;
      default:
        return `${field} === ${value}`;
    }
  }

  private getFieldAccessor(field: string, lang: GeneratorTargetLanguage): string {
    switch (lang) {
      case 'python':
        return `input_data.get('${field}')`;
      case 'java':
        return `input.get("${field}")`;
      default:
        return `input.${field}`;
    }
  }

  private valueToCode(value: { type: string; value: unknown }, lang: GeneratorTargetLanguage): string {
    if (value.type === 'field') {
      return this.getFieldAccessor(String(value.value), lang);
    }

    if (typeof value.value === 'string') {
      return lang === 'java' ? `"${value.value}"` : `'${value.value}'`;
    }

    return String(value.value);
  }

  private generateSQLConditions(conditions: RuleCondition[]): string {
    if (conditions.length === 0) return '1=1';

    return conditions.map(c => {
      const field = c.field;
      const value = typeof c.value.value === 'string' 
        ? `'${c.value.value}'` 
        : c.value.value;

      switch (c.operator) {
        case 'equals': return `${field} = ${value}`;
        case 'not_equals': return `${field} <> ${value}`;
        case 'greater_than': return `${field} > ${value}`;
        case 'less_than': return `${field} < ${value}`;
        case 'is_null': return `${field} IS NULL`;
        case 'is_not_empty': return `${field} IS NOT NULL`;
        default: return `${field} = ${value}`;
      }
    }).join(' AND ');
  }

  private generateExpression(rule: ParsedRule): string {
    const conditions = rule.conditions
      .map(c => `(${c.field} ${c.operator} ${JSON.stringify(c.value.value)})`)
      .join(' AND ');

    return `IF ${conditions || 'TRUE'} THEN ${rule.actions.map(a => a.type).join(', ')}`;
  }

  private generatePseudocode(rule: ParsedRule): string {
    let code = `RULE: ${rule.intent}\n`;
    code += `WHEN:\n`;
    for (const condition of rule.conditions) {
      code += `  ${condition.field} ${condition.operator} ${JSON.stringify(condition.value.value)}\n`;
    }
    code += `THEN:\n`;
    for (const action of rule.actions) {
      code += `  ${action.type} ${action.target}\n`;
    }
    return code;
  }

  private generateTestCases(rule: ParsedRule): TestCase[] {
    const testCases: TestCase[] = [];
    const fields = this.extractFields(rule);

    // Generate passing test case
    testCases.push({
      id: `test_pass_${Date.now()}`,
      name: 'Should pass with valid input',
      inputs: this.generateValidInputs(rule, fields),
      expectedOutputs: {},
      expectedResult: 'pass',
      generated: true,
    });

    // Generate failing test case
    testCases.push({
      id: `test_fail_${Date.now()}`,
      name: 'Should fail with invalid input',
      inputs: this.generateInvalidInputs(rule, fields),
      expectedOutputs: {},
      expectedResult: 'fail',
      generated: true,
    });

    return testCases;
  }

  private generateTypeScriptTests(rule: ParsedRule, funcName: string, interfaceName: string): string {
    const indent = this.getIndent();
    let code = `\ndescribe('${funcName}', () => {\n`;
    
    code += `${indent}it('should return true for valid input', () => {\n`;
    code += `${indent}${indent}const input: ${interfaceName} = ${JSON.stringify(this.generateValidInputs(rule, this.extractFields(rule)), null, 2)};\n`;
    code += `${indent}${indent}expect(${funcName}(input)).toBe(true);\n`;
    code += `${indent}});\n\n`;

    code += `${indent}it('should return false for invalid input', () => {\n`;
    code += `${indent}${indent}const input: ${interfaceName} = ${JSON.stringify(this.generateInvalidInputs(rule, this.extractFields(rule)), null, 2)};\n`;
    code += `${indent}${indent}expect(${funcName}(input)).toBe(false);\n`;
    code += `${indent}});\n`;

    code += '});\n';
    return code;
  }

  private extractFields(rule: ParsedRule): string[] {
    const fields = new Set<string>();
    
    if (rule.subject.field) fields.add(rule.subject.field);
    
    for (const condition of rule.conditions) {
      fields.add(condition.field);
      if (condition.value.type === 'field') {
        fields.add(String(condition.value.value));
      }
    }

    return Array.from(fields);
  }

  private generateValidInputs(rule: ParsedRule, fields: string[]): Record<string, unknown> {
    const inputs: Record<string, unknown> = {};
    
    for (const field of fields) {
      const condition = rule.conditions.find(c => c.field === field);
      if (condition) {
        inputs[field] = this.generateValueForCondition(condition, true);
      } else {
        inputs[field] = 'test_value';
      }
    }

    return inputs;
  }

  private generateInvalidInputs(rule: ParsedRule, fields: string[]): Record<string, unknown> {
    const inputs: Record<string, unknown> = {};
    
    for (const field of fields) {
      const condition = rule.conditions.find(c => c.field === field);
      if (condition) {
        inputs[field] = this.generateValueForCondition(condition, false);
      } else {
        inputs[field] = null;
      }
    }

    return inputs;
  }

  private generateValueForCondition(condition: RuleCondition, shouldPass: boolean): unknown {
    const value = condition.value.value;

    switch (condition.operator) {
      case 'equals':
        return shouldPass ? value : 'other_value';
      case 'greater_than':
        return shouldPass ? (typeof value === 'number' ? value + 1 : 100) : 0;
      case 'less_than':
        return shouldPass ? (typeof value === 'number' ? value - 1 : 0) : 100;
      case 'is_not_empty':
        return shouldPass ? 'non_empty' : '';
      case 'is_empty':
        return shouldPass ? '' : 'not_empty';
      default:
        return shouldPass ? value : null;
    }
  }

  private generateDescription(rule: ParsedRule): string {
    const subjectDesc = rule.subject.field 
      ? `${rule.subject.entity}.${rule.subject.field}` 
      : rule.subject.entity;
    
    return `${rule.intent.charAt(0).toUpperCase() + rule.intent.slice(1)} rule for ${subjectDesc}`;
  }

  private formatName(name: string, convention: 'camelCase' | 'snake_case' | 'PascalCase'): string {
    const words = name.replace(/[^a-zA-Z0-9]/g, ' ').split(/\s+/).filter(w => w);

    switch (convention) {
      case 'camelCase':
        return words.map((w, i) => i === 0 ? w.toLowerCase() : w.charAt(0).toUpperCase() + w.slice(1).toLowerCase()).join('');
      case 'snake_case':
        return words.map(w => w.toLowerCase()).join('_');
      case 'PascalCase':
        return words.map(w => w.charAt(0).toUpperCase() + w.slice(1).toLowerCase()).join('');
    }
  }

  private getIndent(): string {
    return this.config.style.indentation === 'tabs' 
      ? '\t' 
      : ' '.repeat(this.config.style.indentSize);
  }
}

// ============================================================================
// Factory
// ============================================================================

export function createRuleCodeGenerator(config?: Partial<GeneratorConfig>): RuleCodeGenerator {
  return new RuleCodeGenerator(config);
}
