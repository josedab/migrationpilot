/**
 * Java/Spring Boot Code Generator
 */

import { BaseGenerator } from '../base-generator.js';
import type {
  GeneratedFile,
  ModuleDesign,
  GeneratorBusinessRule,
  GeneratorDataStructure,
} from '../types.js';

export class SpringBootGenerator extends BaseGenerator {
  
  protected getFileExtension(): string {
    return '.java';
  }

  protected mapType(legacyType: string): string {
    const typeMap: Record<string, string> = {
      'PIC 9': 'Integer',
      'PIC 99': 'Integer',
      'PIC 9(n)': 'Long',
      'PIC S9': 'Integer',
      'PIC X': 'String',
      'PIC X(n)': 'String',
      'PIC 9V99': 'BigDecimal',
      'COMP': 'Integer',
      'COMP-3': 'BigDecimal',
      'INTEGER': 'Integer',
      'REAL': 'Double',
      'DOUBLE PRECISION': 'Double',
      'CHARACTER': 'String',
      'String': 'String',
      'Integer': 'Integer',
      'Currency': 'BigDecimal',
      'Date': 'LocalDate',
      'Boolean': 'Boolean',
    };
    return typeMap[legacyType] || 'Object';
  }

  protected generateModels(structures: GeneratorDataStructure[]): GeneratedFile[] {
    const files: GeneratedFile[] = [];

    for (const structure of structures) {
      const className = this.toClassName(structure.name);
      const content = this.generateModelClass(structure, className);
      
      files.push({
        path: `${this.config.outputDir}/model/${className}.java`,
        content,
        type: 'model',
      });
    }

    return files;
  }

  private generateModelClass(structure: GeneratorDataStructure, className: string): string {
    const fields = structure.fields || [];
    
    return `package ${this.config.packageName}.model;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import jakarta.persistence.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

/**
 * ${structure.description || structure.name}
 * 
 * Migrated from: ${structure.sourceLocation?.file || 'legacy code'}
 * Lines: ${structure.sourceLocation?.startLine || 'N/A'}-${structure.sourceLocation?.endLine || 'N/A'}
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "${this.toSnakeCase(structure.name)}")
public class ${className} {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;

${fields.map(field => this.generateField(field)).join('\n\n')}
}
`;
  }

  private generateField(field: { name: string; type: string; description?: string; nullable?: boolean }): string {
    const javaType = this.mapType(field.type);
    const fieldName = this.toCamelCase(field.name);
    const nullable = field.nullable !== false;
    
    let annotations = '';
    if (!nullable) {
      annotations = '    @Column(nullable = false)\n';
    }
    
    return `${annotations}    /**
     * ${field.description || field.name}
     */
    private ${javaType} ${fieldName};`;
  }

  protected generateInterfaces(design: ModuleDesign): GeneratedFile[] {
    const files: GeneratedFile[] = [];

    for (const service of design.services) {
      const interfaceName = `${this.toClassName(service.name)}Service`;
      const content = this.generateServiceInterface(service, interfaceName);
      
      files.push({
        path: `${this.config.outputDir}/service/${interfaceName}.java`,
        content,
        type: 'interface',
      });
    }

    return files;
  }

  private generateServiceInterface(
    service: ModuleDesign['services'][0],
    interfaceName: string
  ): string {
    return `package ${this.config.packageName}.service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * ${service.description}
 * 
 * Business Rules: ${service.businessRules.join(', ')}
 */
public interface ${interfaceName} {

${(service.endpoints || []).map(endpoint => `    /**
     * ${endpoint.description}
     */
    ${endpoint.responseType} ${this.toMethodName(endpoint.path, endpoint.method)}(${endpoint.requestBody ? endpoint.requestBody + ' request' : ''});
`).join('\n')}
}
`;
  }

  protected generateServices(
    design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[] {
    const files: GeneratedFile[] = [];

    for (const service of design.services) {
      const className = `${this.toClassName(service.name)}ServiceImpl`;
      const interfaceName = `${this.toClassName(service.name)}Service`;
      const content = this.generateServiceImpl(service, className, interfaceName, rules);
      
      files.push({
        path: `${this.config.outputDir}/service/impl/${className}.java`,
        content,
        type: 'service',
      });
    }

    // Generate a main service class with business rule implementations
    const mainService = this.generateBusinessRuleService(rules);
    files.push(mainService);

    return files;
  }

  private generateServiceImpl(
    service: ModuleDesign['services'][0],
    className: string,
    interfaceName: string,
    rules: GeneratorBusinessRule[]
  ): string {
    const relevantRules = rules.filter(r => service.businessRules.includes(r.id));

    return `package ${this.config.packageName}.service.impl;

import ${this.config.packageName}.service.${interfaceName};
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * ${service.description}
 * 
 * Implementation of business rules:
${relevantRules.map(r => ` * - ${r.id}: ${r.name}`).join('\n')}
 */
@Slf4j
@Service
@RequiredArgsConstructor
@Transactional
public class ${className} implements ${interfaceName} {

${(service.endpoints || []).map(endpoint => `    /**
     * ${endpoint.description}
     */
    @Override
    public ${endpoint.responseType} ${this.toMethodName(endpoint.path, endpoint.method)}(${endpoint.requestBody ? endpoint.requestBody + ' request' : ''}) {
        log.debug("Executing ${this.toMethodName(endpoint.path, endpoint.method)}");
        // TODO: Implement business logic
        throw new UnsupportedOperationException("Not implemented yet");
    }
`).join('\n')}
}
`;
  }

  private generateBusinessRuleService(rules: GeneratorBusinessRule[]): GeneratedFile {
    const content = `package ${this.config.packageName}.service;

import org.springframework.stereotype.Service;
import lombok.extern.slf4j.Slf4j;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

/**
 * Business Rules Service
 * 
 * Contains implementations of all extracted business rules.
 * Each method is documented with its source location and rule ID.
 */
@Slf4j
@Service
public class BusinessRulesService {

${rules.map(rule => this.generateRuleMethod(rule)).join('\n\n')}
}
`;

    return {
      path: `${this.config.outputDir}/service/BusinessRulesService.java`,
      content,
      type: 'service',
    };
  }

  private generateRuleMethod(rule: GeneratorBusinessRule): string {
    const methodName = this.toCamelCase(rule.name.replace(/\s+/g, '_'));
    const inputs = rule.inputs || [];
    const output = rule.outputs?.[0];
    
    const params = inputs.map(i => `${this.mapType(i.type)} ${this.toCamelCase(i.name)}`).join(', ');
    const returnType = output ? this.mapType(output.type) : 'void';

    return `    /**
     * ${rule.name}
     * 
     * ${rule.description}
     * 
     * Rule ID: ${rule.id}
     * Source: ${rule.sourceLocation?.file || 'N/A'}:${rule.sourceLocation?.startLine || 'N/A'}-${rule.sourceLocation?.endLine || 'N/A'}
     * Confidence: ${(rule.confidence * 100).toFixed(1)}%
     * 
${inputs.map(i => `     * @param ${this.toCamelCase(i.name)} ${i.description || i.name}`).join('\n')}
${output ? `     * @return ${output.description || output.name}` : ''}
     */
    public ${returnType} ${methodName}(${params}) {
        log.debug("Executing rule ${rule.id}: ${rule.name}");
        
        // Business logic from source:
        // ${rule.logic?.calculation?.split('\n').join('\n        // ') || 'No calculation defined'}
        
        // TODO: Implement extracted logic
        ${returnType !== 'void' ? `return ${this.getDefaultValue(returnType)};` : ''}
    }`;
  }

  protected generateTests(
    _design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[] {
    const files: GeneratedFile[] = [];

    // Generate test for business rules
    const testContent = this.generateBusinessRuleTests(rules);
    files.push({
      path: `${this.config.outputDir}/service/BusinessRulesServiceTest.java`,
      content: testContent,
      type: 'test',
    });

    return files;
  }

  private generateBusinessRuleTests(rules: GeneratorBusinessRule[]): string {
    return `package ${this.config.packageName}.service;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.assertj.core.api.Assertions.*;
import java.math.BigDecimal;

/**
 * Unit tests for BusinessRulesService
 * 
 * Tests validate behavioral equivalence with legacy system
 */
class BusinessRulesServiceTest {

    private BusinessRulesService service;

    @BeforeEach
    void setUp() {
        service = new BusinessRulesService();
    }

${rules.map(rule => this.generateRuleTest(rule)).join('\n\n')}
}
`;
  }

  private generateRuleTest(rule: GeneratorBusinessRule): string {
    const methodName = this.toCamelCase(rule.name.replace(/\s+/g, '_'));
    const testName = `test${this.toClassName(rule.name.replace(/\s+/g, '_'))}`;

    return `    @Test
    @DisplayName("${rule.name} - ${rule.id}")
    void ${testName}() {
        // Arrange
        // TODO: Set up test inputs based on rule: ${rule.id}
        
        // Act
        // var result = service.${methodName}(...);
        
        // Assert
        // TODO: Add assertions based on expected behavior
        // assertThat(result).isEqualTo(expected);
    }

    @Test
    @DisplayName("${rule.name} - boundary values")
    void ${testName}_BoundaryValues() {
        // Test boundary conditions for rule: ${rule.id}
        // Edge cases: ${rule.edgeCases?.join(', ') || 'None documented'}
    }`;
  }

  // Helper methods
  private toClassName(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, ' ')
      .split(' ')
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }

  private toCamelCase(name: string): string {
    const className = this.toClassName(name);
    return className.charAt(0).toLowerCase() + className.slice(1);
  }

  private toSnakeCase(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, '_')
      .replace(/([a-z])([A-Z])/g, '$1_$2')
      .toLowerCase();
  }

  private toMethodName(path: string, method: string): string {
    const cleanPath = path.replace(/[^a-zA-Z0-9]+/g, ' ').trim();
    const methodPrefix = method.toLowerCase();
    return methodPrefix + this.toClassName(cleanPath);
  }

  private getDefaultValue(type: string): string {
    const defaults: Record<string, string> = {
      'Integer': '0',
      'Long': '0L',
      'Double': '0.0',
      'BigDecimal': 'BigDecimal.ZERO',
      'String': '""',
      'Boolean': 'false',
      'LocalDate': 'LocalDate.now()',
    };
    return defaults[type] || 'null';
  }
}
