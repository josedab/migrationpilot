/**
 * TypeScript/NestJS Code Generator
 */

import { BaseGenerator } from '../base-generator.js';
import type { GeneratedFile, ModuleDesign, GeneratorBusinessRule, GeneratorDataStructure } from '../types.js';

export class NestJSGenerator extends BaseGenerator {
  
  protected getFileExtension(): string {
    return '.ts';
  }

  protected mapType(legacyType: string): string {
    const typeMap: Record<string, string> = {
      'PIC 9': 'number',
      'PIC 99': 'number',
      'PIC 9(n)': 'number',
      'PIC S9': 'number',
      'PIC X': 'string',
      'PIC X(n)': 'string',
      'PIC 9V99': 'number',
      'COMP': 'number',
      'COMP-3': 'number',
      'INTEGER': 'number',
      'REAL': 'number',
      'DOUBLE PRECISION': 'number',
      'CHARACTER': 'string',
      'String': 'string',
      'Integer': 'number',
      'Currency': 'number',
      'Date': 'Date',
      'Boolean': 'boolean',
    };
    return typeMap[legacyType] || 'unknown';
  }

  protected generateModels(structures: GeneratorDataStructure[]): GeneratedFile[] {
    const files: GeneratedFile[] = [];
    
    // Generate DTOs
    for (const structure of structures) {
      const className = this.toPascalCase(structure.name);
      const content = this.generateDTO(structure, className);
      files.push({
        path: `${this.config.outputDir}/dto/${this.toKebabCase(structure.name)}.dto.ts`,
        content,
        type: 'model',
      });
    }

    // Generate entities
    for (const structure of structures) {
      const className = this.toPascalCase(structure.name);
      const content = this.generateEntity(structure, className);
      files.push({
        path: `${this.config.outputDir}/entities/${this.toKebabCase(structure.name)}.entity.ts`,
        content,
        type: 'model',
      });
    }

    // Generate index file
    const indexContent = this.generateModelsIndex(structures);
    files.push({
      path: `${this.config.outputDir}/dto/index.ts`,
      content: indexContent,
      type: 'source',
    });

    return files;
  }

  private generateDTO(structure: GeneratorDataStructure, className: string): string {
    const fields = structure.fields || [];
    
    return `import { IsNotEmpty, IsOptional, IsNumber, IsString, IsDate } from 'class-validator';
import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';

/**
 * ${structure.description || structure.name}
 * 
 * Source: ${structure.sourceLocation?.file || 'legacy code'}
 * Lines: ${structure.sourceLocation?.startLine || 'N/A'}-${structure.sourceLocation?.endLine || 'N/A'}
 */
export class ${className}Dto {
${fields.map(f => this.generateDTOField(f)).join('\n\n')}
}

export class Create${className}Dto {
${fields.filter(f => f.name.toLowerCase() !== 'id').map(f => this.generateDTOField(f)).join('\n\n')}
}

export class Update${className}Dto {
${fields.filter(f => f.name.toLowerCase() !== 'id').map(f => this.generateDTOField(f, true)).join('\n\n')}
}
`;
  }

  private generateDTOField(
    field: { name: string; type: string; description?: string; nullable?: boolean },
    optional = false
  ): string {
    const tsType = this.mapType(field.type);
    const fieldName = this.toCamelCase(field.name);
    const nullable = field.nullable !== false || optional;
    
    const decorators: string[] = [];
    
    if (nullable) {
      decorators.push('  @ApiPropertyOptional()');
      decorators.push('  @IsOptional()');
    } else {
      decorators.push('  @ApiProperty()');
      decorators.push('  @IsNotEmpty()');
    }
    
    if (tsType === 'number') {
      decorators.push('  @IsNumber()');
    } else if (tsType === 'string') {
      decorators.push('  @IsString()');
    } else if (tsType === 'Date') {
      decorators.push('  @IsDate()');
    }
    
    return `${decorators.join('\n')}
  /** ${field.description || field.name} */
  ${fieldName}${nullable ? '?' : ''}: ${tsType};`;
  }

  private generateEntity(structure: GeneratorDataStructure, className: string): string {
    const fields = structure.fields || [];
    
    return `import { Entity, Column, PrimaryGeneratedColumn, CreateDateColumn, UpdateDateColumn } from 'typeorm';

/**
 * ${structure.description || structure.name}
 * 
 * Source: ${structure.sourceLocation?.file || 'legacy code'}
 */
@Entity('${this.toSnakeCase(structure.name)}')
export class ${className}Entity {
  @PrimaryGeneratedColumn('uuid')
  id: string;

${fields.map(f => this.generateEntityField(f)).join('\n\n')}

  @CreateDateColumn()
  createdAt: Date;

  @UpdateDateColumn()
  updatedAt: Date;
}
`;
  }

  private generateEntityField(field: { name: string; type: string; nullable?: boolean }): string {
    const tsType = this.mapType(field.type);
    const fieldName = this.toCamelCase(field.name);
    const nullable = field.nullable !== false;
    const columnType = this.mapToTypeORMType(field.type);
    
    return `  @Column({ type: '${columnType}', nullable: ${nullable} })
  ${fieldName}: ${tsType};`;
  }

  private mapToTypeORMType(legacyType: string): string {
    const tsType = this.mapType(legacyType);
    const typeMap: Record<string, string> = {
      'number': 'decimal',
      'string': 'varchar',
      'boolean': 'boolean',
      'Date': 'timestamp',
    };
    return typeMap[tsType] || 'varchar';
  }

  private generateModelsIndex(structures: GeneratorDataStructure[]): string {
    return structures.map(s => 
      `export * from './${this.toKebabCase(s.name)}.dto';`
    ).join('\n');
  }

  protected generateInterfaces(design: ModuleDesign): GeneratedFile[] {
    const content = this.generateServiceInterfaces(design);
    return [{
      path: `${this.config.outputDir}/interfaces/services.interface.ts`,
      content,
      type: 'interface',
    }];
  }

  private generateServiceInterfaces(design: ModuleDesign): string {
    return `/**
 * Service Interfaces
 * 
 * Type definitions for service contracts.
 * Generated by MigrationPilot.
 */

${design.services.map(s => {
  const interfaceName = `I${this.toPascalCase(s.name)}Service`;
  return `export interface ${interfaceName} {
${(s.endpoints || []).map(e => `  /**
   * ${e.description}
   */
  ${this.toCamelCase(e.path.replace(/\//g, '_'))}(${e.requestBody ? `request: ${e.requestBody}` : ''}): Promise<${e.responseType}>;
`).join('\n')}
}`;
}).join('\n\n')}
`;
  }

  protected generateServices(
    design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[] {
    const files: GeneratedFile[] = [];

    // Generate controller
    const controllerContent = this.generateController(design);
    files.push({
      path: `${this.config.outputDir}/controllers/${this.toKebabCase(design.name)}.controller.ts`,
      content: controllerContent,
      type: 'controller',
    });

    // Generate business rules service
    const rulesContent = this.generateBusinessRulesService(rules);
    files.push({
      path: `${this.config.outputDir}/services/business-rules.service.ts`,
      content: rulesContent,
      type: 'service',
    });

    // Generate module
    const moduleContent = this.generateModule(design);
    files.push({
      path: `${this.config.outputDir}/${this.toKebabCase(design.name)}.module.ts`,
      content: moduleContent,
      type: 'source',
    });

    return files;
  }

  private generateController(design: ModuleDesign): string {
    const className = this.toPascalCase(design.name) + 'Controller';
    
    return `import { Controller, Get, Post, Put, Delete, Body, Param, HttpCode, HttpStatus } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse } from '@nestjs/swagger';
import { BusinessRulesService } from '../services/business-rules.service';

/**
 * ${design.description}
 * 
 * Generated by MigrationPilot
 */
@ApiTags('${design.name}')
@Controller('api/${this.toKebabCase(design.name)}')
export class ${className} {
  constructor(private readonly businessRulesService: BusinessRulesService) {}

${design.services.flatMap(s => s.endpoints || []).map(e => `
  @${e.method.charAt(0) + e.method.slice(1).toLowerCase()}('${e.path}')
  @HttpCode(HttpStatus.OK)
  @ApiOperation({ summary: '${e.description}' })
  @ApiResponse({ status: 200, description: 'Success' })
  async ${this.toCamelCase(e.path.replace(/\//g, '_'))}(${e.requestBody ? `@Body() request: ${e.requestBody}` : ''}): Promise<${e.responseType}> {
    // TODO: Implement endpoint logic
    throw new Error('Not implemented');
  }
`).join('\n')}
}
`;
  }

  private generateBusinessRulesService(rules: GeneratorBusinessRule[]): string {
    return `import { Injectable, Logger } from '@nestjs/common';

/**
 * Business Rules Service
 * 
 * Contains implementations of all extracted business rules.
 * Each method is documented with its source location and rule ID.
 * Generated by MigrationPilot.
 */
@Injectable()
export class BusinessRulesService {
  private readonly logger = new Logger(BusinessRulesService.name);

${rules.map(r => this.generateTSRuleMethod(r)).join('\n\n')}
}
`;
  }

  private generateTSRuleMethod(rule: GeneratorBusinessRule): string {
    const methodName = this.toCamelCase(rule.name.replace(/\s+/g, '_'));
    const inputs = rule.inputs || [];
    const output = rule.outputs?.[0];
    
    const params = inputs.map(i => `${this.toCamelCase(i.name)}: ${this.mapType(i.type)}`).join(', ');
    const returnType = output ? this.mapType(output.type) : 'void';

    return `  /**
   * ${rule.name}
   * 
   * ${rule.description}
   * 
   * @ruleId ${rule.id}
   * @source ${rule.sourceLocation?.file || 'N/A'}:${rule.sourceLocation?.startLine || 'N/A'}-${rule.sourceLocation?.endLine || 'N/A'}
   * @confidence ${(rule.confidence * 100).toFixed(1)}%
   * 
${inputs.map(i => `   * @param ${this.toCamelCase(i.name)} ${i.description || i.name}`).join('\n')}
${output ? `   * @returns ${output.description || output.name}` : ''}
   */
  ${methodName}(${params}): ${returnType} {
    this.logger.debug(\`Executing rule ${rule.id}: ${rule.name}\`);
    
    // Business logic from source:
    // ${rule.logic?.calculation?.split('\n').join('\n    // ') || 'No calculation defined'}
    
    // TODO: Implement extracted logic
    ${returnType !== 'void' ? `return ${this.getTSDefaultValue(returnType)};` : ''}
  }`;
  }

  private generateModule(design: ModuleDesign): string {
    const moduleName = this.toPascalCase(design.name) + 'Module';
    const controllerName = this.toPascalCase(design.name) + 'Controller';
    
    return `import { Module } from '@nestjs/common';
import { ${controllerName} } from './controllers/${this.toKebabCase(design.name)}.controller';
import { BusinessRulesService } from './services/business-rules.service';

@Module({
  controllers: [${controllerName}],
  providers: [BusinessRulesService],
  exports: [BusinessRulesService],
})
export class ${moduleName} {}
`;
  }

  protected generateTests(
    _design: ModuleDesign,
    rules: GeneratorBusinessRule[]
  ): GeneratedFile[] {
    const content = this.generateJestTests(rules);
    return [{
      path: `${this.config.outputDir}/services/business-rules.service.spec.ts`,
      content,
      type: 'test',
    }];
  }

  private generateJestTests(rules: GeneratorBusinessRule[]): string {
    return `import { Test, TestingModule } from '@nestjs/testing';
import { BusinessRulesService } from './business-rules.service';

/**
 * Business Rules Service Tests
 * 
 * Unit tests for validating behavioral equivalence.
 * Generated by MigrationPilot.
 */
describe('BusinessRulesService', () => {
  let service: BusinessRulesService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [BusinessRulesService],
    }).compile();

    service = module.get<BusinessRulesService>(BusinessRulesService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });

${rules.map(r => this.generateJestTestCase(r)).join('\n\n')}
});
`;
  }

  private generateJestTestCase(rule: GeneratorBusinessRule): string {
    const methodName = this.toCamelCase(rule.name.replace(/\s+/g, '_'));
    const testName = this.toPascalCase(rule.name.replace(/\s+/g, ' '));

    return `  describe('${testName}', () => {
    it('should implement ${rule.id} correctly', () => {
      // Arrange
      // TODO: Set up test inputs
      
      // Act
      // const result = service.${methodName}(...);
      
      // Assert
      // expect(result).toEqual(expected);
    });

    it('should handle boundary values', () => {
      // Edge cases: ${rule.edgeCases?.join(', ') || 'None documented'}
    });

    it.each([
      // TODO: Add test cases
    ])('should produce correct output for %s', (input, expected) => {
      // const result = service.${methodName}(input);
      // expect(result).toEqual(expected);
    });
  });`;
  }

  // Helper methods
  private toPascalCase(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, ' ')
      .split(' ')
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }

  private toCamelCase(name: string): string {
    const pascal = this.toPascalCase(name);
    return pascal.charAt(0).toLowerCase() + pascal.slice(1);
  }

  private toSnakeCase(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, '_')
      .replace(/([a-z])([A-Z])/g, '$1_$2')
      .toLowerCase();
  }

  private toKebabCase(name: string): string {
    return name
      .replace(/[^a-zA-Z0-9]+/g, '-')
      .replace(/([a-z])([A-Z])/g, '$1-$2')
      .toLowerCase();
  }

  private getTSDefaultValue(type: string): string {
    const defaults: Record<string, string> = {
      'number': '0',
      'string': "''",
      'boolean': 'false',
      'Date': 'new Date()',
      'unknown': 'undefined',
    };
    return defaults[type] || 'undefined';
  }
}
