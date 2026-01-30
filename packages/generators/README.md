# @migrationpilot/generators

Code generators for target languages in MigrationPilot.

## Overview

This package provides code generators that transform architecture designs into clean, idiomatic source code for modern target languages and frameworks.

## Supported Targets

| Language | Frameworks | Generator |
|----------|------------|-----------|
| Java | Spring Boot, Quarkus | `JavaGenerator` |
| Python | FastAPI, Django | `PythonGenerator` |
| TypeScript | NestJS, Express | `TypeScriptGenerator` |
| Go | Standard Library, Gin | `GoGenerator` |
| C# | .NET Core, ASP.NET Core | `CSharpGenerator` |

## Usage

### Basic Generation

```typescript
import { createGenerator } from '@migrationpilot/generators';

const generator = createGenerator('java', {
  framework: 'spring-boot',
  version: '3.2',
});

const files = await generator.generate(architectureDesign, {
  includeTests: true,
  includeDocumentation: true,
  packageName: 'com.example.payroll',
});
```

### Java/Spring Boot

```typescript
import { JavaGenerator } from '@migrationpilot/generators/java';

const generator = new JavaGenerator({
  framework: 'spring-boot',
  springVersion: '3.2',
  javaVersion: '21',
});

const result = await generator.generateService(serviceDefinition, {
  packageName: 'com.example.payroll',
  includeTests: true,
  testFramework: 'junit5',
});

// Generates:
// - src/main/java/com/example/payroll/service/PayrollService.java
// - src/main/java/com/example/payroll/controller/PayrollController.java
// - src/main/java/com/example/payroll/dto/PayrollDTO.java
// - src/main/java/com/example/payroll/repository/PayrollRepository.java
// - src/test/java/com/example/payroll/service/PayrollServiceTest.java
```

### Python/FastAPI

```typescript
import { PythonGenerator } from '@migrationpilot/generators/python';

const generator = new PythonGenerator({
  framework: 'fastapi',
  pythonVersion: '3.11',
});

const result = await generator.generateService(serviceDefinition, {
  moduleName: 'payroll',
  includeTests: true,
  testFramework: 'pytest',
});

// Generates:
// - payroll/service.py
// - payroll/router.py
// - payroll/models.py
// - payroll/schemas.py
// - tests/test_payroll.py
```

### TypeScript/NestJS

```typescript
import { TypeScriptGenerator } from '@migrationpilot/generators/typescript';

const generator = new TypeScriptGenerator({
  framework: 'nestjs',
  nodeVersion: '20',
});

const result = await generator.generateService(serviceDefinition, {
  moduleName: 'payroll',
  includeTests: true,
});

// Generates:
// - src/payroll/payroll.service.ts
// - src/payroll/payroll.controller.ts
// - src/payroll/payroll.module.ts
// - src/payroll/dto/payroll.dto.ts
// - src/payroll/payroll.service.spec.ts
```

### Go/Gin

```typescript
import { GoGenerator } from '@migrationpilot/generators/go';

const generator = new GoGenerator({
  framework: 'gin',
  goVersion: '1.21',
});

const result = await generator.generateService(serviceDefinition, {
  packageName: 'payroll',
  modulePath: 'github.com/example/payroll',
});

// Generates:
// - internal/payroll/service.go
// - internal/payroll/handler.go
// - internal/payroll/model.go
// - internal/payroll/service_test.go
```

### C#/.NET Core

```typescript
import { CSharpGenerator } from '@migrationpilot/generators/csharp';

const generator = new CSharpGenerator({
  framework: 'aspnet-core',
  dotnetVersion: '8.0',
});

const result = await generator.generateService(serviceDefinition, {
  namespace: 'Example.Payroll',
  includeTests: true,
});

// Generates:
// - Services/PayrollService.cs
// - Controllers/PayrollController.cs
// - Models/PayrollModel.cs
// - DTOs/PayrollDto.cs
// - Tests/PayrollServiceTests.cs
```

## Code Mapping

The `CodeMapper` maintains traceability between legacy and generated code:

```typescript
import { CodeMapper } from '@migrationpilot/generators';

const mapper = new CodeMapper();

// Track mappings during generation
mapper.addMapping({
  legacy: { file: 'PAYROLL.cbl', startLine: 150, endLine: 165 },
  modern: { file: 'PayrollService.java', startLine: 45, endLine: 62 },
  businessRule: 'BR-001',
  confidence: 0.95,
});

// Query mappings
const legacyLocation = mapper.findLegacySource('PayrollService.java', 50);
const modernLocation = mapper.findModernCode('PAYROLL.cbl', 155);
```

## Generated Code Features

All generators produce code with:

1. **Documentation**: JSDoc, Javadoc, docstrings with legacy references
2. **Traceability**: Comments linking to source code and business rules
3. **Tests**: Unit tests covering all paths and edge cases
4. **Type Safety**: Full type annotations and validation
5. **Framework Best Practices**: Idiomatic patterns for each framework

## Configuration Options

```typescript
interface GeneratorOptions {
  // Include unit tests
  includeTests: boolean;
  
  // Include documentation comments
  includeDocumentation: boolean;
  
  // Code style preference
  codeStyle: 'verbose' | 'concise';
  
  // Test framework
  testFramework?: string;
  
  // Add traceability comments
  includeTraceability: boolean;
}
```

## Installation

```bash
pnpm add @migrationpilot/generators
```

## Development

```bash
# Build
pnpm build

# Test
pnpm test

# Type check
pnpm typecheck
```

## License

MIT
