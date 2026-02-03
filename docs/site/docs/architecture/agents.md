# AI Agents Architecture

MigrationPilot uses a multi-agent architecture where specialized AI agents handle different phases of the migration process. Each agent is built on the GitHub Copilot SDK and optimized for its specific task.

## Agent Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    Migration Orchestrator                        │
│  Coordinates agents, manages state, handles human-in-the-loop   │
└─────────────────────────┬───────────────────────────────────────┘
                          │
    ┌─────────────────────┼─────────────────────┐
    │                     │                     │
┌───▼───┐  ┌──────────────▼──────────────┐  ┌──▼────┐
│Archeol│  │         Architect           │  │Builder│
│ogist  │──│  Designs modern architecture│──│       │
│       │  │                              │  │       │
└───┬───┘  └──────────────┬──────────────┘  └───┬───┘
    │                     │                     │
    │              ┌──────▼──────┐              │
    │              │  Validator  │◄─────────────┘
    │              │             │
    │              └─────────────┘
    │
    └──────────────────────────────────────────────
                        │
                  ┌─────▼─────┐
                  │ Explainer │
                  │           │
                  └───────────┘
```

## Archeologist Agent

**Purpose**: Analyzes legacy code to understand structure and extract business rules.

### Capabilities

- Parse legacy language syntax (COBOL, Fortran, VB6, Legacy Java)
- Identify data structures and their relationships
- Extract business rules with confidence scores
- Generate human-readable documentation

### Tools

```typescript
const archeologistTools = [
  {
    name: 'analyze_file',
    description: 'Parse and analyze a legacy source file',
    parameters: {
      content: 'string',
      language: 'cobol | fortran | vb6 | java-legacy',
      filename: 'string',
    },
  },
  {
    name: 'extract_rules',
    description: 'Extract business rules from analyzed code',
    parameters: {
      procedures: 'Procedure[]',
      dataStructures: 'DataStructure[]',
    },
  },
  {
    name: 'calculate_complexity',
    description: 'Calculate cyclomatic complexity',
    parameters: {
      procedure: 'Procedure',
    },
  },
];
```

### Example Output

```json
{
  "analysis": {
    "language": "cobol",
    "files": 12,
    "procedures": 78,
    "dataStructures": 45
  },
  "businessRules": [
    {
      "id": "BR-001",
      "name": "Interest Rate Calculation",
      "confidence": 0.95,
      "sourceLocation": "CALCINT.cbl:245-280"
    }
  ]
}
```

---

## Architect Agent

**Purpose**: Designs modern architecture based on analysis results.

### Capabilities

- Design microservices or modular monolith architecture
- Define service boundaries using domain-driven design
- Generate API specifications (OpenAPI)
- Create database schemas
- Plan data migration strategies

### Tools

```typescript
const architectTools = [
  {
    name: 'design_services',
    description: 'Design service boundaries from business domains',
    parameters: {
      businessRules: 'BusinessRule[]',
      dataStructures: 'DataStructure[]',
      pattern: 'microservices | modular-monolith',
    },
  },
  {
    name: 'generate_api_spec',
    description: 'Generate OpenAPI specification for a service',
    parameters: {
      service: 'ServiceDefinition',
      style: 'rest | graphql',
    },
  },
  {
    name: 'design_database',
    description: 'Design database schema',
    parameters: {
      dataStructures: 'DataStructure[]',
      targetDatabase: 'postgresql | mysql | mongodb',
    },
  },
];
```

### Example Output

```json
{
  "services": [
    {
      "name": "loan-service",
      "responsibilities": ["loan calculation", "eligibility check"],
      "apis": ["/api/loans/calculate", "/api/loans/eligibility"],
      "database": "loan_db"
    }
  ],
  "apis": [
    {
      "path": "/api/loans/calculate",
      "method": "POST",
      "requestBody": "LoanCalculationRequest",
      "response": "LoanCalculationResponse"
    }
  ]
}
```

---

## Builder Agent

**Purpose**: Generates modern, idiomatic code from architecture designs.

### Capabilities

- Generate code in target language (Java, Python, TypeScript, Go, C#)
- Apply framework-specific patterns (Spring Boot, FastAPI, NestJS)
- Create unit and integration tests
- Generate documentation

### Tools

```typescript
const builderTools = [
  {
    name: 'generate_service',
    description: 'Generate service implementation',
    parameters: {
      service: 'ServiceDefinition',
      targetLanguage: 'java | python | typescript | go | csharp',
      framework: 'string',
    },
  },
  {
    name: 'generate_model',
    description: 'Generate data model/entity',
    parameters: {
      dataStructure: 'DataStructure',
      targetLanguage: 'string',
    },
  },
  {
    name: 'generate_tests',
    description: 'Generate test cases for a service',
    parameters: {
      service: 'ServiceDefinition',
      businessRules: 'BusinessRule[]',
    },
  },
];
```

### Code Quality

Generated code follows best practices:

- **Clean Architecture** - Separation of concerns
- **SOLID Principles** - Maintainable, extensible code
- **Idiomatic Style** - Language-specific conventions
- **Documentation** - JSDoc/Javadoc comments with source references

---

## Validator Agent

**Purpose**: Validates behavioral equivalence between legacy and modern systems.

### Capabilities

- Generate comprehensive test cases
- Compare outputs with configurable tolerance
- Calculate confidence scores
- Identify and categorize differences

### Tools

```typescript
const validatorTools = [
  {
    name: 'generate_test_cases',
    description: 'Generate test cases from business rules',
    parameters: {
      rules: 'BusinessRule[]',
      strategies: 'TestStrategy[]',
      maxTestsPerRule: 'number',
    },
  },
  {
    name: 'compare_outputs',
    description: 'Compare legacy and modern outputs',
    parameters: {
      legacy: 'any',
      modern: 'any',
      tolerances: 'ToleranceConfig',
    },
  },
  {
    name: 'calculate_confidence',
    description: 'Calculate equivalence confidence score',
    parameters: {
      testResults: 'TestResult[]',
    },
  },
];
```

---

## Explainer Agent

**Purpose**: Provides interactive Q&A about legacy code and migration.

### Capabilities

- Answer questions about legacy code behavior
- Explain business rules in plain language
- Trace data flow through the system
- Provide migration recommendations

### Tools

```typescript
const explainerTools = [
  {
    name: 'explain_code',
    description: 'Explain what a code section does',
    parameters: {
      code: 'string',
      language: 'string',
      context: 'string',
    },
  },
  {
    name: 'trace_data_flow',
    description: 'Trace how data flows through the system',
    parameters: {
      startPoint: 'string',
      endPoint: 'string',
    },
  },
  {
    name: 'answer_question',
    description: 'Answer a natural language question',
    parameters: {
      question: 'string',
      context: 'AnalysisContext',
    },
  },
];
```

---

## Orchestrator

The Migration Orchestrator coordinates all agents:

```typescript
class MigrationOrchestrator {
  async migrate(config: MigrationConfig): Promise<MigrationResult> {
    // Phase 1: Analysis
    const analysis = await this.archeologist.analyzeFile(...);
    
    // Human review checkpoint
    if (config.humanReviewRequired) {
      await this.requestHumanReview(analysis.businessRules);
    }
    
    // Phase 2: Architecture
    const architecture = await this.architect.designArchitecture(...);
    
    // Phase 3: Generation
    const generated = await this.builder.generateService(...);
    
    // Phase 4: Validation
    const validation = await this.validator.validateEquivalence(...);
    
    return { analysis, architecture, generated, validation };
  }
}
```

## Agent Communication

Agents communicate through structured messages:

```typescript
interface AgentMessage {
  from: AgentType;
  to: AgentType;
  type: 'request' | 'response' | 'event';
  payload: any;
  correlationId: string;
}
```

## Configuration

Configure agent behavior in project settings:

```json
{
  "agentConfig": {
    "archeologist": {
      "confidenceThreshold": 0.8,
      "maxRulesPerFile": 50
    },
    "architect": {
      "pattern": "microservices",
      "apiStyle": "rest"
    },
    "builder": {
      "generateTests": true,
      "generateDocs": true
    },
    "validator": {
      "testStrategies": ["boundary", "partition", "property"],
      "tolerance": 0.0001
    }
  }
}
```

## Related Topics

- [How It Works](/docs/concepts/how-it-works) - Migration process overview
- [Business Rules](/docs/concepts/business-rules) - Rule extraction details
- [Equivalence Testing](/docs/concepts/equivalence-testing) - Validation details
