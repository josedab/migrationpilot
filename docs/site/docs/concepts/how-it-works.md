---
sidebar_position: 1
---

# How MigrationPilot Works

MigrationPilot uses a four-stage pipeline powered by specialized AI agents to modernize legacy code while preserving business logic.

## The Migration Pipeline

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  Analysis   │───▶│   Design    │───▶│ Generation  │───▶│ Validation  │
│ (Archeologist)   │ (Architect) │    │  (Builder)  │    │ (Validator) │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
```

## Stage 1: Analysis (Archeologist Agent)

The Archeologist Agent "excavates" your legacy code to understand:

### What It Extracts

1. **Business Rules**: Calculations, validations, and decision logic
2. **Data Structures**: Records, fields, and their relationships
3. **Control Flow**: Program structure and call graphs
4. **Dependencies**: External systems, files, and databases

### How It Works

```typescript
// The Archeologist uses specialized tools
const tools = [
  'analyze_structure',      // Parse and understand code structure
  'extract_business_rules', // Find calculations and logic
  'identify_dependencies',  // Map external dependencies
  'summarize_module',       // Create human-readable summaries
];
```

### Example Output

```yaml
business_rule:
  id: BR-001
  name: Interest Rate Calculation
  type: calculation
  confidence: 0.95
  source:
    file: CALCINT.cbl
    lines: [245, 280]
  inputs:
    - name: principal
      type: decimal
    - name: rate
      type: decimal
    - name: years
      type: integer
  formula: interest = principal * rate * years
  edge_cases:
    - Zero principal returns zero
    - Negative years are invalid
```

## Stage 2: Design (Architect Agent)

The Architect Agent creates a modern architecture based on extracted knowledge:

### What It Designs

1. **Module Structure**: How to organize the new codebase
2. **Service Boundaries**: For microservices migrations
3. **Data Models**: Modern database schemas
4. **API Contracts**: OpenAPI specifications

### Design Patterns

- **Microservices**: Break monolith into independent services
- **Modular Monolith**: Organize without full distribution
- **Strangler Fig**: Gradual replacement behind a facade

### Example Output

```yaml
architecture:
  pattern: microservices
  services:
    - name: interest-service
      responsibility: Calculate interest rates
      apis:
        - POST /api/interest/calculate
      dependencies:
        - account-service
    - name: account-service
      responsibility: Manage accounts
      database: postgres
```

## Stage 3: Generation (Builder Agent)

The Builder Agent generates clean, modern code:

### What It Generates

1. **Source Code**: Idiomatic implementations
2. **Unit Tests**: Coverage for business logic
3. **Integration Tests**: End-to-end validation
4. **Documentation**: Code comments and API docs

### Code Quality

- Follows target language conventions
- Includes source mappings for traceability
- Runs linters and formatters automatically
- Compiles without errors

### Example Output

```java
/**
 * Interest Calculator Service
 * 
 * Migrated from: CALCINT.cbl
 * Business Rule: BR-001
 */
@Service
public class InterestCalculatorService {
    
    /**
     * Calculate interest using simple interest formula.
     * 
     * @param request Interest calculation request
     * @return Calculated interest amount
     * @throws InvalidInputException if years is negative
     */
    public InterestResponse calculate(InterestRequest request) {
        if (request.getYears() < 0) {
            throw new InvalidInputException("Years cannot be negative");
        }
        
        BigDecimal interest = request.getPrincipal()
            .multiply(request.getRate())
            .multiply(BigDecimal.valueOf(request.getYears()))
            .setScale(2, RoundingMode.HALF_UP);
            
        return new InterestResponse(interest);
    }
}
```

## Stage 4: Validation (Validator Agent)

The Validator Agent proves the new code is equivalent:

### Test Generation Strategies

1. **Boundary Value Analysis**: Test edge cases
2. **Equivalence Partitioning**: Test representative inputs
3. **Historical Data**: Replay production scenarios
4. **Property-Based Testing**: Random inputs with invariants

### Parallel Execution

```
┌─────────────┐         ┌─────────────┐
│ Legacy      │         │ Modern      │
│ System      │         │ System      │
└──────┬──────┘         └──────┬──────┘
       │                       │
       ▼                       ▼
   ┌───────┐               ┌───────┐
   │ Output│               │ Output│
   └───┬───┘               └───┬───┘
       │                       │
       └───────────┬───────────┘
                   ▼
             ┌──────────┐
             │ Compare  │
             └──────────┘
```

### Confidence Scoring

The Validator calculates a confidence score based on:
- Test pass rate
- Code coverage
- Edge case handling
- Numeric precision matching

## Human-in-the-Loop

MigrationPilot includes human review at key stages:

### Business Rule Review

SMEs can:
- Approve extracted rules
- Correct misinterpretations
- Add missing edge cases
- Provide context

### Architecture Review

Architects can:
- Modify service boundaries
- Adjust database designs
- Override pattern choices

## Incremental Migration

For large codebases, use the Strangler Fig pattern:

```
     ┌─────────────────────────────────────┐
     │           API Gateway               │
     └─────────────────┬───────────────────┘
                       │
         ┌─────────────┼─────────────┐
         │             │             │
         ▼             ▼             ▼
    ┌─────────┐   ┌─────────┐   ┌─────────┐
    │ Legacy  │   │ Modern  │   │ Modern  │
    │ Module  │   │ Service │   │ Service │
    └─────────┘   └─────────┘   └─────────┘
```

Benefits:
- Migrate one module at a time
- Rollback easily if issues arise
- Maintain production stability
- Validate incrementally

## Next Steps

- [Learn about the AI Agents](/docs/concepts/ai-agents)
- [Understand Business Rules](/docs/concepts/business-rules)
- [Deep dive into Equivalence Testing](/docs/concepts/equivalence-testing)
