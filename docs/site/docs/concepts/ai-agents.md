---
sidebar_position: 2
---

# AI Agents

Deep dive into MigrationPilot's four specialized AI agents.

## Archeologist Agent

The Archeologist excavates knowledge from legacy code.

### Capabilities
- Parse and understand legacy syntax
- Extract business rules with confidence scores
- Identify data structures and relationships
- Map dependencies and call graphs

### Tools
- `analyze_structure` - Parse AST and semantic info
- `extract_business_rules` - Find calculations and logic
- `identify_dependencies` - Map external dependencies
- `summarize_module` - Generate human-readable summaries

## Architect Agent

The Architect designs modern system architecture.

### Capabilities
- Decompose monoliths into services
- Design database schemas
- Define API contracts
- Create module boundaries

### Tools
- `design_service` - Define service responsibilities
- `map_database` - Convert data structures to schemas
- `create_api` - Generate OpenAPI specifications
- `plan_migration` - Create migration phases

## Builder Agent

The Builder generates clean, modern code.

### Capabilities
- Generate idiomatic code in target language
- Create comprehensive test suites
- Add documentation and comments
- Maintain source traceability

### Tools
- `generate_code` - Create source files
- `generate_tests` - Create unit/integration tests
- `add_documentation` - Generate docs
- `format_code` - Apply style guidelines

## Validator Agent

The Validator proves behavioral equivalence.

### Capabilities
- Generate diverse test cases
- Run parallel executions
- Compare outputs with tolerance
- Calculate confidence scores

### Tools
- `generate_test_cases` - Create boundary/partition tests
- `execute_legacy` - Run legacy system
- `execute_modern` - Run modern system
- `compare_outputs` - Analyze differences
