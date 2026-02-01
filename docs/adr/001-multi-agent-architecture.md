# ADR-001: Multi-Agent AI Architecture

## Status

Accepted

## Context

MigrationPilot needs to transform legacy code (COBOL, Fortran, VB6) into modern languages while preserving business logic. This is a complex task requiring:

1. **Deep understanding** of legacy code patterns and idioms
2. **Architectural design** to map legacy structures to modern patterns
3. **Code generation** that produces idiomatic, maintainable output
4. **Validation** to ensure behavioral equivalence

A single monolithic AI approach would be difficult to tune, debug, and improve. Different aspects of migration require different expertise and prompting strategies.

## Decision

We will implement a **multi-agent architecture** with specialized agents:

1. **Archeologist Agent**: Analyzes legacy code, extracts business rules, identifies patterns
   - Focus: Understanding and documentation
   - Temperature: Low (0.1) for analytical accuracy

2. **Architect Agent**: Designs modern architecture, service boundaries, data models
   - Focus: Design decisions and trade-offs
   - Temperature: Medium (0.3) for creative solutions

3. **Builder Agent**: Generates modern code and tests
   - Focus: Code quality and idioms
   - Temperature: Low (0.1) for consistent output

4. **Validator Agent**: Validates behavioral equivalence, generates test cases
   - Focus: Correctness verification
   - Temperature: Very low (0.0) for deterministic validation

These agents are coordinated by a **Migration Orchestrator** that:
- Manages the pipeline flow
- Handles human-in-the-loop reviews
- Tracks progress and state
- Provides callbacks for UI updates

## Consequences

### Positive

- **Specialized prompts**: Each agent has optimized system prompts for its task
- **Independent improvement**: Agents can be improved without affecting others
- **Debuggability**: Issues can be isolated to specific agents
- **Parallelization**: Some agent tasks can run concurrently
- **Human-in-the-loop**: Easy to insert review points between agents

### Negative

- **Increased complexity**: More moving parts to manage
- **Context passing**: Information must flow correctly between agents
- **Latency**: Sequential pipeline adds to total time
- **Token costs**: Multiple API calls increase usage

### Mitigations

- Clear interfaces between agents (TypeScript types)
- Comprehensive logging and tracing
- Caching of intermediate results
- Streaming responses for user feedback
