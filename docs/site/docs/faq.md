# Frequently Asked Questions

Common questions about MigrationPilot and legacy code modernization.

## General Questions

### What is MigrationPilot?

MigrationPilot is an AI-powered platform for modernizing legacy code. It uses specialized AI agents to analyze legacy systems (COBOL, Fortran, VB6, etc.), extract business rules, and generate modern code in languages like Java, Python, TypeScript, Go, and C#.

### How does it differ from traditional migration tools?

Traditional tools focus on line-by-line translation. MigrationPilot:

- **Understands intent**: Extracts business rules, not just syntax
- **Modern architectures**: Generates idiomatic code with modern patterns
- **Human-in-the-loop**: Enables review and approval of extracted rules
- **Equivalence testing**: Validates that behavior is preserved

### What languages does it support?

**Source Languages:**
- COBOL (including COBOL-85, COBOL 2002)
- Fortran (Fortran 77, 90, 95)
- Visual Basic 6
- VBA (Excel, Access)
- Legacy Java (J2EE, EJB)

**Target Languages:**
- Java (Spring Boot)
- Python (FastAPI)
- TypeScript (NestJS)
- Go (Gin)
- C# (.NET Core)

### Is my code secure?

Yes. MigrationPilot can be:
- **Self-hosted**: Run entirely on your infrastructure
- **Air-gapped**: No internet connection required
- **Encrypted**: All data encrypted at rest and in transit

See [Security](./deployment/security.md) for details.

## Analysis Questions

### How accurate is the rule extraction?

Accuracy depends on code quality:

| Code Quality | Typical Accuracy |
|--------------|-----------------|
| Well-documented, clear naming | 90-95% |
| Average enterprise code | 80-90% |
| Poorly documented, complex | 70-80% |

Low-confidence rules are flagged for human review.

### What if a rule is extracted incorrectly?

You can:
1. **Modify** the rule with correct interpretation
2. **Reject** the rule if it's not needed
3. **Add test cases** to validate behavior

### Can it handle complex nested logic?

Yes, but with caveats:
- Complex logic may have lower confidence scores
- Deeply nested conditions are flagged for review
- Consider refactoring during review if appropriate

### What about copybooks/includes?

Provide all related files:
- COBOL copybooks (.cpy)
- Include files
- Related programs

The analyzer resolves dependencies automatically.

## Migration Questions

### How long does a migration take?

Depends on project size:

| Size | Files | Estimated Time |
|------|-------|----------------|
| Small | < 50 | Hours |
| Medium | 50-500 | Days |
| Large | 500-5000 | Weeks |
| Enterprise | 5000+ | Months |

Most time is spent on human review, not AI processing.

### Can I migrate incrementally?

Yes! Use the [Strangler Fig Pattern](./guides/strangler-fig-pattern.md):

1. Migrate one module at a time
2. Run old and new systems in parallel
3. Gradually route traffic to new system
4. Retire old system when ready

### What if I need custom target architecture?

Options:
1. **Configure generators**: Adjust naming, structure, patterns
2. **Post-process output**: Modify generated code
3. **Custom generators**: Create new generator plugins

### Does it preserve comments?

Comments are:
- **Extracted**: Used to understand business rules
- **Regenerated**: New comments explain logic in target language
- **Not copied verbatim**: Code is regenerated, not translated

## Validation Questions

### What is equivalence testing?

Equivalence testing verifies that generated code behaves identically to original code:
- Same inputs produce same outputs
- Edge cases handled correctly
- Error conditions match

### What's a good equivalence score?

| Score | Interpretation |
|-------|----------------|
| 95%+ | Excellent - ready for production |
| 90-95% | Good - minor issues to review |
| 80-90% | Fair - significant review needed |
| <80% | Poor - major issues present |

### Why might tests fail?

Common reasons:
- **Rounding differences**: Float precision varies
- **Date handling**: Timezone or format differences
- **Null handling**: Different default behaviors
- **Encoding**: Character set differences

### Can I add custom test cases?

Yes. Add test cases during rule review:

```bash
PATCH /api/projects/:id/rules/:ruleId
{
  "testCases": [
    { "input": {...}, "expected": {...} }
  ]
}
```

## Deployment Questions

### What infrastructure do I need?

Minimum requirements:
- **CPU**: 4 cores
- **RAM**: 16 GB
- **Storage**: 50 GB SSD
- **Docker**: 20.10+

For production, see [Kubernetes Deployment](./deployment/kubernetes.md).

### Does it need internet access?

No. MigrationPilot can run fully air-gapped:
- Self-hosted AI models
- Local LLM support (Ollama, LM Studio)
- No external API calls required

### Can I use my own LLM?

Yes. Supported options:
- OpenAI API
- Azure OpenAI
- Anthropic Claude
- Local models via Ollama
- Any OpenAI-compatible API

## Integration Questions

### Does it integrate with CI/CD?

Yes. Use the CLI in pipelines:

```yaml
# GitHub Actions example
- name: Analyze Code
  run: migrationpilot analyze --project ${{ env.PROJECT_ID }}
```

### Can I use it via API only?

Yes. The full REST API supports:
- Project management
- File uploads
- Analysis/migration/validation
- Webhooks for status updates

### Is there IDE integration?

Not yet, but planned:
- VS Code extension (planned)
- IntelliJ plugin (planned)

## Troubleshooting

### Analysis seems stuck

Check:
1. File encoding (must be UTF-8 or specified)
2. Memory limits (increase if needed)
3. Complex dependencies (add missing files)

### Migration fails with low confidence

Options:
1. Review and approve low-confidence rules manually
2. Lower confidence threshold (not recommended)
3. Add documentation/comments to source code

### Tests fail for financial calculations

Likely precision issues:
1. Check decimal precision settings
2. Verify rounding mode matches original
3. Compare with known test cases

## Getting Help

### Where can I get support?

- **Documentation**: You're here!
- **GitHub Issues**: Bug reports and feature requests
- **Discussions**: Community Q&A
- **Enterprise**: Contact sales for support plans

### How do I report a bug?

1. Check existing issues
2. Provide minimal reproduction
3. Include version numbers
4. Attach sanitized logs

### Can I contribute?

Yes! See [Contributing](./contributing.md) for guidelines.

## Related Topics

- [Troubleshooting](./troubleshooting.md) - Common issues and fixes
- [Getting Started](./getting-started.md) - Quick start guide
- [CLI Overview](./cli/overview.md) - Command line reference
