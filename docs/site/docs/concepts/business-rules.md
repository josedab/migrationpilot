# Business Rules

Business rules are the core logic extracted from legacy code that define how the system should behave. MigrationPilot's AI agents identify, document, and preserve these rules during migration.

## What Are Business Rules?

Business rules are the encoded decisions, calculations, validations, and workflows that implement an organization's policies. In legacy systems, these rules are often:

- **Undocumented** - No written specification exists
- **Embedded** - Mixed with infrastructure code
- **Implicit** - Understood only by long-tenured employees
- **Critical** - Essential for correct business operations

## Rule Categories

MigrationPilot classifies business rules into six categories:

### Calculation Rules

Mathematical formulas and computations:

```
IF ACCT-TYPE = "SAVINGS"
   RATE = BASE-RATE + 0.5
ELSE
   RATE = BASE-RATE
END-IF
```

**Extracted as:**
```json
{
  "id": "BR-001",
  "category": "calculation",
  "name": "Savings Interest Rate Bonus",
  "formula": "rate = baseRate + (accountType === 'SAVINGS' ? 0.5 : 0)"
}
```

### Validation Rules

Input constraints and data integrity checks:

```
IF LOAN-AMOUNT > MAX-LOAN-LIMIT
   SET ERROR-FLAG TO TRUE
END-IF
```

### Decision Rules

Business decisions with multiple branches:

```
EVALUATE TRUE
   WHEN CREDIT-SCORE >= 750  SET TIER TO "PRIME"
   WHEN CREDIT-SCORE >= 650  SET TIER TO "STANDARD"
   WHEN OTHER                SET TIER TO "SUBPRIME"
END-EVALUATE
```

### Transformation Rules

Data format conversions and mappings:

```
MOVE FUNCTION UPPER-CASE(CUSTOMER-NAME) TO OUTPUT-NAME
```

### Workflow Rules

Process sequences and state transitions:

```
PERFORM VALIDATE-ORDER
PERFORM CALCULATE-TOTAL
PERFORM APPLY-DISCOUNT
PERFORM FINALIZE-ORDER
```

### Constraint Rules

Business limits and boundaries:

```
01 MAX-WITHDRAWAL    PIC 9(6) VALUE 10000.
01 MIN-BALANCE       PIC 9(6) VALUE 100.
```

## Rule Extraction Process

### 1. Static Analysis

The parser identifies code patterns that indicate business logic:

- Conditional statements (IF, EVALUATE, CASE)
- Calculations (COMPUTE, arithmetic operations)
- Data movements with transformations
- Loop structures with business conditions

### 2. AI Analysis

The Archeologist agent uses AI to:

- Understand the intent behind code patterns
- Generate human-readable descriptions
- Identify inputs and outputs
- Calculate confidence scores

### 3. Confidence Scoring

Each rule receives a confidence score (0-100%):

| Score | Meaning |
|-------|---------|
| 90-100% | High confidence - AI is certain of interpretation |
| 70-89% | Medium confidence - Likely correct, review recommended |
| 50-69% | Low confidence - Requires human verification |
| Below 50% | Very low - Manual review required |

## Rule Schema

```typescript
interface BusinessRule {
  id: string;
  name: string;
  description: string;
  category: 'calculation' | 'validation' | 'decision' | 
            'transformation' | 'workflow' | 'constraint';
  
  // Source location
  sourceFile: string;
  sourceStartLine: number;
  sourceEndLine: number;
  sourceCode: string;
  
  // Logic specification
  inputs: RuleInput[];
  outputs: RuleOutput[];
  logic: string;          // Human-readable logic
  formula?: string;       // Mathematical formula if applicable
  
  // Edge cases and assumptions
  edgeCases: string[];
  assumptions: string[];
  
  // Confidence and review
  confidence: number;
  reviewStatus: 'pending' | 'approved' | 'rejected' | 'needs_clarification';
  reviewedBy?: string;
  reviewComments?: string;
}
```

## SME Review Process

Subject Matter Experts (SMEs) review extracted rules:

### 1. Review Queue

Low-confidence rules are flagged for review:

```bash
migrationpilot rules list --project my-project --pending
```

### 2. Rule Details

SMEs examine the extracted rule:

```bash
migrationpilot rules show BR-001
```

### 3. Actions

- **Approve** - Rule is correct as extracted
- **Reject** - Rule is incorrect, needs re-extraction
- **Clarify** - Add comments or corrections

### 4. Comments

SMEs can add domain knowledge:

```bash
migrationpilot rules approve BR-001 \
  --comment "Verified: 0.5% bonus applies to all savings accounts"
```

## Rule Versioning

All rule changes are tracked:

```sql
SELECT * FROM rule_versions 
WHERE rule_id = 'BR-001' 
ORDER BY version DESC;
```

| Version | Change | Changed By | Date |
|---------|--------|------------|------|
| 3 | Approved after SME review | jane.doe | 2024-01-28 |
| 2 | Updated formula precision | ai-agent | 2024-01-27 |
| 1 | Initial extraction | ai-agent | 2024-01-26 |

## Best Practices

### 1. Review High-Impact Rules First

Focus on rules that affect:
- Financial calculations
- Regulatory compliance
- Customer-facing features

### 2. Involve Domain Experts

The people who understand the business should review rules, not just developers.

### 3. Document Assumptions

When approving rules, document any assumptions:

```
"Assumes fiscal year starts January 1. 
Different for UK subsidiary (April 1)."
```

### 4. Test Edge Cases

Ensure extracted rules handle:
- Boundary values
- Null/empty inputs
- Unusual but valid combinations

## Related Topics

- [AI Agents](./ai-agents.md) - How agents extract rules
- [Equivalence Testing](./equivalence-testing.md) - Validating rule preservation
- [CLI Rules Command](../cli/rules.md) - Managing rules via CLI
