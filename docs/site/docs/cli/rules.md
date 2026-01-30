# rules

Manage extracted business rules from legacy code analysis.

## Usage

```bash
migrationpilot rules <command> [options]
```

## Subcommands

### list

List business rules.

```bash
migrationpilot rules list [options]
```

| Option | Description | Default |
|--------|-------------|---------|
| `-p, --project <name>` | Project name | - |
| `-t, --type <type>` | Filter by type | - |
| `-c, --confidence <min>` | Minimum confidence score | `0` |
| `--pending` | Show only rules pending review | `false` |

**Rule Types:**
- `calculation` - Mathematical formulas and computations
- `validation` - Input validation and constraints
- `decision` - Business decisions and branching logic
- `temporal` - Date/time-based rules

**Example:**

```bash
# List all rules
migrationpilot rules list --project banking-system

# List high-confidence calculation rules
migrationpilot rules list --project banking-system --type calculation --confidence 90

# List rules needing review
migrationpilot rules list --project banking-system --pending
```

**Output:**

```
📋 Business Rules
──────────────────────────────────────────────────────────────────────
┌────────┬──────────────────────────┬─────────────┬────────────┬──────────────┬────────────────┐
│ ID     │ Name                     │ Type        │ Confidence │ Status       │ Source         │
├────────┼──────────────────────────┼─────────────┼────────────┼──────────────┼────────────────┤
│ BR-001 │ Interest Rate Calc       │ calculation │ 95%        │ approved     │ CALCINT.cbl:245│
│ BR-002 │ Account Balance Valid    │ validation  │ 88%        │ pending      │ ACCTVAL.cbl:120│
│ BR-003 │ Loan Eligibility         │ decision    │ 92%        │ approved     │ LOANELIG.cbl:45│
│ BR-004 │ Fiscal Year End          │ temporal    │ 75%        │ needs-review │ FYEND.cbl:80   │
└────────┴──────────────────────────┴─────────────┴────────────┴──────────────┴────────────────┘
  Showing 4 of 4 rules
```

---

### show

Show detailed rule information.

```bash
migrationpilot rules show <id>
```

**Example:**

```bash
migrationpilot rules show BR-001
```

**Output:**

```
📋 Business Rule: BR-001
──────────────────────────────────────────────────
  Name: Interest Rate Calculation
  Type: calculation
  Confidence: 95%
  Status: approved

  Description
  Calculates the interest rate based on account type,
  balance tier, and customer loyalty status.

  Source Reference
  File: CALCINT.cbl
  Lines: 245-280

  Formula
  IF ACCT-TYPE = "SAVINGS"
    IF BALANCE > 10000
      RATE = BASE-RATE + 0.5
    ELSE
      RATE = BASE-RATE
  END-IF

  Generated Code (Java)
  public BigDecimal calculateInterestRate(Account account) {
    BigDecimal rate = baseRate;
    if ("SAVINGS".equals(account.getType())) {
      if (account.getBalance().compareTo(BigDecimal.valueOf(10000)) > 0) {
        rate = rate.add(BigDecimal.valueOf(0.5));
      }
    }
    return rate;
  }
```

---

### approve

Approve a business rule after review.

```bash
migrationpilot rules approve <id> [options]
```

| Option | Description |
|--------|-------------|
| `-c, --comment <text>` | Add a comment |

**Example:**

```bash
migrationpilot rules approve BR-001 --comment "Verified by finance team"
```

---

### reject

Reject a business rule.

```bash
migrationpilot rules reject <id> [options]
```

| Option | Description | Default |
|--------|-------------|---------|
| `-r, --reason <text>` | Rejection reason | `Requires manual review` |

**Example:**

```bash
migrationpilot rules reject BR-004 --reason "Logic needs clarification from SME"
```

---

### export

Export rules to a file.

```bash
migrationpilot rules export [options]
```

| Option | Description | Default |
|--------|-------------|---------|
| `-p, --project <name>` | Project name | - |
| `-f, --format <format>` | Export format (json, yaml, csv, pdf) | `json` |
| `-o, --output <file>` | Output file | `rules-export.<format>` |

**Example:**

```bash
# Export as JSON
migrationpilot rules export --project banking-system -o rules.json

# Export as CSV for spreadsheet review
migrationpilot rules export --project banking-system --format csv -o rules.csv
```

---

### import

Import rules from a file.

```bash
migrationpilot rules import <file> [options]
```

| Option | Description |
|--------|-------------|
| `-p, --project <name>` | Project name |
| `--merge` | Merge with existing rules |

**Example:**

```bash
migrationpilot rules import reviewed-rules.json --project banking-system --merge
```

## Rule Status

| Status | Description |
|--------|-------------|
| `pending` | Awaiting review |
| `approved` | Verified correct by SME |
| `rejected` | Marked incorrect, needs correction |
| `needs-review` | Low confidence, requires human verification |

## Confidence Scores

| Score | Interpretation |
|-------|----------------|
| ≥ 90% | High confidence - AI is confident in extraction |
| 70-89% | Medium confidence - Should be reviewed |
| < 70% | Low confidence - Requires human verification |
