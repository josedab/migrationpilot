---
sidebar_position: 3
---

# Your First Migration

Walk through a complete migration from COBOL to Java.

## Prerequisites

- MigrationPilot installed and running
- Sample COBOL code to migrate

## Step 1: Create a Project

Using the web dashboard or CLI:

```bash
migrationpilot project create banking-migration \
  --language cobol \
  --target java \
  --description "Migrate banking core system"
```

## Step 2: Upload Source Code

Upload your COBOL files through the dashboard or API:

```bash
# Using CLI
migrationpilot analyze ./cobol-src --project banking-migration
```

## Step 3: Review Analysis

The Archeologist agent will analyze your code and extract:
- Data structures
- Business rules
- Dependencies
- Complexity metrics

Review the extracted business rules in the dashboard.

## Step 4: Approve Business Rules

SMEs should review and approve/correct business rules:

1. Navigate to **Projects → banking-migration → Rules**
2. Review each rule's description and formula
3. Add comments or corrections as needed
4. Approve rules when accurate

## Step 5: Start Migration

Once rules are approved:

```bash
migrationpilot migrate --project banking-migration
```

Monitor progress in the dashboard or stream via CLI.

## Step 6: Validate Equivalence

After code generation:

```bash
migrationpilot validate --project banking-migration
```

Review the confidence score and any failing tests.

## Step 7: Review Generated Code

Explore the generated Java code:
- Source files in `/generated/src`
- Tests in `/generated/test`
- Documentation in `/generated/docs`

## Next Steps

- [COBOL to Java guide](/docs/guides/cobol-to-java) for detailed patterns
- [Strangler Fig pattern](/docs/guides/strangler-fig-pattern) for incremental migration
