---
sidebar_position: 2
---

# Quickstart

Get up and running with MigrationPilot in 5 minutes.

## Overview

In this quickstart, you'll:
1. Analyze a sample COBOL program
2. Review extracted business rules
3. Generate modern Java code
4. Validate the migration

## Step 1: Install the CLI

```bash
npm install -g @migrationpilot/cli
```

## Step 2: Set Up Configuration

```bash
migrationpilot config init
```

Follow the prompts to configure:
- API URL (use `http://localhost:3001` for local development)
- Default source language
- Default target language

## Step 3: Analyze Legacy Code

Let's analyze a sample COBOL program:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCINT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL    PIC 9(10)V99.
       01 WS-RATE         PIC 9(2)V9(4).
       01 WS-YEARS        PIC 9(2).
       01 WS-INTEREST     PIC 9(10)V99.
       
       PROCEDURE DIVISION.
           COMPUTE WS-INTEREST = 
               WS-PRINCIPAL * WS-RATE * WS-YEARS.
           STOP RUN.
```

Save this as `sample.cbl` and run:

```bash
migrationpilot analyze ./sample.cbl --language cobol
```

Output:

```
📊 Analysis Summary
──────────────────────────────────────────────────
┌─────────────────┬───────┐
│ Metric          │ Count │
├─────────────────┼───────┤
│ Files           │ 1     │
│ Data Structures │ 4     │
│ Procedures      │ 1     │
│ Errors          │ 0     │
│ Warnings        │ 0     │
└─────────────────┴───────┘
```

## Step 4: Run Full Migration

Now let's run the full AI-powered migration:

```bash
migrationpilot migrate --source ./sample.cbl --target java
```

Follow the interactive wizard:

```
🚀 MigrationPilot Migration
──────────────────────────────────────────────────

? Project name: interest-calculator
? Source language: COBOL
? Target language: Java (Spring Boot)
? Migration pattern: Microservices
? Output directory: ./migrated
? Enable interactive rule review? Yes
```

## Step 5: Review Extracted Rules

The migration process extracts business rules for review:

```
📋 Business Rules Detected
──────────────────────────────────────────────────

BR-001: Interest Calculation
  Type: calculation
  Confidence: 95%
  Formula: INTEREST = PRINCIPAL * RATE * YEARS

Do you want to approve this rule? (y/n)
```

Approve or modify the rules as needed.

## Step 6: View Generated Code

After approval, view the generated Java code:

```java
package com.example.interest;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Interest Calculator
 * Migrated from CALCINT.cbl
 */
public class InterestCalculator {
    
    /**
     * Calculate simple interest.
     * Business Rule: BR-001
     * 
     * @param principal The principal amount
     * @param rate Annual interest rate (as decimal)
     * @param years Number of years
     * @return Calculated interest
     */
    public BigDecimal calculateInterest(
            BigDecimal principal,
            BigDecimal rate,
            int years) {
        
        return principal
            .multiply(rate)
            .multiply(BigDecimal.valueOf(years))
            .setScale(2, RoundingMode.HALF_UP);
    }
}
```

## Step 7: Validate Equivalence

Ensure the new code behaves like the original:

```bash
migrationpilot validate --project interest-calculator
```

Output:

```
🔍 Equivalence Validation
──────────────────────────────────────────────────

📊 Validation Results
┌───────────────────────┬────────┬────────┬──────────┐
│ Category              │ Passed │ Failed │ Coverage │
├───────────────────────┼────────┼────────┼──────────┤
│ Boundary Values       │ 45     │ 0      │ 100%     │
│ Equivalence Classes   │ 52     │ 0      │ 100%     │
│ Random Inputs         │ 48     │ 0      │ 100%     │
│ Edge Cases            │ 38     │ 0      │ 100%     │
├───────────────────────┼────────┼────────┼──────────┤
│ Total                 │ 183    │ 0      │ 100%     │
└───────────────────────┴────────┴────────┴──────────┘

🎯 Confidence Score: 100%
```

## What's Next?

You've completed your first migration! Here are some next steps:

- **Learn the concepts**: [How it works](/docs/concepts/how-it-works)
- **Try a larger migration**: [COBOL to Java guide](/docs/guides/cobol-to-java)
- **Use the web dashboard**: Open http://localhost:3000
- **Integrate with CI/CD**: [API Reference](/docs/api/overview)

## Common Issues

### API Connection Error

Make sure the API server is running:
```bash
pnpm dev
# or
docker compose up -d
```

### GitHub Token Not Set

Set your GitHub token for Copilot SDK access:
```bash
export GITHUB_TOKEN=your_token_here
```

### Memory Issues

For large COBOL programs, increase Node.js memory:
```bash
export NODE_OPTIONS="--max-old-space-size=8192"
```
