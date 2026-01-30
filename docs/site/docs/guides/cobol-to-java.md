# COBOL to Java Migration Guide

This guide walks you through migrating a COBOL banking application to modern Java with Spring Boot.

## Prerequisites

- MigrationPilot installed and configured
- Java 17+ and Maven/Gradle
- Access to COBOL source files
- Basic understanding of the legacy system

## Sample COBOL Application

We'll migrate a loan calculation program:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANCALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL        PIC 9(10)V99.
       01 WS-RATE             PIC 9(3)V9(4).
       01 WS-TERM-MONTHS      PIC 9(3).
       01 WS-MONTHLY-PAYMENT  PIC 9(10)V99.
       01 WS-TOTAL-INTEREST   PIC 9(10)V99.
       
       PROCEDURE DIVISION.
           PERFORM CALCULATE-PAYMENT.
           STOP RUN.
           
       CALCULATE-PAYMENT.
           COMPUTE WS-MONTHLY-PAYMENT = 
               WS-PRINCIPAL * (WS-RATE / 12) /
               (1 - (1 + WS-RATE / 12) ** (-WS-TERM-MONTHS)).
           COMPUTE WS-TOTAL-INTEREST = 
               (WS-MONTHLY-PAYMENT * WS-TERM-MONTHS) - WS-PRINCIPAL.
```

## Step 1: Create a Project

### Using the CLI

```bash
migrationpilot project create loan-modernization \
  --language cobol \
  --target java \
  --description "Loan calculation system modernization"
```

### Using the Web Dashboard

1. Navigate to **Dashboard** → **New Project**
2. Enter project name: "Loan Modernization"
3. Select **COBOL** as source language
4. Select **Java** as target language
5. Choose **Spring Boot** as the framework

## Step 2: Upload Source Code

### CLI Upload

```bash
migrationpilot analyze ./cobol-src \
  --language cobol \
  --project loan-modernization
```

### API Upload

```bash
curl -X POST http://localhost:4000/api/projects/PROJECT_ID/files \
  -H "Content-Type: application/json" \
  -d '[
    {"filename": "LOANCALC.cbl", "content": "...cobol content..."},
    {"filename": "LOANVAL.cbl", "content": "...cobol content..."}
  ]'
```

## Step 3: Run Analysis

The Archeologist agent analyzes your code and extracts business rules:

```bash
migrationpilot migrate --project loan-modernization --dry-run
```

### Expected Output

```
📊 Analysis Summary
──────────────────────────────────────────────────
  Files Analyzed:     12
  Data Structures:    45
  Procedures:         78
  Business Rules:     23
  Complexity Score:   Medium

📋 Business Rules Extracted
──────────────────────────────────────────────────
  BR-001: Monthly Payment Calculation (95% confidence)
  BR-002: Interest Rate Validation (92% confidence)
  BR-003: Term Limit Constraint (88% confidence)
```

## Step 4: Review Business Rules

Review extracted rules for accuracy:

```bash
# List rules needing review
migrationpilot rules list --project loan-modernization --pending

# View rule details
migrationpilot rules show BR-001

# Approve a rule
migrationpilot rules approve BR-001 --comment "Verified by finance team"
```

### Example Rule Detail

```
📋 Business Rule: BR-001
──────────────────────────────────────────────────
  Name: Monthly Payment Calculation
  Type: calculation
  Confidence: 95%
  
  Formula:
    payment = principal * (rate/12) / (1 - (1 + rate/12)^(-term))
  
  Inputs:
    - principal: decimal (loan amount)
    - rate: decimal (annual interest rate)
    - term: integer (months)
  
  Output:
    - monthlyPayment: decimal
```

## Step 5: Run Migration

Execute the full migration:

```bash
migrationpilot migrate \
  --project loan-modernization \
  --output ./generated-java
```

### Generated Project Structure

```
generated-java/
├── pom.xml
├── src/
│   ├── main/java/com/example/loan/
│   │   ├── LoanApplication.java
│   │   ├── controller/
│   │   │   └── LoanController.java
│   │   ├── service/
│   │   │   └── LoanCalculationService.java
│   │   ├── model/
│   │   │   ├── LoanRequest.java
│   │   │   └── LoanResponse.java
│   │   └── validation/
│   │       └── LoanValidator.java
│   └── test/java/com/example/loan/
│       └── service/
│           └── LoanCalculationServiceTest.java
└── docs/
    └── business-rules.md
```

## Step 6: Review Generated Code

### Service Implementation

```java
@Service
public class LoanCalculationService {

    /**
     * Calculate monthly payment for a loan.
     * 
     * Migrated from: LOANCALC.cbl (lines 15-22)
     * Business Rule: BR-001
     */
    public BigDecimal calculateMonthlyPayment(
            BigDecimal principal,
            BigDecimal annualRate,
            int termMonths) {
        
        BigDecimal monthlyRate = annualRate.divide(
            BigDecimal.valueOf(12), 10, RoundingMode.HALF_UP);
        
        // payment = principal * (rate/12) / (1 - (1 + rate/12)^(-term))
        BigDecimal numerator = principal.multiply(monthlyRate);
        BigDecimal denominator = BigDecimal.ONE.subtract(
            BigDecimal.ONE.add(monthlyRate)
                .pow(-termMonths, MathContext.DECIMAL128));
        
        return numerator.divide(denominator, 2, RoundingMode.HALF_UP);
    }
    
    /**
     * Calculate total interest over loan term.
     * 
     * Migrated from: LOANCALC.cbl (lines 23-24)
     * Business Rule: BR-002
     */
    public BigDecimal calculateTotalInterest(
            BigDecimal principal,
            BigDecimal monthlyPayment,
            int termMonths) {
        
        return monthlyPayment
            .multiply(BigDecimal.valueOf(termMonths))
            .subtract(principal);
    }
}
```

### REST Controller

```java
@RestController
@RequestMapping("/api/loans")
public class LoanController {

    private final LoanCalculationService calculationService;
    
    @PostMapping("/calculate")
    public LoanResponse calculateLoan(@Valid @RequestBody LoanRequest request) {
        BigDecimal monthlyPayment = calculationService.calculateMonthlyPayment(
            request.getPrincipal(),
            request.getAnnualRate(),
            request.getTermMonths());
            
        BigDecimal totalInterest = calculationService.calculateTotalInterest(
            request.getPrincipal(),
            monthlyPayment,
            request.getTermMonths());
        
        return new LoanResponse(monthlyPayment, totalInterest);
    }
}
```

## Step 7: Run Validation

Validate the migrated code behaves identically:

```bash
migrationpilot validate --project loan-modernization --verbose
```

### Validation Results

```
📊 Validation Results
──────────────────────────────────────────────────
  Test Cases:        1,247
  Passed:            1,241
  Failed:            6
  Confidence:        99.5%

❌ Failed Tests (review required)
──────────────────────────────────────────────────
  - boundary_max_principal: Rounding difference (0.01)
  - boundary_min_rate: Division precision differs
```

## Step 8: Deploy

Once validation passes, deploy your modern application:

```bash
# Build
cd generated-java
mvn clean package

# Run
java -jar target/loan-service-1.0.0.jar

# Test endpoint
curl -X POST http://localhost:8080/api/loans/calculate \
  -H "Content-Type: application/json" \
  -d '{"principal": 100000, "annualRate": 0.05, "termMonths": 360}'
```

## Data Type Mapping

| COBOL | Java |
|-------|------|
| `PIC 9(n)` | `int` / `long` |
| `PIC 9(n)V9(m)` | `BigDecimal` |
| `PIC X(n)` | `String` |
| `PIC S9(n)` | `int` (signed) |
| `COMP` | `int` |
| `COMP-3` | `BigDecimal` |

## Common Patterns

### COBOL PERFORM → Java Method

```cobol
PERFORM CALCULATE-INTEREST.
```

```java
calculateInterest();
```

### COBOL EVALUATE → Java Switch

```cobol
EVALUATE ACCOUNT-TYPE
    WHEN "SAVINGS"  PERFORM SAVINGS-LOGIC
    WHEN "CHECKING" PERFORM CHECKING-LOGIC
    WHEN OTHER      PERFORM DEFAULT-LOGIC
END-EVALUATE.
```

```java
switch (accountType) {
    case "SAVINGS" -> savingsLogic();
    case "CHECKING" -> checkingLogic();
    default -> defaultLogic();
}
```

## Troubleshooting

### Low Confidence Rules

If rules have confidence below 80%, they may need manual review:

```bash
migrationpilot rules list --project loan-modernization --confidence 80
```

### Validation Failures

Check the validation report for specific failing tests:

```bash
migrationpilot validate --project loan-modernization --report validation.json
```

## Next Steps

- [Review the generated documentation](./generated-java/docs/business-rules.md)
- [Set up CI/CD pipeline](../deployment/cloud.md)
- [Configure monitoring](../deployment/kubernetes.md)
